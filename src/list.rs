//! general purpose linked list
//!
//! mostly cribbed from the immortal [Rust in Entirely Too Many Linked
//! Lists](https://rust-unofficial.github.io/too-many-lists) but improved with reference passing
//! and utility methods to get, insert, and delete conditionally

#[derive(Debug)]
pub(crate) struct List<T> {
    head: Option<Box<Node<T>>>,
}

#[derive(Debug)]
struct Node<T> {
    item: T,
    next: Option<Box<Node<T>>>,
}

impl<T> List<T> {
    pub(crate) fn new() -> Self {
        Self { head: None }
    }

    #[allow(dead_code)]
    pub(crate) fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.item)
    }

    #[allow(dead_code)]
    pub(crate) fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.item)
    }

    pub(crate) fn push(&mut self, item: T) -> &mut T {
        Node::splice_in(&mut self.head, item)
    }

    #[allow(dead_code)]
    pub(crate) fn pop(&mut self) -> Option<T> {
        self.head.take().map(|node| node.item)
    }

    pub(crate) fn insert_before<F>(&mut self, value: T, predicate: F) -> &mut T
    where
        F: Fn(&T) -> bool,
    {
        let mut current = &mut self.head;

        while current
            .as_ref()
            .filter(|node| predicate(&node.item))
            .is_some()
        {
            current = &mut current.as_mut().unwrap().next;
        }

        Node::splice_in(current, value)
    }

    pub(crate) fn find<F>(&self, predicate: F) -> Option<&T>
    where
        F: Fn(&T) -> bool,
    {
        let mut current = &self.head;

        while let Some(ref head) = &current {
            if predicate(&head.item) {
                break;
            }

            current = &head.next;
        }

        current.as_ref().map(|node| &node.item)
    }

    pub(crate) fn retain<F>(&mut self, predicate: F) -> Vec<T>
    where
        F: Fn(&T) -> bool,
    {
        let mut out = Vec::new();

        let mut current = &mut self.head;

        while current.is_some() {
            if current
                .as_ref()
                .filter(|node| predicate(&node.item))
                .is_some()
            {
                current = &mut current.as_mut().unwrap().next;
            } else if let Some(to_remove) = current.take() {
                *current = to_remove.next;
                out.push(to_remove.item);
            }
        }

        out
    }

    pub(crate) fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_ref().map(|node| &**node),
        }
    }
}

pub(crate) struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_ref().map(|node| &**node);
            &node.item
        })
    }
}

impl<T> Node<T> {
    fn splice_in(maybe_node: &mut Option<Box<Self>>, item: T) -> &mut T {
        let new_node = Box::new(Node {
            item,
            next: maybe_node.take().and_then(|mut node| node.next.take()),
        });

        &mut maybe_node.get_or_insert(new_node).item
    }
}
