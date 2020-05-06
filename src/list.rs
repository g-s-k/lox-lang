use std::{
    mem,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub(crate) struct List<T> {
    next: Option<Box<List<T>>>,
    item: Box<T>,
}

impl<T> List<T> {
    pub(crate) fn push(list: &mut Option<Box<Self>>, value: T) -> *mut T {
        let old_head = list.take();
        let new_head = Self {
            next: old_head,
            item: Box::new(value),
        };
        &mut *list.get_or_insert(Box::new(new_head)).item
    }

    pub(crate) fn pop(list: &mut Option<Box<Self>>) -> Option<Box<T>> {
        if let Some(node) = list.take() {
            mem::replace(list, node.next);
            Some(node.item)
        } else {
            None
        }
    }

    pub(crate) fn insert_before<F>(list: &mut Option<Box<Self>>, value: T, predicate: F) -> *mut T
    where
        F: Fn(&T) -> bool,
    {
        match list {
            None => Self::push(list, value),
            Some(ref head) if predicate(&head.item) => Self::push(list, value),
            Some(ref mut head) => match &head.next {
                None => Self::push(&mut head.next, value),
                Some(tail_first) if predicate(&tail_first.item) => {
                    Self::push(&mut head.next, value)
                }
                Some(_) => Self::insert_before(&mut head.next, value, predicate),
            },
        }
    }

    pub(crate) fn get<F>(list: &Option<Box<Self>>, predicate: F) -> Option<*const T>
    where
        F: Fn(&T) -> bool,
    {
        match list {
            Some(head) => {
                if predicate(&head.item) {
                    Some(&*head.item)
                } else {
                    Self::get(&head.next, predicate)
                }
            }
            None => None,
        }
    }

    pub(crate) fn get_mut<F>(list: &mut Option<Box<Self>>, predicate: F) -> Option<*mut T>
    where
        F: Fn(&T) -> bool,
    {
        match list {
            Some(ref mut head) => {
                if predicate(&head.item) {
                    Some(&mut *head.item)
                } else {
                    Self::get_mut(&mut head.next, predicate)
                }
            }
            None => None,
        }
    }
}

impl<T> AsRef<T> for List<T> {
    fn as_ref(&self) -> &T {
        &self.item
    }
}

impl<T> Deref for List<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> DerefMut for List<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}
