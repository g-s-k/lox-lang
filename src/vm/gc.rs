use std::{any::Any, mem};

use crate::Gc;

impl super::VM {
    fn mark_roots(&mut self) -> Vec<Gc<dyn Any>> {
        let mut gray_stack = Vec::new();

        // locals and temporaries
        for el in &self.stack {
            el.mark(&mut gray_stack);
        }

        // globals
        for val in self.globals.values() {
            val.mark(&mut gray_stack);
        }

        // call frames
        for frame in &self.frames {
            frame.mark(&mut gray_stack);
        }

        // open upvalues
        for c in self.open_upvalues.iter() {
            c.mark();
            gray_stack.push(c.as_any());
        }

        // compiler roots
        for root in &self.compiler_roots {
            root.mark(&mut gray_stack);
        }

        gray_stack
    }

    fn trace_references(gray_stack: &mut Vec<Gc<dyn Any>>) {
        while let Some(top_obj) = gray_stack.pop() {
            top_obj.blacken(gray_stack);
        }
    }

    fn sweep(&mut self) {
        let to_drop = self.objects.retain(|obj| {
            if obj.is_marked() {
                obj.clear_mark();
                true
            } else {
                false
            }
        });

        for ptr in to_drop {
            self.total_allocations -= mem::size_of_val(&*ptr);
            ptr.free();
        }
    }

    fn collect_garbage(&mut self) {
        #[cfg(feature = "trace-gc")]
        let before = self.total_allocations;

        #[cfg(feature = "trace-gc")]
        log::debug!("gc begin :: total allocations {} bytes", before);

        let mut gray_stack = self.mark_roots();
        Self::trace_references(&mut gray_stack);
        self.sweep();

        // adjust threshold
        self.next_gc = self.total_allocations * 2;

        #[cfg(feature = "trace-gc")]
        log::debug!(
            "gc end   :: collected {} bytes (total was {}, now {}) :: next at {}",
            before - self.total_allocations,
            before,
            self.total_allocations,
            self.next_gc
        );
    }

    fn should_collect(&self) -> bool {
        cfg!(feature = "stress-test-gc") || self.total_allocations > self.next_gc
    }

    /// Allocate a garbage-collected value on the heap.
    ///
    /// This method is how to obtain a `Gc` pointer (not exported from this crate and has no public
    /// constructor). Values allocated with this method will be owned (and eventually freed) by the
    /// VM. If the value lives until the VM goes out of scope, it will be freed in the VM's `Drop`
    /// implementation.
    ///
    /// For a usage example, see [`NativeFun`](./type.NativeFun.html).
    pub fn alloc<T: Any>(&mut self, obj: T) -> Gc<T> {
        if self.should_collect() {
            self.collect_garbage();
        }

        let size = mem::size_of::<T>();
        self.total_allocations += size;

        let ptr = Gc::new(obj);
        self.objects.push(ptr.as_any());

        #[cfg(feature = "trace-gc")]
        log::debug!(
            "{:p} allocate {} bytes for {}",
            ptr,
            size,
            std::any::type_name::<T>()
        );

        ptr
    }
}
