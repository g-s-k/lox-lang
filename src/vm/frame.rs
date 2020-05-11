use std::any::Any;

use crate::{Chunk, Fun, Gc, UpvalueRef};

#[derive(Clone, Debug)]
pub(crate) struct CallFrame {
    pub(crate) func: Gc<Fun>,
    pub(crate) inst: usize,
    pub(crate) base: usize,
    pub(crate) upvalues: Box<[Gc<UpvalueRef>]>,
}

impl CallFrame {
    pub(crate) fn chunk(&self) -> &Chunk {
        &(*self.func).chunk
    }

    pub(crate) fn get_upvalue(&self, index: usize) -> Option<Gc<UpvalueRef>> {
        self.upvalues.get(index).cloned()
    }

    pub(crate) fn mark(&self, grays: &mut Vec<Gc<dyn Any>>) {
        self.func.mark();
        grays.push(self.func.as_any());

        for u_val in self.upvalues.iter() {
            u_val.mark();
            grays.push(u_val.as_any());
        }
    }
}
