use crate::Fun;

#[derive(Clone, Copy)]
pub(super) enum FunType {
    Script,
    Function,
    Method,
    Initializer,
}

#[derive(Debug)]
pub(super) struct Local<'compile> {
    pub(super) name: &'compile str,
    pub(super) depth: usize,
    pub(super) is_defined: bool,
    pub(super) is_captured: bool,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Upvalue {
    pub index: usize,
    pub is_local: bool,
}

pub(super) struct FunWrapper<'compile> {
    pub(super) enclosing: Option<Box<Self>>,
    pub(super) inner: Fun,
    pub(super) r#type: FunType,
    pub(super) locals: Vec<Local<'compile>>,
    pub(super) upvalues: Vec<Upvalue>,
    pub(super) scope_depth: usize,
}

impl<'compile> FunWrapper<'compile> {
    pub(super) fn new<T: ToString>(name: &T, r#type: FunType) -> Self {
        Self {
            enclosing: None,
            inner: Fun::new(name, 0),
            r#type,
            locals: vec![Local {
                name: if let FunType::Method | FunType::Initializer = r#type {
                    "this"
                } else {
                    ""
                },
                depth: 0,
                is_defined: true,
                is_captured: false,
            }],
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }

    pub(super) fn resolve_local(&self, query: &str) -> Option<(usize, bool)> {
        for (
            index,
            Local {
                name, is_defined, ..
            },
        ) in self.locals.iter().enumerate().rev()
        {
            if *name == query {
                return Some((index, *is_defined));
            }
        }

        None
    }

    pub(super) fn resolve_upvalue(&mut self, query: &str) -> Option<usize> {
        if let Some(parent) = &mut self.enclosing {
            if let Some((index, _)) = parent.resolve_local(query) {
                parent.locals[index].is_captured = true;
                return Some(self.add_upvalue(index, true));
            } else if let Some(index) = parent.resolve_upvalue(query) {
                return Some(self.add_upvalue(index, false));
            }
        }

        None
    }

    pub(super) fn add_upvalue(&mut self, local_index: usize, is_local: bool) -> usize {
        for (idx, u_val) in self.upvalues.iter().enumerate() {
            if u_val.index == local_index && u_val.is_local == is_local {
                return idx;
            }
        }

        self.upvalues.push(Upvalue {
            index: local_index,
            is_local,
        });
        self.upvalues.len() - 1
    }
}

#[derive(Debug)]
pub(super) struct ClassWrapper {
    pub(super) enclosing: Option<Box<Self>>,
    pub(super) has_superclass: bool,
}
