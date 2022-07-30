use std::fmt;

use {
    super::frame::CallFrame,
    crate::{Class, Fun, Gc, Instance, Op, RuntimeError, Upvalue, UpvalueRef, Value},
};

type RunResult = Result<(), RuntimeError>;

impl super::VM {
    pub(super) fn run(&mut self) -> RunResult {
        macro_rules! binary_op_body {
            ($op: tt, $variant: ident) => {
                if let (Value::Number(a), Value::Number(b)) = self.pop_pair()? {
                    self.stack.push(Value::$variant(a $op b));
                }
            };
        }

        macro_rules! binary_op {
            ($op: tt, $variant: ident) => {{
                match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => (),
                    _ => return Err(RuntimeError::ArgumentTypes),
                }

                // should be infallible now
                binary_op_body!($op, $variant)
            }};
        }

        while let Some(instruction) = self.step() {
            {
                #![cfg(feature = "trace-execution")]
                // print stack before operation
                log::debug!(
                    "          {}",
                    self.stack
                        .iter()
                        .map(|v| format!("[ {} ]", v))
                        .collect::<String>()
                );

                // print operation and arguments
                let frame = self.frame();
                log::debug!("{:1$x}", frame.chunk(), frame.inst);
            }

            self.frame_mut().inst += 1;

            match instruction {
                Op::Constant(index) => {
                    self.stack.push(self.read_constant(index).clone());
                }
                Op::ConstantLong(index) => {
                    self.stack.push(self.read_constant(index).clone());
                }
                Op::Nil => self.stack.push(Value::Nil),
                Op::True => self.stack.push(Value::Boolean(true)),
                Op::False => self.stack.push(Value::Boolean(false)),
                Op::Pop => {
                    self.stack.pop();
                }
                Op::GetGlobal(index) => self.fetch_global(index)?,
                Op::GetGlobalLong(index) => self.fetch_global(index)?,
                Op::DefineGlobal(index) => self.define_global_from_stack(index)?,
                Op::DefineGlobalLong(index) => self.define_global_from_stack(index)?,
                Op::SetGlobal(index) => self.set_global(index)?,
                Op::SetGlobalLong(index) => self.set_global(index)?,
                Op::GetLocal(index) => self.fetch_local(index)?,
                Op::GetLocalLong(index) => self.fetch_local(index)?,
                Op::SetLocal(index) => self.set_local(index)?,
                Op::SetLocalLong(index) => self.set_local(index)?,
                Op::GetUpvalue(index) => self.fetch_upvalue(index)?,
                Op::GetUpvalueLong(index) => self.fetch_upvalue(index)?,
                Op::SetUpvalue(index) => self.set_upvalue(index)?,
                Op::SetUpvalueLong(index) => self.set_upvalue(index)?,
                Op::GetProperty(index) => self.get_property(index)?,
                Op::GetPropertyLong(index) => self.get_property(index)?,
                Op::SetProperty(index) => self.set_property(index)?,
                Op::SetPropertyLong(index) => self.set_property(index)?,
                Op::GetSuper(index) => self.get_super(index)?,
                Op::GetSuperLong(index) => self.get_super(index)?,
                Op::Equal => {
                    let (a, b) = self.pop_pair()?;
                    self.stack.push(Value::Boolean(b == a));
                }
                Op::Greater => binary_op!(>, Boolean),
                Op::Less => binary_op!(<, Boolean),
                Op::Add => match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => {
                        binary_op_body!(+, Number);
                    }
                    (Value::r#String(_), Value::r#String(_)) => {
                        if let (Value::r#String(a), Value::r#String(b)) = self.pop_pair()? {
                            let mut a = a.into_string();
                            a.push_str(&b);
                            self.stack.push(Value::r#String(a.into_boxed_str()));
                        }
                    }
                    _ => return Err(RuntimeError::ArgumentTypes),
                },
                Op::Subtract => binary_op!(-, Number),
                Op::Multiply => binary_op!(*, Number),
                Op::Divide => binary_op!(/, Number),
                Op::Not => {
                    if let Some(val) = self.stack.pop() {
                        self.stack.push(Value::Boolean(val.is_falsey()));
                    }
                }
                Op::Negate => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(value.negate()?);
                    }
                }
                Op::Print => {
                    if let Some(val) = self.stack.pop() {
                        self.print(format_args!("{}\n", val));
                    }
                }
                Op::JumpIfFalse(distance) => {
                    if self.peek(0)?.is_falsey() {
                        self.frame_mut().inst += distance as usize;
                    }
                }
                Op::Jump(distance) => self.frame_mut().inst += distance as usize,
                Op::Loop(distance) => self.frame_mut().inst -= distance as usize,
                Op::Call(arg_count) => {
                    self.call_value_from_stack(arg_count)?;
                }
                Op::Invoke(index, arg_count) => {
                    let name = self.read_string(index).to_string();
                    self.invoke(name, arg_count)?;
                }
                Op::InvokeLong(index, arg_count) => {
                    let name = self.read_string(index).to_string();
                    self.invoke(name, arg_count)?;
                }
                Op::SuperInvoke(index, arg_count) => {
                    let name = self.read_string(index).to_string();
                    if let Value::Class(super_class) =
                        self.stack.pop().ok_or(RuntimeError::StackEmpty)?
                    {
                        self.invoke_from_class(super_class, &name, arg_count)?;
                    } else {
                        return Err(RuntimeError::ArgumentTypes);
                    }
                }
                Op::SuperInvokeLong(index, arg_count) => {
                    let name = self.read_string(index).to_string();
                    if let Value::Class(super_class) =
                        self.stack.pop().ok_or(RuntimeError::StackEmpty)?
                    {
                        self.invoke_from_class(super_class, &name, arg_count)?;
                    } else {
                        return Err(RuntimeError::ArgumentTypes);
                    }
                }
                Op::Closure(index, upvals) => self.create_closure(index, &upvals)?,
                Op::ClosureLong(index, upvals) => self.create_closure(index, &upvals)?,
                Op::CloseUpvalue => {
                    let idx = self
                        .stack
                        .len()
                        .checked_sub(1)
                        .ok_or(RuntimeError::StackEmpty)?;
                    self.close_upvalues(idx);
                    self.stack.pop();
                }
                Op::Return => {
                    let base_ptr = self.frame().base;
                    let result = self.stack.pop().ok_or(RuntimeError::StackEmpty)?;

                    self.close_upvalues(base_ptr);

                    self.frames.pop();
                    if self.frames.is_empty() {
                        self.stack.pop();
                        break;
                    }

                    self.stack.truncate(base_ptr as usize);
                    self.stack.push(result);
                }
                Op::Class(index) => {
                    let name = Class::new(self.read_constant(index));
                    let val = Value::Class(self.alloc(name));
                    self.stack.push(val);
                }
                Op::ClassLong(index) => {
                    let name = Class::new(self.read_constant(index));
                    let val = Value::Class(self.alloc(name));
                    self.stack.push(val);
                }
                Op::Inherit => {
                    if let (Value::Class(mut sub_class), Value::Class(super_class)) = (
                        self.stack.pop().ok_or(RuntimeError::StackEmpty)?,
                        self.peek(0)?.clone(),
                    ) {
                        for (name, value) in &super_class.methods {
                            sub_class.methods.insert(name.clone(), value.clone());
                        }
                    } else {
                        return Err(RuntimeError::ArgumentTypes);
                    }
                }
                Op::Method(index) => self.define_method(index)?,
                Op::MethodLong(index) => self.define_method(index)?,
            }
        }

        Ok(())
    }

    fn frame(&self) -> &CallFrame {
        if let Some(frame) = self.frames.last() {
            return frame;
        }

        unreachable!("Current call frame not found.")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        if let Some(frame) = self.frames.last_mut() {
            return frame;
        }

        unreachable!("Current call frame not found.")
    }

    fn step(&mut self) -> Option<Op> {
        let frame = self.frame();
        frame.chunk().code.get(frame.inst).cloned()
    }

    fn read_constant<T: Into<usize>>(&self, index: T) -> &Value {
        self.frame().chunk().read_constant(index.into())
    }

    fn read_string<T: Into<usize> + fmt::Display + Copy>(&self, index: T) -> &str {
        match self.read_constant(index) {
            Value::r#String(s) => s,
            c =>
            unreachable!(
                "Invariant violation: tried to load a string value from the constant table at index {}; found non-string value {}",
                index, c
            ),
        }
    }

    fn define_global_from_stack<T: Into<usize> + fmt::Display + Copy>(
        &mut self,
        index: T,
    ) -> RunResult {
        if let Some(value) = self.stack.pop() {
            let var_name = self.read_string(index).to_string().into_boxed_str();
            self.globals.insert(var_name, value);
            Ok(())
        } else {
            Err(RuntimeError::BadStackIndex(index.into(), self.stack.len()))
        }
    }

    fn fetch_global<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        let var_name = self.read_string(index);
        if let Some(value) = self.globals.get(var_name).cloned() {
            self.stack.push(value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedGlobal(var_name.to_string()))
        }
    }

    fn set_global<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        let var_str = self.read_string(index).to_string();
        if self.globals.contains_key(&*var_str) {
            let value = self.peek(0)?.clone();
            self.globals.insert(var_str.into_boxed_str(), value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedGlobal(var_str))
        }
    }

    fn fetch_local<T: Into<usize>>(&mut self, index: T) -> RunResult {
        let base_ptr = self.frame().base;

        let index = index.into();
        if let Some(val) = self.stack.get(index + base_ptr).cloned() {
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::BadStackIndex(index, self.stack.len()))
        }
    }

    fn set_local<T: Into<usize>>(&mut self, index: T) -> RunResult {
        let base_ptr = self.frame().base;
        let new_val = self.peek(0)?.clone();

        if let Some(el) = self.stack.get_mut(index.into() + base_ptr) {
            *el = new_val;
        }

        Ok(())
    }

    fn pop_pair(&mut self) -> Result<(Value, Value), RuntimeError> {
        if let Some(b) = self.stack.pop() {
            if let Some(a) = self.stack.pop() {
                return Ok((a, b));
            }
        }

        Err(RuntimeError::StackEmpty)
    }

    fn pop_many(&mut self, count: u16) -> RunResult {
        if let Some(new_len) = self.stack.len().checked_sub(count as usize) {
            self.stack.truncate(new_len);
            Ok(())
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn peek(&self, distance: usize) -> Result<&Value, RuntimeError> {
        if let Some(idx) = self.stack.len().checked_sub(distance + 1) {
            if let Some(val) = self.stack.get(idx) {
                Ok(val)
            } else {
                Err(RuntimeError::BadStackIndex(idx, self.stack.len()))
            }
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn create_closure<T: Into<usize>>(&mut self, index: T, upvals: &[Upvalue]) -> RunResult {
        if let Value::Fun(f) = self.read_constant(index).clone() {
            let mut upvalues = Vec::new();
            for Upvalue { index, is_local } in upvals {
                upvalues.push(if *is_local {
                    self.capture_upvalue(self.frame().base + index)
                } else {
                    self.frame().upvalues[*index]
                });
            }

            self.stack
                .push(Value::Closure(f, upvalues.into_boxed_slice()));
            Ok(())
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }

    // possible TODO: double iteration here. not sure if it's worth the custom code to do it in one
    // iteration, given that the list is almost always empty or very short
    fn capture_upvalue(&mut self, slot: usize) -> Gc<UpvalueRef> {
        if let Some(r) = self
            .open_upvalues
            .find(|u_val| matches!(&**u_val,  UpvalueRef::Live(s) if *s == slot))
        {
            *r
        } else {
            let allocated = self.alloc(UpvalueRef::new(slot));

            *self.open_upvalues.insert_before(
                allocated,
                |u_val| matches!(&**u_val, UpvalueRef::Live(s) if *s < slot),
            )
        }
    }

    fn fetch_upvalue<T: Into<usize>>(&mut self, index: T) -> RunResult {
        match self.frame().get_upvalue(index.into()) {
            Some(u) => match &*u {
                UpvalueRef::Live(slot) => {
                    if let Some(val) = self.stack.get(*slot).cloned() {
                        self.stack.push(val);
                        Ok(())
                    } else {
                        Err(RuntimeError::BadStackIndex(*slot, self.stack.len()))
                    }
                }
                UpvalueRef::Captured(ptr) => {
                    let val = (*ptr).clone();
                    self.stack.push(*val);
                    Ok(())
                }
            },
            _ => unreachable!(),
        }
    }

    fn set_upvalue<T: Into<usize>>(&mut self, index: T) -> RunResult {
        match self.frame().get_upvalue(index.into()) {
            Some(mut u) => match &mut *u {
                UpvalueRef::Live(slot) => {
                    let slot = *slot;
                    let val = self.peek(0)?.clone();
                    if let Some(el) = self.stack.get_mut(slot) {
                        *el = val;
                        Ok(())
                    } else {
                        Err(RuntimeError::BadStackIndex(slot, self.stack.len()))
                    }
                }
                UpvalueRef::Captured(ref mut val) => {
                    **val = self.peek(0)?.clone();
                    Ok(())
                }
            },
            _ => unreachable!(),
        }
    }

    fn close_upvalues(&mut self, max_height: usize) {
        while let Some(u_val) = self.open_upvalues.peek() {
            let slot = if let UpvalueRef::Live(slot) = &**u_val {
                if *slot < max_height {
                    break;
                }

                *slot
            } else {
                unreachable!("open upvalue list should not contain any closed upvalues")
            };

            if let Some(mut ptr) = self.open_upvalues.pop() {
                ptr.close(self.stack[slot].clone());
            }
        }
    }

    fn get_property<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        match self.stack.pop() {
            None => Err(RuntimeError::StackEmpty),
            Some(Value::Instance(i)) => {
                let var_str = self.read_string(index);

                if let Some(val) = i.fields.get(var_str) {
                    self.stack.push(val.clone());
                    Ok(())
                } else {
                    let var_string = var_str.to_string();
                    self.bind_method(i.class, i, var_string)
                }
            }
            Some(_) => Err(RuntimeError::ArgumentTypes),
        }
    }

    fn bind_method<T: AsRef<str>>(
        &mut self,
        class: Gc<Class>,
        instance: Gc<Instance>,
        name: T,
    ) -> RunResult {
        if let Some(m) = class.methods.get(name.as_ref()) {
            let (fun, upvalues) = match m {
                Value::Fun(f) => (f, Box::new([]) as Box<[_]>),
                Value::Closure(f, u) => (f, u.clone()),
                _ => return Err(RuntimeError::ArgumentTypes),
            };

            self.stack.push(Value::BoundMethod {
                recv: instance,
                fun: *fun,
                upvalues,
            });

            Ok(())
        } else {
            Err(RuntimeError::UndefinedProperty(name.as_ref().to_string()))
        }
    }

    fn set_property<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        let value = self.stack.pop().ok_or(RuntimeError::StackEmpty)?;

        match self.stack.pop() {
            None => Err(RuntimeError::StackEmpty),
            Some(Value::Instance(mut i)) => {
                let var_str = self.read_string(index);
                i.fields
                    .insert(var_str.to_string().into_boxed_str(), value.clone());
                self.stack.push(value);
                Ok(())
            }
            _ => Err(RuntimeError::ArgumentTypes),
        }
    }

    fn get_super<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        let (this_val, super_val) = self.pop_pair()?;

        if let (Value::Class(super_class), Value::Instance(this)) = (super_val, this_val) {
            let method_name = self.read_string(index).to_string();
            self.bind_method(super_class, this, method_name)
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }

    fn define_method<T: Into<usize> + fmt::Display + Copy>(&mut self, index: T) -> RunResult {
        let value = self.stack.pop().ok_or(RuntimeError::StackEmpty)?;

        match self.peek(0)? {
            Value::Class(c) => {
                let mut c = *c;
                let var_str = self.read_string(index);
                c.methods
                    .insert(var_str.to_string().into_boxed_str(), value);
                Ok(())
            }
            _ => Err(RuntimeError::ArgumentTypes),
        }
    }

    fn invoke<T: AsRef<str>>(&mut self, method: T, arg_count: u8) -> RunResult {
        if let Value::Instance(recv) = self.peek(arg_count.into())? {
            // check for fields, they shadow class methods
            if let Some(field) = recv.fields.get(method.as_ref()).cloned() {
                let l = self.stack.len();
                self.stack[l - usize::from(arg_count) - 1] = field;
                self.call_value_from_stack(arg_count)
            } else {
                let class = recv.class;
                self.invoke_from_class(class, method.as_ref(), arg_count)
            }
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }

    fn invoke_from_class(&mut self, class: Gc<Class>, method: &str, arg_count: u8) -> RunResult {
        if let Some(m) = class.methods.get(method) {
            self.call_value(m.clone(), arg_count)
        } else {
            Err(RuntimeError::UndefinedProperty(method.to_string()))
        }
    }

    fn call_value(&mut self, value: Value, arg_count: u8) -> RunResult {
        match value {
            Value::Closure(f, u) => self.call(f, u, arg_count),
            Value::Fun(f) => self.call(f, Box::new([]), arg_count),
            Value::Class(c) => {
                let cls = c;

                let val = Value::Instance(self.alloc(Instance::new(cls)));
                let l = self.stack.len();
                self.stack[l - usize::from(arg_count) - 1] = val;

                if let Some(init) = c.methods.get("init") {
                    match init {
                        Value::Fun(f) => {
                            let f = *f;
                            self.call(f, Box::new([]), arg_count)
                        }
                        Value::Closure(f, u) => {
                            let (f, u) = (*f, u.clone());
                            self.call(f, u, arg_count)
                        }
                        _ => Err(RuntimeError::ArgumentTypes),
                    }
                } else if arg_count == 0 {
                    Ok(())
                } else {
                    Err(RuntimeError::ArityMismatch(0, arg_count))
                }
            }
            Value::BoundMethod {
                recv,
                fun,
                upvalues,
                ..
            } => {
                let l = self.stack.len();
                self.stack[l - usize::from(arg_count) - 1] = Value::Instance(recv);
                self.call(fun, upvalues, arg_count)
            }
            Value::NativeFun(mut f) => {
                let from = self
                    .stack
                    .len()
                    .checked_sub(arg_count.into())
                    .ok_or(RuntimeError::StackEmpty)?;

                let result = (f.as_mut())(&self.stack[from..]);
                self.pop_many(u16::from(arg_count) + 1)?;
                self.stack
                    .push(result.map_err(RuntimeError::NativeFunError)?);

                Ok(())
            }
            _ => Err(RuntimeError::NotCallable),
        }
    }

    pub(super) fn call_value_from_stack(&mut self, arg_count: u8) -> RunResult {
        let val = self.peek(arg_count.into())?.clone();
        self.call_value(val, arg_count)
    }

    fn call(
        &mut self,
        callee: Gc<Fun>,
        upvalues: Box<[Gc<UpvalueRef>]>,
        arg_count: u8,
    ) -> RunResult {
        if arg_count == callee.arity {
            self.frames.push(CallFrame {
                func: callee,
                inst: 0,
                base: self
                    .stack
                    .len()
                    .checked_sub(1 + usize::from(arg_count))
                    .ok_or(RuntimeError::StackEmpty)?,
                upvalues,
            });
            Ok(())
        } else {
            Err(RuntimeError::ArityMismatch(callee.arity, arg_count))
        }
    }
}
