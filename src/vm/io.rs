use std::{
    cmp, fmt,
    io::{Error, ErrorKind, Read, Result, Write},
    mem,
};

use super::VM;

impl VM {
    /// Print directly to stdout, or buffer output internally.
    ///
    /// Pass `true` to this method to buffer print output internally. This output can then be
    /// accessed through this struct's [`Read`] implementation.
    ///
    /// Pass `false` (the default mode) to sink all program output directly to [`Stdout`].
    ///
    /// ### Example
    ///
    /// ```
    /// use std::io::Read;
    ///
    /// let mut vm = lox_lang::VM::default();
    /// vm.buffer_output(true);
    ///
    /// vm.interpret("print nil == true;");
    ///
    /// let mut buffer = String::new();
    /// vm.read_to_string(&mut buffer).unwrap();
    /// assert_eq!(buffer, "false\n");
    ///
    /// vm.interpret("for (var a = 3; a < 12; a = a + 3) print a;");
    ///
    /// buffer.clear();
    /// vm.read_to_string(&mut buffer).unwrap();
    /// assert_eq!(buffer, "3\n6\n9\n");
    /// ```
    ///
    /// [`Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    pub fn buffer_output(&mut self, should_buffer: bool) {
        if should_buffer && self.stdout.is_none() {
            self.stdout = Some(Vec::new());
        } else if !should_buffer && self.stdout.is_some() {
            self.stdout = None;
        }
    }

    pub(super) fn print(&mut self, args: fmt::Arguments) {
        if let Some(ref mut out) = self.stdout {
            out.write_fmt(args).unwrap();
        } else {
            print!("{}", args);
        }
    }

    fn try_read_buf(&mut self) -> Result<&mut Vec<u8>> {
        if let Some(ref mut my_buf) = &mut self.stdout {
            Ok(my_buf)
        } else {
            Err(Error::new(
                ErrorKind::Other,
                "Currently logging directly to stdout.",
            ))
        }
    }
}

impl Read for VM {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let my_buf = self.try_read_buf()?;

        if my_buf.is_empty() {
            return Ok(0);
        }

        let common_len = cmp::min(my_buf.len(), buf.len());

        let new_vec = my_buf.split_off(common_len);
        let old_vec = mem::replace(my_buf, new_vec);

        buf[0..common_len].copy_from_slice(&old_vec);

        Ok(common_len)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> {
        let my_buf = self.try_read_buf()?;

        if my_buf.is_empty() {
            return Ok(0);
        }

        let len = my_buf.len();
        buf.append(my_buf);

        Ok(len)
    }
}
