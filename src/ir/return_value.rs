use std::{fmt, rc::Rc};

use super::IR;

#[derive(Clone, Debug)]
pub struct IRReturnValue {
    pub value: Rc<IR>,
}

impl fmt::Display for IRReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}
