use fnv::FnvHashMap;
use std::{
    cell::RefCell,
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};
use string_interner::{symbol::SymbolU32, StringInterner};

use crate::{
    ast::{Identifier, Statement},
    eval::Env,
    token::IntegerSize,
};

#[derive(Debug)]
pub enum IR {
    Nothing,
    Integer(IntegerSize),
    Boolean(bool),
    Null,
    ReturnValue(Rc<IR>),
    Function(
        Rc<Vec<Identifier>>, // Parameters
        Rc<Statement>,       // Body
        Rc<RefCell<Env>>,    // Env
    ),
    String(InternedString),
    Array(Vec<Rc<IR>>),
    StdLib(BuiltIn),
    Hash(FnvHashMap<Rc<IR>, Rc<IR>>),
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IR::Nothing => Ok(()),
            IR::Integer(ir) => ir.fmt(f),
            IR::Boolean(ir) => ir.fmt(f),
            IR::Null => write!(f, "null"),
            IR::ReturnValue(ir) => ir.fmt(f),
            IR::Function(_, _, _) => Ok(()),
            IR::String(is) => is.fmt(f),
            IR::StdLib(bi) => bi.fmt(f),
            IR::Array(irs) => write!(
                f,
                "[{}]",
                irs.iter()
                    .map(|ir| ir.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            IR::Hash(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl PartialEq for IR {
    fn eq(&self, b: &Self) -> bool {
        match (self, b) {
            (IR::Integer(a), IR::Integer(b)) => a == b,
            (IR::Boolean(a), IR::Boolean(b)) => a == b,
            (IR::String(a), IR::String(b)) => a.string_key == b.string_key,
            (IR::Null, IR::Null) => true,
            _ => false,
        }
    }
}

impl Eq for IR {}

impl Hash for IR {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            IR::Integer(integer) => integer.hash(state),
            IR::String(interned) => interned.string_key.hash(state),
            IR::Boolean(boolean) => boolean.hash(state),
            ir => panic!("Unhashable: {}", ir),
        }
    }
}

#[derive(Debug)]
pub struct InternedString {
    pub string_key: SymbolU32,
    pub interner: Rc<RefCell<StringInterner>>,
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let interner = self.interner.borrow();
        let value = interner
            .resolve(self.string_key)
            .expect("String should have been interned");
        write!(f, "{}", value)
    }
}

#[derive(Debug)]
pub enum BuiltIn {
    Len,
    Last,
    Head,
    Tail,
    Push,
}

impl fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltIn::Len => write!(f, "len"),
            BuiltIn::Last => write!(f, "last"),
            BuiltIn::Head => write!(f, "head"),
            BuiltIn::Tail => write!(f, "tail"),
            BuiltIn::Push => write!(f, "push"),
        }
    }
}
