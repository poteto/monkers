mod integer;
mod boolean;
mod null;

pub use integer::IRInteger;
pub use boolean::IRBoolean;
pub use null::IRNull;

trait IRTypedObject {
    const TYPE: IRObjectKind;
}

enum IRObjectKind {
    Integer,
    Boolean,
    Null,
}

pub enum IR {
    Integer(IRInteger),
    Boolean(IRBoolean),
    Null(IRNull),
    NotImplemented
}
