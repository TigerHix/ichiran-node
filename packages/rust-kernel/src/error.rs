use std::fmt::{Display, Formatter};

use serde::Serialize;

pub type Result<T> = std::result::Result<T, KernelError>;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ErrorCode {
    InvalidHeader,
    UnsupportedVersion,
    InvalidDirectory,
    CorruptSection,
    CorruptPayload,
    CorruptIndex,
    CorruptBlock,
    MissingSection,
    OutOfRange,
    InvalidInput,
    Internal,
}

#[derive(Debug, Serialize)]
pub struct KernelError {
    pub code: ErrorCode,
    pub message: String,
}

impl KernelError {
    pub fn new(code: ErrorCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

impl Display for KernelError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.message)
    }
}

impl std::error::Error for KernelError {}
