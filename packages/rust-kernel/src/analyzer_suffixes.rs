//! Exact suffix candidate transforms shared by the recursive analyzer facade.
//!
//! Recursive surface discovery remains with the analyzer owner. This module
//! owns the semantic selection, suffix component, compound, and abbreviation
//! transforms so those operations have one direct implementation.

mod compound;
pub use compound::{SuffixCompound, abbreviate_suffix, compound_suffix, unique_suffix};

#[cfg(test)]
mod tests;
