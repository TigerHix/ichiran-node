mod analysis;
#[allow(dead_code)]
mod analyzer_counters;
#[allow(dead_code)]
mod analyzer_lexicon;
#[allow(dead_code)]
mod analyzer_model;
#[allow(dead_code)]
mod analyzer_paths;
#[allow(dead_code)]
mod analyzer_rules;
#[allow(dead_code)]
mod analyzer_scoring;
#[allow(dead_code)]
mod analyzer_suffixes;
mod annotations;
mod binary;
pub mod characters;
mod details;
mod dto;
mod error;
mod morphology;
pub mod numbers;
mod pack;
pub mod romanization;
mod roots;
mod scoring;
mod support;
mod surface;
mod text;

#[cfg(all(feature = "native", not(target_arch = "wasm32")))]
mod ffi;
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
mod wasm;

pub use analysis::Kernel;
pub use annotations::{GeneratedFacts, GeneratedMember};
pub use details::{DetailEntry, DetailRange, DetailStore};
pub use dto::{AnalysisResult, Utf16Text};
pub use error::{ErrorCode, KernelError, Result};
pub use morphology::{MorphologyCandidate, MorphologyProperty, Route};
pub use pack::{Pack, PackManifest, PackSection};
pub use surface::{SurfaceIndex, SurfaceMatch};
