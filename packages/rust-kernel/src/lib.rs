mod analysis;
mod annotations;
mod binary;
mod details;
mod dto;
mod error;
mod morphology;
mod pack;
mod romanization;
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
