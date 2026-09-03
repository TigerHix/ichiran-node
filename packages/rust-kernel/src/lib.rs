mod analysis;
mod analyzer_counters;
mod analyzer_engine;
mod analyzer_legacy;
mod analyzer_lexicon;
mod analyzer_model;
mod analyzer_options;
mod analyzer_paths;
mod analyzer_projection;
mod analyzer_romanize;
mod analyzer_rules;
mod analyzer_scoring;
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
mod support;
mod surface;
mod text;
mod token_details;

#[cfg(all(feature = "native", not(target_arch = "wasm32")))]
mod ffi;
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
mod wasm;

pub use analysis::{
    Kernel, LegacyDetailSession, LegacyDetailStep, TokenDetailsSession, TokenDetailsStep,
};
pub use analyzer_model::EntityHint;
pub use analyzer_options::AnalyzeOptions;
pub use annotations::{GeneratedFacts, GeneratedMember};
pub use details::{DetailEntry, DetailRange, DetailStore};
pub use dto::{AnalysisResult, Utf16Text};
pub use error::{ErrorCode, KernelError, Result};
pub use morphology::{MorphologyCandidate, MorphologyProperty, Route};
pub use pack::{Pack, PackManifest, PackSection};
pub use romanization::RomanizationName;
pub use surface::{SurfaceIndex, SurfaceMatch};
pub use token_details::{
    TokenConjugation, TokenConjugationProperty, TokenCounter, TokenDetailForm, TokenDetails,
    TokenMeaning,
};
