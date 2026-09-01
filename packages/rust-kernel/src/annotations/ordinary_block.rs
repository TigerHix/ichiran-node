use std::io::Read;

use flate2::bufread::GzDecoder;
use serde_json::Value;

use crate::binary::{checked_range, crc32};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;
use crate::support::{
    SupportSplit, SupportSplitConjugation, SupportSplitKind, SupportSplitPart, SupportSplitWord,
};

use super::{ANNOTATION_CACHE_BLOCKS, AnalyzerAnnotations};

const FORMAT_VERSION: u64 = 4;

#[derive(Clone, Copy, Debug)]
pub(super) struct AnnotationIndex {
    pub(super) seq: u32,
    pub(super) offset: usize,
    pub(super) compressed: usize,
    pub(super) uncompressed: usize,
    pub(super) checksum: u32,
    pub(super) splits: usize,
    pub(super) hints: usize,
}

#[derive(Debug)]
pub(super) struct AnnotationHint {
    pub(super) route: Route,
    pub(super) surface: String,
    pub(super) reading: String,
    pub(super) hint: String,
}

#[derive(Debug)]
pub(super) struct DecodedAnnotationBlock {
    pub(super) splits: Vec<SupportSplit>,
    pub(super) hints: Vec<AnnotationHint>,
}

pub(super) fn decode_annotation_block(
    compressed: &[u8],
    index: AnnotationIndex,
) -> Result<DecodedAnnotationBlock> {
    let bytes = gunzip(compressed, index.uncompressed, index.seq)?;
    if crc32(&bytes) != index.checksum {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("annotation block {} checksum does not match", index.seq),
        ));
    }
    parse_annotation_block(&bytes, index)
}

fn gunzip(compressed: &[u8], expected: usize, seq: u32) -> Result<Vec<u8>> {
    let limit = expected.checked_add(1).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "annotation gzip decoded length overflows",
        )
    })?;
    let limit = u64::try_from(limit).map_err(|_| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "annotation gzip decoded length exceeds the reader limit",
        )
    })?;
    let decoder = GzDecoder::new(compressed);
    let mut bounded = decoder.take(limit);
    let mut decoded = Vec::new();
    bounded.read_to_end(&mut decoded).map_err(|error| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("annotation block {seq} gzip decode failed: {error}"),
        )
    })?;
    if decoded.len() != expected {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!(
                "annotation block {seq} decoded {} bytes; expected {expected}",
                decoded.len()
            ),
        ));
    }
    if !bounded.into_inner().into_inner().is_empty() {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("annotation block {seq} gzip has trailing compressed bytes"),
        ));
    }
    Ok(decoded)
}

fn parse_annotation_block(bytes: &[u8], index: AnnotationIndex) -> Result<DecodedAnnotationBlock> {
    let parsed: Value = serde_json::from_slice(bytes).map_err(|_| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("annotation block {} is not valid JSON", index.seq),
        )
    })?;
    let tuple = array(&parsed, "annotation block")?;
    if tuple.len() != 4
        || unsigned(&tuple[0], "annotation version", u64::MAX)? != FORMAT_VERSION
        || unsigned(&tuple[1], "annotation sequence", u32::MAX.into())? != u64::from(index.seq)
    {
        return invalid_structure(index.seq);
    }
    let split_values = array(&tuple[2], "annotation splits")?;
    let hint_values = array(&tuple[3], "annotation hints")?;
    if split_values.len() != index.splits || hint_values.len() != index.hints {
        return invalid_structure(index.seq);
    }
    let splits = split_values
        .iter()
        .map(|value| split(index.seq, value))
        .collect::<Result<Vec<_>>>()?;
    let hints = hint_values.iter().map(hint).collect::<Result<Vec<_>>>()?;
    Ok(DecodedAnnotationBlock { splits, hints })
}

fn invalid_structure<T>(seq: u32) -> Result<T> {
    Err(KernelError::new(
        ErrorCode::CorruptBlock,
        format!("annotation block {seq} has invalid structure"),
    ))
}

fn split(seq: u32, value: &Value) -> Result<SupportSplit> {
    let tuple = array(value, "split tuple")?;
    if tuple.len() != 8 {
        return corrupt("invalid split tuple");
    }
    let parts = array(&tuple[3], "split parts")?
        .iter()
        .map(part)
        .collect::<Result<Vec<_>>>()?;
    let root = array(&tuple[7], "split root")?
        .iter()
        .enumerate()
        .map(|(index, entry)| {
            unsigned(entry, &format!("split root {index}"), u32::MAX.into()).map(|v| v as u32)
        })
        .collect::<Result<Vec<_>>>()?;
    let kind = match unsigned(&tuple[2], "split kind", 1)? {
        0 => SupportSplitKind::Split,
        _ => SupportSplitKind::Segsplit,
    };
    Ok(SupportSplit {
        definition_seq: seq,
        route: route(&tuple[0])?,
        surface: text(&tuple[1], "split surface")?.to_owned(),
        kind,
        parts,
        score: signed_i32(&tuple[4], "split score")?,
        primary: unsigned(&tuple[5], "split primary", u8::MAX.into())? as u8,
        connector: text(&tuple[6], "split connector")?.to_owned(),
        root,
    })
}

fn part(value: &Value) -> Result<SupportSplitPart> {
    if value.as_u64() == Some(1) {
        return Ok(SupportSplitPart::Score);
    }
    if value.as_u64() == Some(2) {
        return Ok(SupportSplitPart::Pscore);
    }
    let tuple = array(value, "split-part tuple")?;
    if tuple.len() != 10 || tuple[0].as_u64() != Some(0) {
        return corrupt("invalid split-part tuple");
    }
    let flags = unsigned(&tuple[8], "split-part flags", 3)? as u8;
    let generated = if tuple[9].is_null() {
        None
    } else {
        Some(
            array(&tuple[9], "split-part generated locator")?
                .iter()
                .enumerate()
                .map(|(index, entry)| conjugation(index, entry))
                .collect::<Result<Vec<_>>>()?,
        )
    };
    Ok(SupportSplitPart::Word(SupportSplitWord {
        route: route(&tuple[1])?,
        seq: unsigned(&tuple[2], "split-part seq", u32::MAX.into())? as u32,
        text: text(&tuple[3], "split-part text")?.to_owned(),
        best: nullable_text(&tuple[4], "split-part best")?,
        ord: unsigned(&tuple[5], "split-part ordinal", u16::MAX.into())? as u16,
        common: nullable_u8(&tuple[6], "split-part common")?,
        common_tags: text(&tuple[7], "split-part common tags")?.to_owned(),
        conjugatable: flags & 1 != 0,
        nokanji: flags & 2 != 0,
        generated,
    }))
}

fn conjugation(index: usize, value: &Value) -> Result<SupportSplitConjugation> {
    let tuple = array(value, "split-part generated locator")?;
    if tuple.len() != 6 {
        return corrupt(format!("invalid split-part generated locator {index}"));
    }
    Ok(SupportSplitConjugation {
        from: unsigned(
            &tuple[0],
            &format!("split-part generated from {index}"),
            u32::MAX.into(),
        )? as u32,
        via: unsigned(&tuple[1], &format!("split-part generated via {index}"), 1)? == 1,
        pos: text(&tuple[2], &format!("split-part generated POS {index}"))?.to_owned(),
        kind: unsigned(
            &tuple[3],
            &format!("split-part generated type {index}"),
            u16::MAX.into(),
        )? as u16,
        negative: nullable_bool(&tuple[4], &format!("split-part generated negative {index}"))?,
        formal: nullable_bool(&tuple[5], &format!("split-part generated formal {index}"))?,
    })
}

fn hint(value: &Value) -> Result<AnnotationHint> {
    let tuple = array(value, "hint tuple")?;
    if tuple.len() != 4 {
        return corrupt("invalid hint tuple");
    }
    Ok(AnnotationHint {
        route: route(&tuple[0])?,
        surface: text(&tuple[1], "hint surface")?.to_owned(),
        reading: text(&tuple[2], "hint reading")?.to_owned(),
        hint: text(&tuple[3], "hint value")?.to_owned(),
    })
}

fn route(value: &Value) -> Result<Route> {
    match value.as_u64() {
        Some(0) => Ok(Route::Kana),
        Some(1) => Ok(Route::Kanji),
        _ => corrupt(format!("invalid annotation route {value}")),
    }
}

fn array<'a>(value: &'a Value, label: &str) -> Result<&'a [Value]> {
    value.as_array().map(Vec::as_slice).ok_or_else(|| {
        KernelError::new(ErrorCode::CorruptBlock, format!("{label} is not an array"))
    })
}

fn unsigned(value: &Value, label: &str, max: u64) -> Result<u64> {
    value.as_u64().filter(|value| *value <= max).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("{label} is not an unsigned integer"),
        )
    })
}

fn signed_i32(value: &Value, label: &str) -> Result<i32> {
    value
        .as_i64()
        .filter(|value| i32::try_from(*value).is_ok())
        .map(|value| value as i32)
        .ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                format!("{label} is not a signed 32-bit integer"),
            )
        })
}

fn text<'a>(value: &'a Value, label: &str) -> Result<&'a str> {
    value
        .as_str()
        .ok_or_else(|| KernelError::new(ErrorCode::CorruptBlock, format!("{label} is not text")))
}

fn nullable_text(value: &Value, label: &str) -> Result<Option<String>> {
    if value.is_null() {
        Ok(None)
    } else {
        text(value, label).map(|value| Some(value.to_owned()))
    }
}

fn nullable_u8(value: &Value, label: &str) -> Result<Option<u8>> {
    if value.is_null() {
        Ok(None)
    } else {
        unsigned(value, label, 0xfe).map(|value| Some(value as u8))
    }
}

fn nullable_bool(value: &Value, label: &str) -> Result<Option<bool>> {
    if value.is_null() {
        Ok(None)
    } else {
        value.as_bool().map(Some).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                format!("{label} is not a nullable boolean"),
            )
        })
    }
}

fn corrupt<T>(message: impl Into<String>) -> Result<T> {
    Err(KernelError::new(ErrorCode::CorruptBlock, message))
}

impl AnalyzerAnnotations {
    #[allow(dead_code)]
    pub(crate) fn split(
        &mut self,
        definition_seq: u32,
        route: Route,
        surface: &str,
        kind: SupportSplitKind,
    ) -> Result<Option<SupportSplit>> {
        if !self.load_annotation(definition_seq)? {
            return Ok(None);
        }
        Ok(self.annotation_cache[&definition_seq]
            .splits
            .iter()
            .find(|split| split.route == route && split.surface == surface && split.kind == kind)
            .cloned())
    }

    #[allow(dead_code)]
    pub(crate) fn hint(
        &mut self,
        definition_seq: u32,
        route: Route,
        surface: &str,
        reading: &str,
    ) -> Result<Option<String>> {
        if !self.load_annotation(definition_seq)? {
            return Ok(None);
        }
        Ok(self.annotation_cache[&definition_seq]
            .hints
            .iter()
            .find(|hint| hint.route == route && hint.surface == surface && hint.reading == reading)
            .map(|hint| hint.hint.clone()))
    }

    pub(super) fn load_annotation(&mut self, definition_seq: u32) -> Result<bool> {
        if self.annotation_cache.contains_key(&definition_seq) {
            self.promote_annotation(definition_seq);
            return Ok(true);
        }
        let Some(index) = self.annotation_index(definition_seq) else {
            return Ok(false);
        };
        let offset = self
            .annotation_data_offset
            .checked_add(index.offset)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "annotation block data offset overflows",
                )
            })?;
        let compressed = checked_range(
            &self.bytes,
            offset,
            index.compressed,
            ErrorCode::CorruptBlock,
            "annotation compressed block",
        )?;
        let decoded = decode_annotation_block(compressed, index)?;
        self.annotation_cache.insert(definition_seq, decoded);
        self.annotation_lru.push_back(definition_seq);
        if self.annotation_lru.len() > ANNOTATION_CACHE_BLOCKS {
            let oldest = self.annotation_lru.pop_front().ok_or_else(|| {
                KernelError::new(ErrorCode::Internal, "annotation LRU is unexpectedly empty")
            })?;
            self.annotation_cache.remove(&oldest);
        }
        Ok(true)
    }

    fn annotation_index(&self, definition_seq: u32) -> Option<AnnotationIndex> {
        self.annotation_indexes
            .binary_search_by_key(&definition_seq, |index| index.seq)
            .ok()
            .map(|index| self.annotation_indexes[index])
    }

    fn promote_annotation(&mut self, definition_seq: u32) {
        if let Some(position) = self
            .annotation_lru
            .iter()
            .position(|seq| *seq == definition_seq)
        {
            self.annotation_lru.remove(position);
        }
        self.annotation_lru.push_back(definition_seq);
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use flate2::{Compression, write::GzEncoder};

    use super::*;

    fn gzip(bytes: &[u8]) -> Vec<u8> {
        let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
        encoder.write_all(bytes).unwrap();
        encoder.finish().unwrap()
    }

    fn index(bytes: &[u8]) -> AnnotationIndex {
        AnnotationIndex {
            seq: 42,
            offset: 0,
            compressed: 0,
            uncompressed: bytes.len(),
            checksum: crc32(bytes),
            splits: 1,
            hints: 1,
        }
    }

    #[test]
    fn parses_complete_split_and_hint_tuples() {
        let json = br#"[4,42,[[1,"surface",1,[[0,0,7,"text",null,8,3,"ichi1",3,[[9,1,"v1",4,null,true]]],1,2],-6,2,"connector",[10,11]]],[[0,"surface","reading","hint"]]]"#;
        let decoded = parse_annotation_block(json, index(json)).unwrap();
        assert_eq!(decoded.splits.len(), 1);
        assert_eq!(decoded.hints.len(), 1);
        assert_eq!(decoded.splits[0].route, Route::Kanji);
        assert_eq!(decoded.splits[0].kind, SupportSplitKind::Segsplit);
        assert_eq!(decoded.splits[0].score, -6);
        assert_eq!(decoded.hints[0].hint, "hint");
        let SupportSplitPart::Word(word) = &decoded.splits[0].parts[0] else {
            panic!("expected word")
        };
        assert!(word.conjugatable);
        assert!(word.nokanji);
        assert_eq!(word.generated.as_ref().unwrap()[0].formal, Some(true));
    }

    #[test]
    fn rejects_trailing_gzip_bytes() {
        let mut compressed = gzip(b"decoded");
        compressed.push(0);
        let error = gunzip(&compressed, 7, 42).unwrap_err();
        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(
            error.message,
            "annotation block 42 gzip has trailing compressed bytes"
        );
    }

    #[test]
    fn rejects_oversized_gzip_expansion() {
        let compressed = gzip(&vec![0; 1024]);
        let error = gunzip(&compressed, 7, 42).unwrap_err();
        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(
            error.message,
            "annotation block 42 decoded 8 bytes; expected 7"
        );
    }

    #[test]
    fn rejects_count_mismatch_and_non_integer_fields() {
        let mismatch = br#"[4,42,[],[]]"#;
        assert_eq!(
            parse_annotation_block(mismatch, index(mismatch))
                .unwrap_err()
                .code,
            ErrorCode::CorruptBlock
        );
        let invalid = br#"[4,42,[[0,"x",0,[],1.5,0,"",[]]],[[0,"x","x","x"]]]"#;
        assert_eq!(
            parse_annotation_block(invalid, index(invalid))
                .unwrap_err()
                .message,
            "split score is not a signed 32-bit integer"
        );
    }
}
