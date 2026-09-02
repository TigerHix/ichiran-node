use std::cell::RefCell;

use serde::Serialize;

use crate::binary::{ByteSlice, magic, u16_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};
use crate::text::string as utf16_string;

mod validation;

use validation::{covered_end, strictly_after, validate_layout};

const MAGIC: &[u8; 8] = b"ICHIMOR1";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 144;
const NONE: u32 = 0xffff_ffff;

const POS_BYTES: usize = 4;
const RULE_BYTES: usize = 20;
const SUFFIX_BYTES: usize = 12;
const TEMPLATE_BYTES: usize = 12;
const ROOT_KEY_BYTES: usize = 16;
const ROOT_RECORD_BYTES: usize = 16;
const ROOT_GROUP_BYTES: usize = 12;
const ROOT_FORM_BYTES: usize = 4;
const PATCH_BUCKET_BYTES: usize = 12;
const PATCH_BYTES: usize = 40;
const TOMBSTONE_BYTES: usize = 20;
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Route {
    Kana,
    Kanji,
}

impl Route {
    pub(crate) fn code(self) -> u8 {
        match self {
            Self::Kana => 0,
            Self::Kanji => 1,
        }
    }

    fn from_code(code: u8) -> Result<Self> {
        match code {
            0 => Ok(Self::Kana),
            1 => Ok(Self::Kanji),
            _ => Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("invalid morphology route {code}"),
            )),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct MorphologyProperty {
    pub pos: String,
    #[serde(rename = "type")]
    pub kind: u8,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
    pub ordinal: u8,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MorphologyCandidate {
    pub route: Route,
    pub surface: String,
    pub root_seq: u32,
    pub source_text: String,
    pub source_form: String,
    pub source_reading: String,
    pub form: String,
    pub reading: String,
    pub intermediate: Option<String>,
    pub rule_ids: Vec<u32>,
    pub path: Vec<MorphologyProperty>,
    pub ord: u8,
    pub common: Option<u8>,
    pub compatibility: &'static str,
}

#[derive(Clone, Copy)]
struct Header {
    byte_length: usize,
    positions: usize,
    rules: usize,
    suffixes: usize,
    templates: usize,
    root_keys: usize,
    root_records: usize,
    root_hash_slots: usize,
    root_groups: usize,
    root_forms: usize,
    patch_buckets: usize,
    patches: usize,
    strings: usize,
    string_code_units: usize,
    pos_offset: usize,
    rule_offset: usize,
    suffix_offset: usize,
    template_offset: usize,
    root_key_offset: usize,
    root_record_offset: usize,
    root_hash_offset: usize,
    root_group_offset: usize,
    root_form_offset: usize,
    patch_bucket_offset: usize,
    patch_offset: usize,
    tombstones: usize,
    tombstone_offset: usize,
    string_dir_offset: usize,
    string_pool_offset: usize,
}

#[derive(Clone)]
struct SuffixBucket {
    suffix: Vec<u16>,
    first: usize,
    count: usize,
}

#[derive(Clone)]
struct PatchBucket {
    route: Route,
    surface: Vec<u16>,
    first: usize,
    count: usize,
}

#[derive(Clone)]
struct Tombstone {
    route: Route,
    surface: Vec<u16>,
    root_seq: u32,
    first_rule: u32,
    second_rule: u32,
}

pub struct Morphology {
    bytes: ByteSlice,
    header: Header,
    positions: Vec<String>,
    strings: RefCell<Vec<Option<Vec<u16>>>>,
    suffixes: Vec<SuffixBucket>,
    patches: Vec<PatchBucket>,
    tombstones: Vec<Tombstone>,
}

impl Morphology {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete ICHIMOR1 header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "morphology version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported morphology version {version}"),
            ));
        }
        if u16_at(
            &bytes,
            10,
            ErrorCode::InvalidHeader,
            "morphology header size",
        )? as usize
            != HEADER_BYTES
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "morphology header size is invalid",
            ));
        }
        let field = |offset: usize, label: &str| -> Result<usize> {
            Ok(u32_at(&bytes, offset, ErrorCode::InvalidHeader, label)? as usize)
        };
        let header = Header {
            byte_length: field(12, "morphology byte length")?,
            positions: field(16, "morphology position count")?,
            rules: field(20, "morphology rule count")?,
            suffixes: field(24, "morphology suffix count")?,
            templates: field(28, "morphology template count")?,
            root_keys: field(32, "morphology root-key count")?,
            root_records: field(36, "morphology root-record count")?,
            root_hash_slots: field(40, "morphology root hash count")?,
            root_groups: field(44, "morphology root-group count")?,
            root_forms: field(48, "morphology root-form count")?,
            patch_buckets: field(52, "morphology patch-bucket count")?,
            patches: field(56, "morphology patch count")?,
            strings: field(60, "morphology string count")?,
            string_code_units: field(64, "morphology string-pool length")?,
            pos_offset: field(68, "morphology POS offset")?,
            rule_offset: field(72, "morphology rule offset")?,
            suffix_offset: field(76, "morphology suffix offset")?,
            template_offset: field(80, "morphology template offset")?,
            root_key_offset: field(84, "morphology root-key offset")?,
            root_record_offset: field(88, "morphology root-record offset")?,
            root_hash_offset: field(92, "morphology root-hash offset")?,
            root_group_offset: field(96, "morphology root-group offset")?,
            root_form_offset: field(100, "morphology root-form offset")?,
            patch_bucket_offset: field(104, "morphology patch-bucket offset")?,
            patch_offset: field(108, "morphology patch offset")?,
            string_dir_offset: field(112, "morphology string-directory offset")?,
            string_pool_offset: field(116, "morphology string-pool offset")?,
            tombstones: field(120, "morphology tombstone count")?,
            tombstone_offset: field(124, "morphology tombstone offset")?,
        };
        if header.byte_length != bytes.len()
            || header.root_hash_slots < 2
            || !header.root_hash_slots.is_power_of_two()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "morphology size or root hash count is invalid",
            ));
        }
        validate_layout(&bytes, header)?;
        let strings = RefCell::new(vec![None; header.strings]);
        let mut reader = Self {
            bytes,
            header,
            positions: Vec::with_capacity(header.positions),
            strings,
            suffixes: Vec::with_capacity(header.suffixes),
            patches: Vec::with_capacity(header.patch_buckets),
            tombstones: Vec::with_capacity(header.tombstones),
        };
        reader.validate_strings()?;
        for index in 0..header.positions {
            let id = reader.u32(header.pos_offset + index * POS_BYTES)? as usize;
            let value = reader.string_text(id)?;
            if reader.positions.last().is_some_and(|prior| prior >= &value) {
                return Err(corrupt("morphology POS table is not strictly sorted"));
            }
            reader.positions.push(value);
        }
        let mut next_template = 0;
        let mut previous_suffix: Option<Vec<u16>> = None;
        for index in 0..header.suffixes {
            let at = header.suffix_offset + index * SUFFIX_BYTES;
            let suffix = reader.string_units(reader.u32(at)? as usize)?;
            let first = reader.u32(at + 4)? as usize;
            let count = reader.u32(at + 8)? as usize;
            let end = covered_end(first, count, header.templates, "morphology suffix bucket")?;
            if first != next_template
                || count == 0
                || previous_suffix
                    .as_ref()
                    .is_some_and(|prior| prior >= &suffix)
            {
                return Err(corrupt("morphology suffix buckets are not canonical"));
            }
            let mut previous_template = None;
            for template in first..end {
                let template_at = header.template_offset + template * TEMPLATE_BYTES;
                let removed = reader.string_units(reader.u32(template_at)? as usize)?;
                let first_rule = reader.rule_id(reader.u32(template_at + 4)?)?;
                let second_rule = reader.u32(template_at + 8)?;
                if second_rule != NONE {
                    reader.rule_id(second_rule)?;
                }
                strictly_after(
                    &mut previous_template,
                    (removed, first_rule, second_rule),
                    "morphology templates are not strictly sorted",
                )?;
            }
            previous_suffix = Some(suffix.clone());
            reader.suffixes.push(SuffixBucket {
                suffix,
                first,
                count,
            });
            next_template = end;
        }
        if next_template != header.templates {
            return Err(corrupt("morphology suffix buckets do not cover templates"));
        }
        let mut next_patch = 0;
        let mut previous_patch_bucket = None;
        for index in 0..header.patch_buckets {
            let at = header.patch_bucket_offset + index * PATCH_BUCKET_BYTES;
            let surface = reader.string_units(reader.u32(at)? as usize)?;
            let first = reader.u32(at + 4)? as usize;
            let count = reader.u16(at + 8)? as usize;
            let route = Route::from_code(reader.u8(at + 10)?)?;
            let end = covered_end(first, count, header.patches, "morphology patch bucket")?;
            let bucket_key = (route.code(), surface.clone());
            if first != next_patch
                || count == 0
                || end > header.patches
                || previous_patch_bucket
                    .as_ref()
                    .is_some_and(|prior| prior >= &bucket_key)
            {
                return Err(corrupt("morphology patch buckets are not canonical"));
            }
            let mut previous_patch = None;
            for patch in first..end {
                let patch_at = header.patch_offset + patch * PATCH_BYTES;
                let root_seq = reader.u32(patch_at)?;
                if root_seq == 0 {
                    return Err(corrupt("morphology patch root sequence is zero"));
                }
                let source_text = reader.string_units(reader.u32(patch_at + 4)? as usize)?;
                for offset in [8, 12, 16, 20] {
                    reader.string_id(reader.u32(patch_at + offset)? as usize)?;
                }
                let first_rule = reader.rule_id(reader.u32(patch_at + 24)?)?;
                let second_rule = reader.u32(patch_at + 28)?;
                if second_rule != NONE {
                    reader.rule_id(second_rule)?;
                }
                let intermediate = reader.u32(patch_at + 32)?;
                if intermediate != NONE {
                    reader.string_id(intermediate as usize)?;
                }
                strictly_after(
                    &mut previous_patch,
                    (root_seq, source_text, first_rule, second_rule),
                    "morphology patches are not strictly sorted",
                )?;
            }
            previous_patch_bucket = Some(bucket_key);
            reader.patches.push(PatchBucket {
                route,
                surface,
                first,
                count,
            });
            next_patch = end;
        }
        if next_patch != header.patches {
            return Err(corrupt("morphology patch buckets do not cover patches"));
        }
        let mut previous_tombstone = None;
        for index in 0..header.tombstones {
            let at = header.tombstone_offset + index * TOMBSTONE_BYTES;
            let root_seq = reader.u32(at)?;
            if root_seq == 0 {
                return Err(corrupt("morphology tombstone root sequence is zero"));
            }
            let surface = reader.string_units(reader.u32(at + 4)? as usize)?;
            let first_rule = reader.u32(at + 8)?;
            let second_rule = reader.u32(at + 12)?;
            reader.rule_id(first_rule)?;
            if second_rule != NONE {
                reader.rule_id(second_rule)?;
            }
            let route = Route::from_code(reader.u8(at + 16)?)?;
            strictly_after(
                &mut previous_tombstone,
                (
                    route.code(),
                    surface.clone(),
                    root_seq,
                    first_rule,
                    second_rule,
                ),
                "morphology tombstones are not strictly sorted",
            )?;
            reader.tombstones.push(Tombstone {
                root_seq,
                surface,
                first_rule,
                second_rule,
                route,
            });
        }
        reader.validate_records()?;
        Ok(reader)
    }

    pub fn position(&self, index: usize) -> Result<&str> {
        self.positions
            .get(index)
            .map(String::as_str)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::OutOfRange,
                    format!("morphology position {index} is missing"),
                )
            })
    }

    pub fn lookup(&self, surface: &[u16], route: Route) -> Result<Vec<MorphologyCandidate>> {
        let mut candidates = Vec::new();
        let mut seen: Vec<Vec<u16>> = Vec::new();
        for bucket in &self.suffixes {
            if bucket.suffix.len() > surface.len() || !surface.ends_with(&bucket.suffix) {
                continue;
            }
            let prefix = &surface[..surface.len() - bucket.suffix.len()];
            for relative in 0..bucket.count {
                let at = self.header.template_offset + (bucket.first + relative) * TEMPLATE_BYTES;
                let removed = self.string_units(self.u32(at)? as usize)?;
                let first_rule = self.u32(at + 4)?;
                let second_rule = self.u32(at + 8)?;
                self.rule_id(first_rule)?;
                if second_rule != NONE {
                    self.rule_id(second_rule)?;
                }
                let mut source_text = prefix.to_vec();
                source_text.extend_from_slice(&removed);
                let pos_id = self.rule_pos(first_rule)?;
                let Some(key) = self.find_root_key(route, pos_id, &source_text)? else {
                    continue;
                };
                let intermediate = self.apply_rule(&source_text, first_rule)?;
                let generated = if second_rule == NONE {
                    intermediate.clone()
                } else {
                    self.apply_rule(&intermediate, second_rule)?
                };
                if generated != surface {
                    continue;
                }
                let key_at = self.header.root_key_offset + key * ROOT_KEY_BYTES;
                let first_record = self.u32(key_at + 4)? as usize;
                let record_count = self.u32(key_at + 8)? as usize;
                for offset in 0..record_count {
                    let record_at = self.header.root_record_offset
                        + (first_record + offset) * ROOT_RECORD_BYTES;
                    let root_group = self.u32(record_at)? as usize;
                    let root_seq =
                        self.u32(self.header.root_group_offset + root_group * ROOT_GROUP_BYTES)?;
                    if (second_rule != NONE && self.root_has_form(root_group, &intermediate)?)
                        || self.root_has_form(root_group, surface)?
                        || self.is_tombstone(route, surface, root_seq, first_rule, second_rule)
                    {
                        continue;
                    }
                    let source_form = self.string_units(self.u32(record_at + 4)? as usize)?;
                    let source_reading = self.string_units(self.u32(record_at + 8)? as usize)?;
                    let mut path = vec![self.rule_property(first_rule)?];
                    if second_rule != NONE {
                        path.push(self.rule_property(second_rule)?);
                    }
                    let form_intermediate = self.apply_rule(&source_form, first_rule)?;
                    let reading_intermediate = self.apply_rule(&source_reading, first_rule)?;
                    let form = if second_rule == NONE {
                        form_intermediate
                    } else {
                        self.apply_rule(&form_intermediate, second_rule)?
                    };
                    let reading = if second_rule == NONE {
                        reading_intermediate
                    } else {
                        self.apply_rule(&reading_intermediate, second_rule)?
                    };
                    let common = self.u8(record_at + 13)?;
                    let candidate = MorphologyCandidate {
                        route,
                        surface: utf16_string(surface, "morphology surface")?,
                        root_seq,
                        source_text: utf16_string(&source_text, "morphology source")?,
                        source_form: utf16_string(&source_form, "morphology source form")?,
                        source_reading: utf16_string(&source_reading, "morphology source reading")?,
                        form: utf16_string(&form, "morphology form")?,
                        reading: utf16_string(&reading, "morphology reading")?,
                        intermediate: (second_rule != NONE)
                            .then(|| utf16_string(&intermediate, "morphology intermediate"))
                            .transpose()?,
                        rule_ids: if second_rule == NONE {
                            vec![first_rule]
                        } else {
                            vec![first_rule, second_rule]
                        },
                        path,
                        ord: self.u8(record_at + 12)?,
                        common: (common != 0xff).then_some(common),
                        compatibility: "rule",
                    };
                    let key = canonical_key(&candidate)?;
                    if !seen.contains(&key) {
                        seen.push(key);
                        candidates.push(candidate);
                    }
                }
            }
        }
        for bucket in self
            .patches
            .iter()
            .filter(|value| value.route == route && value.surface == surface)
        {
            for relative in 0..bucket.count {
                let at = self.header.patch_offset + (bucket.first + relative) * PATCH_BYTES;
                let first_rule = self.u32(at + 24)?;
                let second_rule = self.u32(at + 28)?;
                self.rule_id(first_rule)?;
                if second_rule != NONE {
                    self.rule_id(second_rule)?;
                }
                let mut path = vec![self.rule_property(first_rule)?];
                if second_rule != NONE {
                    path.push(self.rule_property(second_rule)?);
                }
                let common = self.u8(at + 37)?;
                let intermediate = self.u32(at + 32)?;
                let candidate = MorphologyCandidate {
                    route,
                    surface: utf16_string(surface, "morphology surface")?,
                    root_seq: self.u32(at)?,
                    source_text: self.string_text(self.u32(at + 4)? as usize)?,
                    source_form: self.string_text(self.u32(at + 8)? as usize)?,
                    source_reading: self.string_text(self.u32(at + 12)? as usize)?,
                    form: self.string_text(self.u32(at + 16)? as usize)?,
                    reading: self.string_text(self.u32(at + 20)? as usize)?,
                    intermediate: (intermediate != NONE)
                        .then(|| self.string_text(intermediate as usize))
                        .transpose()?,
                    rule_ids: if second_rule == NONE {
                        vec![first_rule]
                    } else {
                        vec![first_rule, second_rule]
                    },
                    path,
                    ord: self.u8(at + 36)?,
                    common: (common != 0xff).then_some(common),
                    compatibility: "manual",
                };
                let key = canonical_key(&candidate)?;
                if !seen.contains(&key) {
                    seen.push(key);
                    candidates.push(candidate);
                }
            }
        }
        candidates.sort_by_cached_key(|value| canonical_key(value).unwrap_or_default());
        Ok(candidates)
    }

    fn find_root_key(&self, route: Route, pos_id: u16, text: &[u16]) -> Result<Option<usize>> {
        let mask = self.header.root_hash_slots - 1;
        let mut slot = hash_root_key(route, pos_id, text) as usize & mask;
        for _ in 0..self.header.root_hash_slots {
            let entry = self.u32(self.header.root_hash_offset + slot * 4)? as usize;
            if entry == 0 {
                return Ok(None);
            }
            let index = entry - 1;
            let at = self.header.root_key_offset + index * ROOT_KEY_BYTES;
            if self.u16(at + 12)? == pos_id
                && self.u8(at + 14)? == route.code()
                && self.string_equals(self.u32(at)? as usize, text)?
            {
                return Ok(Some(index));
            }
            slot = (slot + 1) & mask;
        }
        Err(KernelError::new(
            ErrorCode::CorruptPayload,
            "morphology root hash probe exhausted",
        ))
    }

    fn root_has_form(&self, group: usize, surface: &[u16]) -> Result<bool> {
        if group >= self.header.root_groups {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "morphology root record references a missing group",
            ));
        }
        let at = self.header.root_group_offset + group * ROOT_GROUP_BYTES;
        let first = self.u32(at + 4)? as usize;
        let count = self.u32(at + 8)? as usize;
        for index in 0..count {
            let id = self.u32(self.header.root_form_offset + (first + index) * ROOT_FORM_BYTES)?
                as usize;
            if self.string_equals(id, surface)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn apply_rule(&self, word: &[u16], rule: u32) -> Result<Vec<u16>> {
        let at = self.rule_offset(rule)?;
        let stem = self.u8(at + 5)? as usize;
        let tail = &word[word.len().saturating_sub(2)..];
        let kana = tail.iter().all(|unit| is_kana_unit(*unit));
        let euphony = self.string_units(self.u32(at + if kana { 12 } else { 16 })? as usize)?;
        let removed = stem + usize::from(!euphony.is_empty());
        if removed > word.len() {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "morphology rule removes more units than its word",
            ));
        }
        let mut result = word[..word.len() - removed].to_vec();
        result.extend_from_slice(&euphony);
        result.extend_from_slice(&self.string_units(self.u32(at + 8)? as usize)?);
        Ok(result)
    }

    fn rule_pos(&self, rule: u32) -> Result<u16> {
        self.u16(self.rule_offset(rule)?)
    }

    fn rule_property(&self, rule: u32) -> Result<MorphologyProperty> {
        let at = self.rule_offset(rule)?;
        let flags = self.u8(at + 3)?;
        Ok(MorphologyProperty {
            pos: self.position(self.u16(at)? as usize)?.to_owned(),
            kind: self.u8(at + 2)?,
            negative: tri(flags & 3)?,
            formal: tri((flags >> 2) & 3)?,
            ordinal: self.u8(at + 4)?,
        })
    }

    fn is_tombstone(
        &self,
        route: Route,
        surface: &[u16],
        root_seq: u32,
        first_rule: u32,
        second_rule: u32,
    ) -> bool {
        self.tombstones.iter().any(|value| {
            value.route == route
                && value.surface == surface
                && value.root_seq == root_seq
                && value.first_rule == first_rule
                && value.second_rule == second_rule
        })
    }

    fn string_units(&self, id: usize) -> Result<Vec<u16>> {
        self.string_id(id)?;
        if let Some(value) = self.strings.borrow()[id].clone() {
            return Ok(value);
        }
        let start = self.u32(self.header.string_dir_offset + id * 4)? as usize;
        let end = self.u32(self.header.string_dir_offset + (id + 1) * 4)? as usize;
        let mut value = Vec::with_capacity(end - start);
        for index in start..end {
            value.push(self.u16(self.header.string_pool_offset + index * 2)?);
        }
        self.strings.borrow_mut()[id] = Some(value.clone());
        Ok(value)
    }

    fn string_text(&self, id: usize) -> Result<String> {
        utf16_string(&self.string_units(id)?, "morphology string")
    }

    fn string_equals(&self, id: usize, value: &[u16]) -> Result<bool> {
        self.string_id(id)?;
        let start = self.u32(self.header.string_dir_offset + id * 4)? as usize;
        let end = self.u32(self.header.string_dir_offset + (id + 1) * 4)? as usize;
        if end - start != value.len() {
            return Ok(false);
        }
        for (offset, expected) in value.iter().enumerate() {
            if self.u16(self.header.string_pool_offset + (start + offset) * 2)? != *expected {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn string_id(&self, id: usize) -> Result<usize> {
        if id >= self.header.strings {
            return Err(corrupt(format!("invalid morphology string ID {id}")));
        }
        Ok(id)
    }

    fn rule_id(&self, id: u32) -> Result<u32> {
        if id as usize >= self.header.rules {
            return Err(corrupt(format!("invalid morphology rule ID {id}")));
        }
        Ok(id)
    }

    fn rule_offset(&self, id: u32) -> Result<usize> {
        Ok(self.header.rule_offset + self.rule_id(id)? as usize * RULE_BYTES)
    }

    fn u8(&self, offset: usize) -> Result<u8> {
        self.bytes.get(offset).copied().ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptPayload, "morphology byte is truncated")
        })
    }

    fn u16(&self, offset: usize) -> Result<u16> {
        u16_at(
            &self.bytes,
            offset,
            ErrorCode::CorruptPayload,
            "morphology field",
        )
    }

    fn u32(&self, offset: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            offset,
            ErrorCode::CorruptPayload,
            "morphology field",
        )
    }
}

fn corrupt(message: impl Into<String>) -> KernelError {
    KernelError::new(ErrorCode::CorruptPayload, message)
}

fn tri(code: u8) -> Result<Option<bool>> {
    match code {
        0 => Ok(Some(false)),
        1 => Ok(Some(true)),
        2 => Ok(None),
        _ => Err(KernelError::new(
            ErrorCode::CorruptPayload,
            format!("invalid morphology tri-state {code}"),
        )),
    }
}

fn hash_root_key(route: Route, pos: u16, text: &[u16]) -> u32 {
    let mut hash = 0x811c_9dc5_u32;
    for byte in [route.code(), pos as u8, (pos >> 8) as u8]
        .into_iter()
        .chain(
            text.iter()
                .flat_map(|unit| [*unit as u8, (*unit >> 8) as u8]),
        )
    {
        hash = (hash ^ u32::from(byte)).wrapping_mul(0x0100_0193);
    }
    hash
}

fn is_kana_unit(unit: u16) -> bool {
    (0x30a1..=0x30fa).contains(&unit)
        || unit == 0x30fd
        || unit == 0x30fe
        || unit == 0x30fc
        || (0x3041..=0x3094).contains(&unit)
        || unit == 0x309d
        || unit == 0x309e
}

fn canonical_key(candidate: &MorphologyCandidate) -> Result<Vec<u16>> {
    let path: Vec<_> = candidate
        .path
        .iter()
        .map(|value| {
            serde_json::json!([
                value.pos,
                value.kind,
                value.negative,
                value.formal,
                value.ordinal
            ])
        })
        .collect();
    let value = serde_json::to_string(&serde_json::json!([
        candidate.route,
        candidate.surface,
        candidate.root_seq,
        candidate.source_form,
        candidate.source_reading,
        path,
    ]))
    .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))?;
    Ok(value.encode_utf16().collect())
}
