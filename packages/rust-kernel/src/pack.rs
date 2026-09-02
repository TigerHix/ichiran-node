use std::collections::BTreeMap;
use std::sync::Arc;

use serde::Serialize;

use crate::binary::{ByteSlice, align, assert_zero, checked_range, crc32, magic, u16_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};

const MAGIC: &[u8; 8] = b"ICHIPACK";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 32;
const DIRECTORY_ENTRY_BYTES: usize = 24;
const ALIGNMENT: usize = 8;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PackSection {
    pub id: u32,
    pub offset: u32,
    pub byte_length: u32,
    pub checksum: u32,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PackManifest {
    pub format_version: u16,
    pub byte_length: usize,
    pub sections: Vec<PackSection>,
}

pub struct Pack {
    bytes: Arc<Vec<u8>>,
    sections: BTreeMap<u32, PackSection>,
    manifest: PackManifest,
}

impl Pack {
    pub fn open(input: Vec<u8>) -> Result<Self> {
        let bytes = Arc::new(input);
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete ICHIPACK header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "pack version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported pack format version {version}"),
            ));
        }
        let header_bytes = u16_at(&bytes, 10, ErrorCode::InvalidHeader, "header size")?;
        let flags = u32_at(&bytes, 12, ErrorCode::InvalidHeader, "header flags")?;
        let section_count =
            u32_at(&bytes, 16, ErrorCode::InvalidDirectory, "section count")? as usize;
        let directory_bytes =
            u32_at(&bytes, 20, ErrorCode::InvalidDirectory, "directory size")? as usize;
        let total_bytes = u32_at(&bytes, 24, ErrorCode::InvalidHeader, "pack size")? as usize;
        if header_bytes as usize != HEADER_BYTES || flags != 0 || total_bytes != bytes.len() {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "pack header size, flags, or total byte length is invalid",
            ));
        }
        let expected_directory = section_count
            .checked_mul(DIRECTORY_ENTRY_BYTES)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidDirectory, "directory length overflows")
            })?;
        if directory_bytes != expected_directory {
            return Err(KernelError::new(
                ErrorCode::InvalidDirectory,
                "section count does not match directory size",
            ));
        }
        let directory_end = HEADER_BYTES.checked_add(directory_bytes).ok_or_else(|| {
            KernelError::new(ErrorCode::InvalidDirectory, "directory end overflows")
        })?;
        let directory = checked_range(
            &bytes,
            HEADER_BYTES,
            directory_bytes,
            ErrorCode::InvalidDirectory,
            "section directory",
        )?;
        let expected_checksum = u32_at(
            &bytes,
            28,
            ErrorCode::InvalidDirectory,
            "directory checksum",
        )?;
        if crc32(directory) != expected_checksum {
            return Err(KernelError::new(
                ErrorCode::InvalidDirectory,
                "section directory checksum does not match",
            ));
        }

        let mut expected_offset = align(directory_end, ALIGNMENT)?;
        assert_zero(
            &bytes,
            directory_end,
            expected_offset,
            ErrorCode::InvalidDirectory,
            "directory padding",
        )?;
        let mut prior_id = 0;
        let mut sections = BTreeMap::new();
        let mut manifest_sections = Vec::with_capacity(section_count);
        for index in 0..section_count {
            let entry = HEADER_BYTES + index * DIRECTORY_ENTRY_BYTES;
            let id = u32_at(&bytes, entry, ErrorCode::InvalidDirectory, "section id")?;
            let offset = u32_at(
                &bytes,
                entry + 4,
                ErrorCode::InvalidDirectory,
                "section offset",
            )?;
            let byte_length = u32_at(
                &bytes,
                entry + 8,
                ErrorCode::InvalidDirectory,
                "section length",
            )?;
            let checksum = u32_at(
                &bytes,
                entry + 12,
                ErrorCode::InvalidDirectory,
                "section checksum",
            )?;
            let reserved0 = u32_at(
                &bytes,
                entry + 16,
                ErrorCode::InvalidDirectory,
                "reserved field",
            )?;
            let reserved1 = u32_at(
                &bytes,
                entry + 20,
                ErrorCode::InvalidDirectory,
                "reserved field",
            )?;
            if id == 0 || id <= prior_id || reserved0 != 0 || reserved1 != 0 {
                return Err(KernelError::new(
                    ErrorCode::InvalidDirectory,
                    "section IDs or reserved directory fields are invalid",
                ));
            }
            if offset as usize != expected_offset {
                return Err(KernelError::new(
                    ErrorCode::InvalidDirectory,
                    format!("section {id} has a non-canonical offset"),
                ));
            }
            let end = expected_offset
                .checked_add(byte_length as usize)
                .ok_or_else(|| {
                    KernelError::new(ErrorCode::InvalidDirectory, "section end overflows")
                })?;
            checked_range(
                &bytes,
                expected_offset,
                byte_length as usize,
                ErrorCode::InvalidDirectory,
                "section payload",
            )?;
            let next = align(end, ALIGNMENT)?;
            assert_zero(
                &bytes,
                end,
                next,
                ErrorCode::InvalidDirectory,
                "section padding",
            )?;
            let section = PackSection {
                id,
                offset,
                byte_length,
                checksum,
            };
            sections.insert(id, section.clone());
            manifest_sections.push(section);
            prior_id = id;
            expected_offset = next;
        }
        if expected_offset != bytes.len() {
            return Err(KernelError::new(
                ErrorCode::InvalidDirectory,
                "pack has trailing bytes outside its sections",
            ));
        }
        Ok(Self {
            bytes,
            sections,
            manifest: PackManifest {
                format_version: version,
                byte_length: total_bytes,
                sections: manifest_sections,
            },
        })
    }

    pub fn manifest(&self) -> &PackManifest {
        &self.manifest
    }

    pub fn section(&self, id: u32) -> Result<&[u8]> {
        let section = self.sections.get(&id).ok_or_else(|| {
            KernelError::new(
                ErrorCode::MissingSection,
                format!("pack has no section {id}"),
            )
        })?;
        let bytes = checked_range(
            &self.bytes,
            section.offset as usize,
            section.byte_length as usize,
            ErrorCode::InvalidDirectory,
            "section",
        )?;
        if crc32(bytes) != section.checksum {
            return Err(KernelError::new(
                ErrorCode::CorruptSection,
                format!("section {id} checksum does not match"),
            ));
        }
        Ok(bytes)
    }

    pub(crate) fn section_data(&self, id: u32) -> Result<ByteSlice> {
        let section = self.sections.get(&id).ok_or_else(|| {
            KernelError::new(
                ErrorCode::MissingSection,
                format!("pack has no section {id}"),
            )
        })?;
        let bytes = self.section(id)?;
        debug_assert_eq!(bytes.len(), section.byte_length as usize);
        ByteSlice::new(
            Arc::clone(&self.bytes),
            section.offset as usize,
            section.byte_length as usize,
        )
    }

    pub fn verify_all(&self) -> Result<()> {
        for section in &self.manifest.sections {
            self.section(section.id)?;
        }
        Ok(())
    }
}
