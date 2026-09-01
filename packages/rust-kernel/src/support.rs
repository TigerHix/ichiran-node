use crate::binary::{ByteSlice, align, checked_table_end, crc32, magic, u16_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};

const MAGIC: &[u8; 8] = b"IANSUP01";
const VERSION: u16 = 2;
const HEADER_BYTES: usize = 224;

const STRIDES: [usize; 16] = [12, 8, 32, 24, 8, 12, 64, 12, 4, 4, 36, 28, 20, 36, 4, 1];

pub struct AnalyzerSupport {
    bytes: ByteSlice,
    generated_rules: usize,
    generated_rule_aliases_offset: usize,
}

impl AnalyzerSupport {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete IANSUP01 header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "support version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported analyzer-support version {version}"),
            ));
        }
        if u16_at(&bytes, 10, ErrorCode::InvalidHeader, "support header size")? as usize
            != HEADER_BYTES
            || u32_at(&bytes, 12, ErrorCode::InvalidHeader, "support total size")? as usize
                != bytes.len()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support header size is invalid",
            ));
        }
        let mut header = bytes[..HEADER_BYTES].to_vec();
        header[16..20].fill(0);
        if crc32(&header)
            != u32_at(
                &bytes,
                16,
                ErrorCode::InvalidHeader,
                "support header checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support header checksum does not match",
            ));
        }
        if crc32(&bytes[HEADER_BYTES..])
            != u32_at(
                &bytes,
                20,
                ErrorCode::CorruptPayload,
                "support payload checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "analyzer-support payload checksum does not match",
            ));
        }
        let mut counts = [0_usize; 16];
        let mut offsets = [0_usize; 16];
        for index in 0..16 {
            counts[index] = u32_at(
                &bytes,
                24 + index * 4,
                ErrorCode::InvalidHeader,
                "support table count",
            )? as usize;
            offsets[index] = u32_at(
                &bytes,
                88 + index * 4,
                ErrorCode::InvalidHeader,
                "support table offset",
            )? as usize;
        }
        let generated_rules = u32_at(
            &bytes,
            152,
            ErrorCode::InvalidHeader,
            "generated rule count",
        )? as usize;
        let generated_aliases = u32_at(
            &bytes,
            156,
            ErrorCode::InvalidHeader,
            "generated alias count",
        )? as usize;
        let generated_rule_aliases_offset = u32_at(
            &bytes,
            160,
            ErrorCode::InvalidHeader,
            "generated alias offset",
        )? as usize;
        let mut expected = HEADER_BYTES;
        for index in 0..16 {
            if offsets[index] != expected {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    format!("analyzer-support table {index} has a non-canonical offset"),
                ));
            }
            let count = if index == 14 {
                counts[index].checked_add(1).ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::InvalidHeader,
                        "support string-directory count overflows",
                    )
                })?
            } else {
                counts[index]
            };
            expected = checked_table_end(
                offsets[index],
                count,
                STRIDES[index],
                bytes.len(),
                ErrorCode::InvalidHeader,
                "analyzer-support table",
            )?;
        }
        expected = align(expected, 8)?;
        if generated_rule_aliases_offset != expected {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "generated rule aliases have a non-canonical offset",
            ));
        }
        expected = checked_table_end(
            expected,
            generated_rules,
            2,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated rule aliases",
        )?;
        if align(expected, 8)? != bytes.len() || bytes[expected..].iter().any(|value| *value != 0) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support trailing bytes are invalid",
            ));
        }
        let mut previous = 0_u32;
        for index in 0..=counts[14] {
            let current = u32_at(
                &bytes,
                offsets[14] + index * 4,
                ErrorCode::CorruptPayload,
                "support string offset",
            )?;
            if current < previous || current as usize > counts[15] {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "support string directory is not monotonic",
                ));
            }
            previous = current;
        }
        if previous as usize != counts[15] {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "support string directory does not cover its pool",
            ));
        }
        for index in 0..generated_rules {
            let alias = u16_at(
                &bytes,
                generated_rule_aliases_offset + index * 2,
                ErrorCode::CorruptPayload,
                "generated alias",
            )? as usize;
            if alias >= generated_aliases {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "generated rule alias is out of range",
                ));
            }
        }
        Ok(Self {
            bytes,
            generated_rules,
            generated_rule_aliases_offset,
        })
    }

    pub fn generated_aliases(&self, rule_ids: &[u32]) -> Result<Vec<u16>> {
        if rule_ids.len() != 1 && rule_ids.len() != 2 {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "generated lookup requires one or two rules",
            ));
        }
        rule_ids
            .iter()
            .map(|id| {
                if *id as usize >= self.generated_rules {
                    return Err(KernelError::new(
                        ErrorCode::OutOfRange,
                        format!("generated rule {id} is out of range"),
                    ));
                }
                u16_at(
                    &self.bytes,
                    self.generated_rule_aliases_offset + *id as usize * 2,
                    ErrorCode::CorruptPayload,
                    "generated rule alias",
                )
            })
            .collect()
    }
}
