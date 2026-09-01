use serde::Serialize;

use crate::binary::{ByteSlice, checked_table_end, magic, u16_at, u24_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;
use crate::text::{next_scalar, scalar_utf8};

#[cfg(test)]
mod strict_tests;

const MAGIC: &[u8; 8] = b"ICHISURF";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 64;
const STATE_BYTES: usize = 8;
const EDGE_BYTES: usize = 4;
const DIRECT_FLAG: u32 = 0x4000_0000;
const MORPHOLOGY_FLAG: u32 = 0x8000_0000;
const COUNT_MASK: u32 = 0x3fff_ffff;
const MAX_STATE_COUNT: usize = 0x0100_0000;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SurfaceMatch {
    pub end: usize,
    pub route: Route,
    pub direct: bool,
    pub morphology: bool,
    pub direct_rank: Option<u32>,
}

pub struct SurfaceIndex {
    bytes: ByteSlice,
    state_count: usize,
    edge_count: usize,
    direct_count: u32,
    root: usize,
    states_offset: usize,
    edges_offset: usize,
}

#[derive(Clone, Copy)]
struct Walk {
    state: usize,
    direct_rank: u32,
}

impl SurfaceIndex {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete ICHISURF header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "surface version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported surface-index version {version}"),
            ));
        }
        if u16_at(&bytes, 10, ErrorCode::InvalidHeader, "surface header size")? as usize
            != HEADER_BYTES
            || u32_at(&bytes, 12, ErrorCode::InvalidHeader, "surface flags")? != 0
            || u16_at(&bytes, 60, ErrorCode::InvalidHeader, "surface state stride")? as usize
                != STATE_BYTES
            || u16_at(&bytes, 62, ErrorCode::InvalidHeader, "surface edge stride")? as usize
                != EDGE_BYTES
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "surface header sizes or flags are invalid",
            ));
        }
        let state_count =
            u32_at(&bytes, 16, ErrorCode::InvalidHeader, "surface state count")? as usize;
        let edge_count =
            u32_at(&bytes, 20, ErrorCode::InvalidHeader, "surface edge count")? as usize;
        let accepted_count = u32_at(
            &bytes,
            24,
            ErrorCode::InvalidHeader,
            "surface accepted count",
        )?;
        let direct_count = u32_at(&bytes, 28, ErrorCode::InvalidHeader, "surface direct count")?;
        let morphology_count = u32_at(
            &bytes,
            32,
            ErrorCode::InvalidHeader,
            "surface morphology count",
        )?;
        let overlap_count = u32_at(
            &bytes,
            36,
            ErrorCode::InvalidHeader,
            "surface overlap count",
        )?;
        let input_count = u32_at(&bytes, 40, ErrorCode::InvalidHeader, "surface input count")?;
        let root = u32_at(&bytes, 44, ErrorCode::InvalidHeader, "surface root")? as usize;
        let states_offset = u32_at(
            &bytes,
            48,
            ErrorCode::InvalidHeader,
            "surface states offset",
        )? as usize;
        let edges_offset =
            u32_at(&bytes, 52, ErrorCode::InvalidHeader, "surface edges offset")? as usize;
        let total_bytes = u32_at(&bytes, 56, ErrorCode::InvalidHeader, "surface size")? as usize;
        let state_table_count = state_count.checked_add(1).ok_or_else(|| {
            KernelError::new(
                ErrorCode::InvalidHeader,
                "surface state-table count overflows",
            )
        })?;
        let expected_edges = HEADER_BYTES
            .checked_add(state_table_count.checked_mul(STATE_BYTES).ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidHeader, "surface state table overflows")
            })?)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidHeader, "surface state table overflows")
            })?;
        let expected_total = expected_edges
            .checked_add(edge_count.checked_mul(EDGE_BYTES).ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidHeader, "surface edge table overflows")
            })?)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidHeader, "surface edge table overflows")
            })?;
        let expected_accepted = direct_count
            .checked_add(morphology_count)
            .and_then(|total| total.checked_sub(overlap_count))
            .ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidHeader, "surface accepted count overflows")
            })?;
        if state_count == 0
            || state_count > MAX_STATE_COUNT
            || root != state_count - 1
            || states_offset != HEADER_BYTES
            || edges_offset != expected_edges
            || total_bytes != expected_total
            || total_bytes != bytes.len()
            || overlap_count > direct_count
            || overlap_count > morphology_count
            || accepted_count != expected_accepted
            || input_count < accepted_count
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "surface offsets, counts, or byte length are invalid",
            ));
        }
        checked_table_end(
            states_offset,
            state_table_count,
            STATE_BYTES,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "surface states",
        )?;
        checked_table_end(
            edges_offset,
            edge_count,
            EDGE_BYTES,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "surface edges",
        )?;
        let index = Self {
            bytes,
            state_count,
            edge_count,
            direct_count,
            root,
            states_offset,
            edges_offset,
        };
        index.validate(accepted_count, morphology_count, overlap_count)?;
        Ok(index)
    }

    pub fn direct_count(&self) -> u32 {
        self.direct_count
    }

    pub fn lookup(&self, surface: &[u16]) -> Result<Option<SurfaceMatch>> {
        Ok(self
            .scan(surface, 0, surface.len().max(1))?
            .into_iter()
            .find(|value| value.end == surface.len()))
    }

    pub fn scan(
        &self,
        text: &[u16],
        start: usize,
        max_code_units: usize,
    ) -> Result<Vec<SurfaceMatch>> {
        if start > text.len() || max_code_units == 0 {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "surface scan range is invalid",
            ));
        }
        let mut matches = Vec::new();
        let mut walk = Walk {
            state: self.root,
            direct_rank: 0,
        };
        let mut kana = true;
        let mut offset = start;
        while offset < text.len() {
            let (scalar, width) = next_scalar(text, offset);
            if offset + width - start > max_code_units {
                break;
            }
            kana &= is_kana(scalar);
            for byte in scalar_utf8(scalar) {
                if !self.advance_byte(&mut walk, byte)? {
                    return Ok(matches);
                }
            }
            offset += width;
            let flags = self.state_word(walk.state)?;
            let direct = flags & DIRECT_FLAG != 0;
            let morphology = flags & MORPHOLOGY_FLAG != 0;
            if direct || morphology {
                matches.push(SurfaceMatch {
                    end: offset,
                    route: if kana { Route::Kana } else { Route::Kanji },
                    direct,
                    morphology,
                    direct_rank: direct.then_some(walk.direct_rank),
                });
            }
        }
        Ok(matches)
    }

    pub fn direct_surface(&self, rank: u32) -> Result<String> {
        if rank >= self.direct_count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "direct surface rank is out of range",
            ));
        }
        let mut state = self.root;
        let mut remaining = rank;
        let mut bytes = Vec::new();
        loop {
            if self.state_word(state)? & DIRECT_FLAG != 0 {
                if remaining == 0 {
                    return String::from_utf8(bytes).map_err(|_| {
                        KernelError::new(
                            ErrorCode::CorruptPayload,
                            "direct surface is not valid UTF-8",
                        )
                    });
                }
                remaining -= 1;
            }
            let start = self.first_edge(state)?;
            let end = self.first_edge(state + 1)?;
            let mut descended = false;
            for edge in start..end {
                let target = self.edge_target(edge)?;
                let count = self.state_word(target)? & COUNT_MASK;
                if remaining < count {
                    bytes.push(self.edge_label(edge)?);
                    state = target;
                    descended = true;
                    break;
                }
                remaining -= count;
            }
            if !descended {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "direct rank traversal reached no child",
                ));
            }
        }
    }

    fn validate(
        &self,
        accepted_count: u32,
        morphology_count: u32,
        overlap_count: u32,
    ) -> Result<()> {
        if self.first_edge(0)? != 0
            || self.first_edge(self.state_count)? != self.edge_count
            || self.state_word(self.state_count)? != 0
        {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "surface state sentinel is invalid",
            ));
        }
        let mut counts = vec![[0_u32; 3]; self.state_count];
        for state in 0..self.state_count {
            let start = self.first_edge(state)?;
            let end = self.first_edge(state + 1)?;
            if start > end || end > self.edge_count {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "surface state edge span is invalid",
                ));
            }
            let word = self.state_word(state)?;
            let mut direct = u32::from(word & DIRECT_FLAG != 0);
            let mut accepted = u32::from(word & (DIRECT_FLAG | MORPHOLOGY_FLAG) != 0);
            let mut morphology = u32::from(word & MORPHOLOGY_FLAG != 0);
            let mut overlap = u32::from(
                word & (DIRECT_FLAG | MORPHOLOGY_FLAG) == (DIRECT_FLAG | MORPHOLOGY_FLAG),
            );
            let mut previous = None;
            for edge in start..end {
                let label = self.edge_label(edge)?;
                let target = self.edge_target(edge)?;
                if previous.is_some_and(|value| label <= value) || target >= state {
                    return Err(KernelError::new(
                        ErrorCode::CorruptPayload,
                        "surface edges are unsorted or not bottom-up",
                    ));
                }
                previous = Some(label);
                direct = direct
                    .checked_add(self.state_word(target)? & COUNT_MASK)
                    .ok_or_else(|| {
                        KernelError::new(ErrorCode::CorruptPayload, "surface count overflows")
                    })?;
                accepted = accepted.checked_add(counts[target][0]).ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptPayload, "surface count overflows")
                })?;
                morphology = morphology.checked_add(counts[target][1]).ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptPayload, "surface count overflows")
                })?;
                overlap = overlap.checked_add(counts[target][2]).ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptPayload, "surface count overflows")
                })?;
            }
            if direct != word & COUNT_MASK {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "surface direct subtree count is invalid",
                ));
            }
            counts[state] = [accepted, morphology, overlap];
        }
        let root = counts[self.root];
        if self.state_word(self.root)? & (DIRECT_FLAG | MORPHOLOGY_FLAG) != 0
            || self.state_word(self.root)? & COUNT_MASK != self.direct_count
            || root != [accepted_count, morphology_count, overlap_count]
        {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "surface root counts are inconsistent",
            ));
        }
        Ok(())
    }

    fn advance_byte(&self, walk: &mut Walk, label: u8) -> Result<bool> {
        if self.state_word(walk.state)? & DIRECT_FLAG != 0 {
            walk.direct_rank += 1;
        }
        for edge in self.first_edge(walk.state)?..self.first_edge(walk.state + 1)? {
            let found = self.edge_label(edge)?;
            let target = self.edge_target(edge)?;
            if found < label {
                walk.direct_rank = walk
                    .direct_rank
                    .checked_add(self.state_word(target)? & COUNT_MASK)
                    .ok_or_else(|| {
                        KernelError::new(ErrorCode::CorruptPayload, "direct rank overflows")
                    })?;
            } else if found == label {
                walk.state = target;
                return Ok(true);
            } else {
                return Ok(false);
            }
        }
        Ok(false)
    }

    fn first_edge(&self, state: usize) -> Result<usize> {
        Ok(u32_at(
            &self.bytes,
            self.states_offset + state * STATE_BYTES,
            ErrorCode::CorruptPayload,
            "surface state",
        )? as usize)
    }

    fn state_word(&self, state: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            self.states_offset + state * STATE_BYTES + 4,
            ErrorCode::CorruptPayload,
            "surface state",
        )
    }

    fn edge_label(&self, edge: usize) -> Result<u8> {
        self.bytes
            .get(self.edges_offset + edge * EDGE_BYTES)
            .copied()
            .ok_or_else(|| KernelError::new(ErrorCode::CorruptPayload, "surface edge is truncated"))
    }

    fn edge_target(&self, edge: usize) -> Result<usize> {
        Ok(u24_at(
            &self.bytes,
            self.edges_offset + edge * EDGE_BYTES + 1,
            ErrorCode::CorruptPayload,
            "surface edge",
        )? as usize)
    }
}

fn is_kana(scalar: u32) -> bool {
    (0x30a1..=0x30fa).contains(&scalar)
        || scalar == 0x30fd
        || scalar == 0x30fe
        || scalar == 0x30fc
        || (0x3041..=0x3094).contains(&scalar)
        || scalar == 0x309d
        || scalar == 0x309e
}
