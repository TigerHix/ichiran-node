use std::cmp::Ordering;

use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

use super::{AnalyzerSupport, COLLISIONS, HINTS, NONE, SPLITS, SupportCollision, SupportSplitKind};
#[cfg(test)]
use super::{SPLIT_PARTS, SupportSplit, SupportSplitPart, SupportSplitWord};

impl AnalyzerSupport {
    #[cfg(test)]
    pub fn split(
        &self,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
        kind: SupportSplitKind,
    ) -> Result<Option<SupportSplit>> {
        let mut low = 0;
        let mut high = self.count(SPLITS);
        while low < high {
            let middle = low + (high - low) / 2;
            if self.compare_split_to(middle, definition_seq, route, surface, kind)?
                == Ordering::Less
            {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= self.count(SPLITS)
            || self.compare_split_to(low, definition_seq, route, surface, kind)? != Ordering::Equal
        {
            return Ok(None);
        }
        self.read_split(low).map(Some)
    }

    #[cfg(test)]
    pub fn hint(
        &self,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
        reading: &[u16],
    ) -> Result<Option<String>> {
        let mut low = 0;
        let mut high = self.count(HINTS);
        while low < high {
            let middle = low + (high - low) / 2;
            if self.compare_hint_to(middle, definition_seq, route, surface, reading)?
                == Ordering::Less
            {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= self.count(HINTS)
            || self.compare_hint_to(low, definition_seq, route, surface, reading)?
                != Ordering::Equal
        {
            return Ok(None);
        }
        let at = self.record(HINTS, low, "support hint")?;
        self.string(self.string_id(at + 12, "support hint text")?)
            .map(Some)
    }

    pub fn collision(
        &self,
        root_seq: u32,
        route: Route,
        surface: &[u16],
        rule_ids: &[u32],
    ) -> Result<Option<SupportCollision>> {
        if rule_ids.len() != 1 && rule_ids.len() != 2 {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "collision lookup requires one or two rules",
            ));
        }
        let first = rule_ids[0];
        let second = rule_ids.get(1).copied().unwrap_or(NONE);
        let mut low = 0;
        let mut high = self.count(COLLISIONS);
        while low < high {
            let middle = low + (high - low) / 2;
            if self.compare_collision_to(middle, root_seq, first, second, route, surface)?
                == Ordering::Less
            {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= self.count(COLLISIONS)
            || self.compare_collision_to(low, root_seq, first, second, route, surface)?
                != Ordering::Equal
        {
            return Ok(None);
        }
        self.read_collision(low).map(Some)
    }

    pub(super) fn compare_split_records(&self, left: usize, right: usize) -> Result<Ordering> {
        let left_at = self.record(SPLITS, left, "support split")?;
        let right_at = self.record(SPLITS, right, "support split")?;
        let sequence = self
            .u32(left_at, "support split sequence")?
            .cmp(&self.u32(right_at, "support split sequence")?);
        if sequence != Ordering::Equal {
            return Ok(sequence);
        }
        let route = self
            .u8(left_at + 29, "support split route")?
            .cmp(&self.u8(right_at + 29, "support split route")?);
        if route != Ordering::Equal {
            return Ok(route);
        }
        let left_surface = self.string_id(left_at + 4, "support split surface")?;
        let right_surface = self.string_id(right_at + 4, "support split surface")?;
        let surface = self.compare_strings(left_surface, right_surface)?;
        if surface != Ordering::Equal {
            return Ok(surface);
        }
        Ok(
            split_kind_order(self.split_kind(self.u8(left_at + 30, "support split kind")?)?).cmp(
                &split_kind_order(self.split_kind(self.u8(right_at + 30, "support split kind")?)?),
            ),
        )
    }

    pub(super) fn compare_hint_records(&self, left: usize, right: usize) -> Result<Ordering> {
        let left_at = self.record(HINTS, left, "support hint")?;
        let right_at = self.record(HINTS, right, "support hint")?;
        let sequence = self
            .u32(left_at, "support hint sequence")?
            .cmp(&self.u32(right_at, "support hint sequence")?);
        if sequence != Ordering::Equal {
            return Ok(sequence);
        }
        let route = self
            .u8(left_at + 16, "support hint route")?
            .cmp(&self.u8(right_at + 16, "support hint route")?);
        if route != Ordering::Equal {
            return Ok(route);
        }
        for offset in [4, 8] {
            let order = self.compare_strings(
                self.string_id(left_at + offset, "support hint key")?,
                self.string_id(right_at + offset, "support hint key")?,
            )?;
            if order != Ordering::Equal {
                return Ok(order);
            }
        }
        Ok(Ordering::Equal)
    }

    pub(super) fn compare_collision_records(&self, left: usize, right: usize) -> Result<Ordering> {
        let left_at = self.record(COLLISIONS, left, "support collision")?;
        let right_at = self.record(COLLISIONS, right, "support collision")?;
        for offset in [0, 12, 16] {
            let order = self
                .u32(left_at + offset, "support collision key")?
                .cmp(&self.u32(right_at + offset, "support collision key")?);
            if order != Ordering::Equal {
                return Ok(order);
            }
        }
        let route = (self.u16(left_at + 30, "support collision flags")? & 1)
            .cmp(&(self.u16(right_at + 30, "support collision flags")? & 1));
        if route != Ordering::Equal {
            return Ok(route);
        }
        self.compare_strings(
            self.string_id(left_at + 8, "support collision surface")?,
            self.string_id(right_at + 8, "support collision surface")?,
        )
    }

    #[cfg(test)]
    fn compare_split_to(
        &self,
        index: usize,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
        kind: SupportSplitKind,
    ) -> Result<Ordering> {
        let at = self.record(SPLITS, index, "support split")?;
        let sequence = self.u32(at, "support split sequence")?.cmp(&definition_seq);
        if sequence != Ordering::Equal {
            return Ok(sequence);
        }
        let current_route = self.u8(at + 29, "support split route")?;
        let route_order = current_route.cmp(&route.code());
        if route_order != Ordering::Equal {
            return Ok(route_order);
        }
        let surface_order = self
            .compare_string_to_units(self.string_id(at + 4, "support split surface")?, surface)?;
        if surface_order != Ordering::Equal {
            return Ok(surface_order);
        }
        Ok(
            split_kind_order(self.split_kind(self.u8(at + 30, "support split kind")?)?)
                .cmp(&split_kind_order(kind)),
        )
    }

    #[cfg(test)]
    fn compare_hint_to(
        &self,
        index: usize,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
        reading: &[u16],
    ) -> Result<Ordering> {
        let at = self.record(HINTS, index, "support hint")?;
        let sequence = self.u32(at, "support hint sequence")?.cmp(&definition_seq);
        if sequence != Ordering::Equal {
            return Ok(sequence);
        }
        let route_order = self.u8(at + 16, "support hint route")?.cmp(&route.code());
        if route_order != Ordering::Equal {
            return Ok(route_order);
        }
        let surface_order =
            self.compare_string_to_units(self.string_id(at + 4, "support hint surface")?, surface)?;
        if surface_order != Ordering::Equal {
            return Ok(surface_order);
        }
        self.compare_string_to_units(self.string_id(at + 8, "support hint reading")?, reading)
    }

    fn compare_collision_to(
        &self,
        index: usize,
        root_seq: u32,
        first: u32,
        second: u32,
        route: Route,
        surface: &[u16],
    ) -> Result<Ordering> {
        let at = self.record(COLLISIONS, index, "support collision")?;
        for (offset, wanted) in [(0, root_seq), (12, first), (16, second)] {
            let order = self.u32(at + offset, "support collision key")?.cmp(&wanted);
            if order != Ordering::Equal {
                return Ok(order);
            }
        }
        let route_order =
            (self.u16(at + 30, "support collision flags")? as u8 & 1).cmp(&route.code());
        if route_order != Ordering::Equal {
            return Ok(route_order);
        }
        self.compare_string_to_units(
            self.string_id(at + 8, "support collision surface")?,
            surface,
        )
    }

    #[cfg(test)]
    fn read_split(&self, index: usize) -> Result<SupportSplit> {
        let at = self.record(SPLITS, index, "support split")?;
        let first = self.u32(at + 8, "support split-part start")? as usize;
        let count = self.u16(at + 24, "support split-part count")? as usize;
        let parts = self
            .span(first, count, self.count(SPLIT_PARTS), "support split parts")?
            .map(|index| self.read_split_part(index))
            .collect::<Result<Vec<_>>>()?;
        let connector = self.u32(at + 16, "support split connector")?;
        Ok(SupportSplit {
            definition_seq: self.u32(at, "support split sequence")?,
            route: self.route(self.u8(at + 29, "support split route")?)?,
            surface: self.string(self.string_id(at + 4, "support split surface")?)?,
            kind: self.split_kind(self.u8(at + 30, "support split kind")?)?,
            parts,
            score: self.i32(at + 12, "support split score")?,
            primary: self.u8(at + 28, "support split primary")?,
            connector: if connector == NONE {
                " ".to_owned()
            } else {
                self.string(connector as usize)?
            },
            root: self.number_list(
                self.u32(at + 20, "support split-root start")? as usize,
                self.u16(at + 26, "support split-root count")? as usize,
            )?,
        })
    }

    #[cfg(test)]
    fn read_split_part(&self, index: usize) -> Result<SupportSplitPart> {
        let at = self.record(SPLIT_PARTS, index, "support split part")?;
        match self.u8(at, "support split-part kind")? {
            1 => Ok(SupportSplitPart::Score),
            2 => Ok(SupportSplitPart::Pscore),
            0 => {
                let flags = self.u8(at + 2, "support split-part flags")?;
                let common = self.u8(at + 3, "support split-part common")?;
                let best = self.u32(at + 12, "support split-part best")?;
                Ok(SupportSplitPart::Word(SupportSplitWord {
                    seq: self.u32(at + 4, "support split-part sequence")?,
                    route: self.route(self.u8(at + 1, "support split-part route")?)?,
                    text: self.string(self.string_id(at + 8, "support split-part text")?)?,
                    best: self.optional_string(best)?,
                    ord: self.u16(at + 20, "support split-part ordinal")?,
                    common: (common != 0xff).then_some(common),
                    common_tags: self
                        .string(self.string_id(at + 16, "support split-part common tags")?)?,
                    conjugatable: flags & 1 != 0,
                    nokanji: flags & 2 != 0,
                    generated: None,
                }))
            }
            kind => Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("invalid support split part {kind}"),
            )),
        }
    }

    fn read_collision(&self, index: usize) -> Result<SupportCollision> {
        let at = self.record(COLLISIONS, index, "support collision")?;
        let second = self.u32(at + 16, "support collision second rule")?;
        let flags = self.u16(at + 30, "support collision flags")?;
        let via = self.u32(at + 32, "support collision via")?;
        Ok(SupportCollision {
            root_seq: self.u32(at, "support collision root sequence")?,
            collision_seq: self.u32(at + 4, "support collision sequence")?,
            via_seq: (via != NONE).then_some(via),
            route: self.route((flags & 1) as u8)?,
            surface: self.string(self.string_id(at + 8, "support collision surface")?)?,
            rule_ids: if second == NONE {
                vec![self.u32(at + 12, "support collision first rule")?]
            } else {
                vec![self.u32(at + 12, "support collision first rule")?, second]
            },
            n_kanji: self.u16(at + 20, "support collision kanji count")?,
            n_kana: self.u16(at + 22, "support collision kana count")?,
            primary_nokanji: flags & (1 << 1) != 0,
            archived: flags & (1 << 2) != 0,
            prefer_kana: flags & (1 << 3) != 0,
            prefer_kana_on_ordinal_zero: flags & (1 << 4) != 0,
            pos: self.string_list(
                self.u32(at + 24, "support collision position start")? as usize,
                self.u16(at + 28, "support collision position count")? as usize,
            )?,
            skip_word: flags & (1 << 5) != 0,
            final_particle: flags & (1 << 6) != 0,
            semi_final_particle: flags & (1 << 7) != 0,
            non_final_particle: flags & (1 << 8) != 0,
            copula: flags & (1 << 9) != 0,
            no_kanji_break_penalty: flags & (1 << 10) != 0,
        })
    }

    fn split_kind(&self, code: u8) -> Result<SupportSplitKind> {
        match code {
            0 => Ok(SupportSplitKind::Split),
            1 => Ok(SupportSplitKind::Segsplit),
            _ => Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("invalid support split kind {code}"),
            )),
        }
    }
}

fn split_kind_order(kind: SupportSplitKind) -> u8 {
    match kind {
        SupportSplitKind::Segsplit => 0,
        SupportSplitKind::Split => 1,
    }
}
