use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::time::Instant;

const MAGIC: &[u8; 8] = b"ICHISURF";
const FORMAT_VERSION: u16 = 1;
const HEADER_BYTES: u16 = 64;
const STATE_BYTES: u16 = 8;
const EDGE_BYTES: u16 = 4;

const TERMINAL_DIRECT: u8 = 1;
const TERMINAL_MORPHOLOGY: u8 = 2;
const DIRECT_FLAG: u32 = 1 << 30;
const MORPHOLOGY_FLAG: u32 = 1 << 31;
const DIRECT_COUNT_MASK: u32 = (1 << 30) - 1;
const MAX_PACKED_TARGET: u32 = 0x00ff_ffff;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Edge {
    label: u8,
    target: u32,
}

#[derive(Debug)]
struct State {
    flags: u8,
    edge_start: u32,
    edge_len: u16,
}

#[derive(Default)]
struct PendingState {
    flags: u8,
    edges: Vec<Edge>,
}

struct Builder {
    states: Vec<State>,
    edges: Vec<Edge>,
    registry: HashMap<u64, u32>,
    path: Vec<PendingState>,
    previous: Vec<u8>,
    last_input: Vec<u8>,
    input_count: u32,
    accepted_count: u32,
    direct_count: u32,
    morphology_count: u32,
    overlap_count: u32,
    omitted_count: u32,
}

impl Builder {
    fn new() -> Self {
        Self {
            states: Vec::new(),
            edges: Vec::new(),
            registry: HashMap::new(),
            path: vec![PendingState::default()],
            previous: Vec::new(),
            last_input: Vec::new(),
            input_count: 0,
            accepted_count: 0,
            direct_count: 0,
            morphology_count: 0,
            overlap_count: 0,
            omitted_count: 0,
        }
    }

    fn add_input(&mut self, word: &[u8], flags: u8) -> Result<(), String> {
        self.input_count = checked_increment(self.input_count, "input surface count")?;

        if !self.last_input.is_empty() && word <= self.last_input.as_slice() {
            return Err(format!(
                "surface input is not strictly UTF-8 bytewise sorted near {:?}",
                String::from_utf8_lossy(word)
            ));
        }
        self.last_input.clear();
        self.last_input.extend_from_slice(word);

        if flags == 0 {
            self.omitted_count = checked_increment(self.omitted_count, "omitted surface count")?;
            return Ok(());
        }

        let common = common_prefix_len(&self.previous, word);
        self.minimize_to(common)?;

        for _ in common..word.len() {
            self.path.push(PendingState::default());
        }
        self.path[word.len()].flags = flags;
        self.previous.clear();
        self.previous.extend_from_slice(word);

        self.accepted_count = checked_increment(self.accepted_count, "accepted surface count")?;
        if flags & TERMINAL_DIRECT != 0 {
            self.direct_count = checked_increment(self.direct_count, "direct surface count")?;
        }
        if flags & TERMINAL_MORPHOLOGY != 0 {
            self.morphology_count =
                checked_increment(self.morphology_count, "morphology surface count")?;
        }
        if flags == TERMINAL_DIRECT | TERMINAL_MORPHOLOGY {
            self.overlap_count = checked_increment(self.overlap_count, "overlap surface count")?;
        }
        Ok(())
    }

    fn minimize_to(&mut self, prefix_len: usize) -> Result<(), String> {
        while self.path.len() > prefix_len + 1 {
            let child = self.path.pop().expect("path has a child");
            let target = self.intern(child)?;
            let label = self.previous[self.path.len() - 1];
            let parent = self.path.last_mut().expect("path has a parent");
            if parent.edges.last().is_some_and(|edge| edge.label >= label) {
                return Err("internal edge ordering failure".to_owned());
            }
            parent.edges.push(Edge { label, target });
        }
        Ok(())
    }

    fn intern(&mut self, pending: PendingState) -> Result<u32, String> {
        if pending.edges.len() > 256 {
            return Err("a byte automaton state cannot have more than 256 edges".to_owned());
        }

        let base_hash = signature_hash(pending.flags, &pending.edges);
        let mut probe = base_hash;
        let mut collision = 0_u64;

        loop {
            if let Some(&id) = self.registry.get(&probe) {
                if self.state_equals(id, pending.flags, &pending.edges) {
                    return Ok(id);
                }
                collision += 1;
                probe = collision_probe(base_hash, collision);
                continue;
            }

            let id = u32::try_from(self.states.len())
                .map_err(|_| "surface index has more than 2^32 states".to_owned())?;
            if id > MAX_PACKED_TARGET {
                return Err("surface index exceeds the packed 24-bit state limit".to_owned());
            }
            let edge_start = u32::try_from(self.edges.len())
                .map_err(|_| "surface index has more than 2^32 edges".to_owned())?;
            let edge_len = u16::try_from(pending.edges.len())
                .map_err(|_| "surface index state edge count does not fit u16".to_owned())?;

            self.edges.extend_from_slice(&pending.edges);
            self.states.push(State {
                flags: pending.flags,
                edge_start,
                edge_len,
            });
            self.registry.insert(probe, id);
            return Ok(id);
        }
    }

    fn state_equals(&self, id: u32, flags: u8, edges: &[Edge]) -> bool {
        let state = &self.states[id as usize];
        if state.flags != flags || usize::from(state.edge_len) != edges.len() {
            return false;
        }
        let start = state.edge_start as usize;
        self.edges[start..start + edges.len()] == *edges
    }

    fn finish(mut self) -> Result<FinishedIndex, String> {
        self.minimize_to(0)?;
        let root_pending = self.path.pop().expect("root state exists");
        let root = self.intern(root_pending)?;
        if !self.path.is_empty() {
            return Err("internal path finalization failure".to_owned());
        }
        if root as usize + 1 != self.states.len() {
            return Err("root state was unexpectedly merged".to_owned());
        }

        let mut direct_subtree_counts = Vec::with_capacity(self.states.len());
        for (id, state) in self.states.iter().enumerate() {
            let mut direct = u32::from(state.flags & TERMINAL_DIRECT != 0);
            let start = state.edge_start as usize;
            let end = start + state.edge_len as usize;
            for edge in &self.edges[start..end] {
                if edge.target as usize >= id {
                    return Err("automaton target is not in bottom-up order".to_owned());
                }
                direct = direct
                    .checked_add(direct_subtree_counts[edge.target as usize])
                    .ok_or_else(|| "direct subtree count exceeds u32".to_owned())?;
            }
            if direct > DIRECT_COUNT_MASK {
                return Err("direct subtree count exceeds the packed 30-bit limit".to_owned());
            }
            direct_subtree_counts.push(direct);
        }

        if direct_subtree_counts[root as usize] != self.direct_count {
            return Err("compiled direct count does not match terminal input".to_owned());
        }
        Ok(FinishedIndex {
            states: self.states,
            edges: self.edges,
            direct_subtree_counts,
            root,
            input_count: self.input_count,
            accepted_count: self.accepted_count,
            direct_count: self.direct_count,
            morphology_count: self.morphology_count,
            overlap_count: self.overlap_count,
            omitted_count: self.omitted_count,
        })
    }
}

struct FinishedIndex {
    states: Vec<State>,
    edges: Vec<Edge>,
    direct_subtree_counts: Vec<u32>,
    root: u32,
    input_count: u32,
    accepted_count: u32,
    direct_count: u32,
    morphology_count: u32,
    overlap_count: u32,
    omitted_count: u32,
}

impl FinishedIndex {
    fn write(&self, writer: &mut dyn Write) -> io::Result<u64> {
        let state_count = self.states.len() as u32;
        let edge_count = self.edges.len() as u32;
        let states_offset = u32::from(HEADER_BYTES);
        let edges_offset = states_offset + (state_count + 1) * u32::from(STATE_BYTES);
        let total_bytes = edges_offset + edge_count * u32::from(EDGE_BYTES);

        writer.write_all(MAGIC)?;
        write_u16(writer, FORMAT_VERSION)?;
        write_u16(writer, HEADER_BYTES)?;
        write_u32(writer, 0)?;
        write_u32(writer, state_count)?;
        write_u32(writer, edge_count)?;
        write_u32(writer, self.accepted_count)?;
        write_u32(writer, self.direct_count)?;
        write_u32(writer, self.morphology_count)?;
        write_u32(writer, self.overlap_count)?;
        write_u32(writer, self.input_count)?;
        write_u32(writer, self.root)?;
        write_u32(writer, states_offset)?;
        write_u32(writer, edges_offset)?;
        write_u32(writer, total_bytes)?;
        write_u16(writer, STATE_BYTES)?;
        write_u16(writer, EDGE_BYTES)?;

        for (state, &direct_count) in self.states.iter().zip(&self.direct_subtree_counts) {
            let first = state.edge_start;
            let mut second = direct_count;
            if state.flags & TERMINAL_DIRECT != 0 {
                second |= DIRECT_FLAG;
            }
            if state.flags & TERMINAL_MORPHOLOGY != 0 {
                second |= MORPHOLOGY_FLAG;
            }
            write_u32(writer, first)?;
            write_u32(writer, second)?;
        }
        write_u32(writer, edge_count)?;
        write_u32(writer, 0)?;

        for edge in &self.edges {
            writer.write_all(&[
                edge.label,
                edge.target as u8,
                (edge.target >> 8) as u8,
                (edge.target >> 16) as u8,
            ])?;
        }
        Ok(u64::from(total_bytes))
    }
}

fn main() {
    if let Err(message) = run() {
        eprintln!("ichiran-surface-index: {message}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let started = Instant::now();
    let (input, output) = parse_args()?;
    let reader: Box<dyn Read> = match input {
        Some(path) => Box::new(
            File::open(&path)
                .map_err(|error| format!("cannot open {}: {error}", path.display()))?,
        ),
        None => Box::new(io::stdin().lock()),
    };
    let mut builder = Builder::new();
    let mut line = Vec::new();
    let mut buffered = BufReader::with_capacity(1024 * 1024, reader);

    loop {
        line.clear();
        let bytes_read = buffered
            .read_until(b'\n', &mut line)
            .map_err(|error| format!("cannot read TSV input: {error}"))?;
        if bytes_read == 0 {
            break;
        }
        if line.last() == Some(&b'\n') {
            line.pop();
        }
        if line.last() == Some(&b'\r') {
            line.pop();
        }
        let (surface, flags) = parse_line(&line, builder.input_count + 1)?;
        builder.add_input(surface, flags)?;
    }

    let index = builder.finish()?;
    let mut writer: Box<dyn Write> = match output {
        Some(path) => Box::new(BufWriter::with_capacity(
            1024 * 1024,
            File::create(&path)
                .map_err(|error| format!("cannot create {}: {error}", path.display()))?,
        )),
        None => Box::new(BufWriter::with_capacity(1024 * 1024, io::stdout().lock())),
    };
    let bytes = index
        .write(&mut writer)
        .map_err(|error| format!("cannot write index: {error}"))?;
    writer
        .flush()
        .map_err(|error| format!("cannot flush index: {error}"))?;

    eprintln!(
        "surfaces={} accepted={} direct={} morphology={} overlap={} omitted={} states={} edges={} bytes={} elapsed_ms={}",
        index.input_count,
        index.accepted_count,
        index.direct_count,
        index.morphology_count,
        index.overlap_count,
        index.omitted_count,
        index.states.len(),
        index.edges.len(),
        bytes,
        started.elapsed().as_millis()
    );
    Ok(())
}

fn parse_args() -> Result<(Option<PathBuf>, Option<PathBuf>), String> {
    let mut input = None;
    let mut output = None;
    let mut args = env::args_os().skip(1);
    while let Some(arg) = args.next() {
        if arg == "--input" {
            input = Some(PathBuf::from(args.next().ok_or("--input requires a path")?));
        } else if arg == "--output" {
            output = Some(PathBuf::from(
                args.next().ok_or("--output requires a path")?,
            ));
        } else if arg == "--help" || arg == "-h" {
            eprintln!(
                "usage: ichiran-surface-index [--input surfaces.tsv] [--output surface-index.bin]"
            );
            std::process::exit(0);
        } else {
            return Err(format!("unknown argument {:?}", arg));
        }
    }
    Ok((input, output))
}

fn parse_line(line: &[u8], line_number: u32) -> Result<(&[u8], u8), String> {
    let mut fields = line.split(|&byte| byte == b'\t');
    let surface = fields.next().unwrap_or_default();
    let kana_direct = parse_flag(fields.next(), line_number, "kana_direct")?;
    let kana_morph = parse_flag(fields.next(), line_number, "kana_morph")?;
    let kanji_direct = parse_flag(fields.next(), line_number, "kanji_direct")?;
    let kanji_morph = parse_flag(fields.next(), line_number, "kanji_morph")?;
    if fields.next().is_some() {
        return Err(format!("line {line_number} has more than five TSV fields"));
    }
    if surface.is_empty() {
        return Err(format!("line {line_number} has an empty surface"));
    }
    let surface_text = std::str::from_utf8(surface)
        .map_err(|_| format!("line {line_number} surface is not valid UTF-8"))?;
    if surface_text.contains('\0') {
        return Err(format!("line {line_number} surface contains NUL"));
    }

    let flags = if is_kana_surface(surface_text) {
        kana_direct | (kana_morph << 1)
    } else {
        kanji_direct | (kanji_morph << 1)
    };
    Ok((surface, flags))
}

fn parse_flag(field: Option<&[u8]>, line_number: u32, name: &str) -> Result<u8, String> {
    match field {
        Some(b"0") => Ok(0),
        Some(b"1") => Ok(1),
        Some(_) => Err(format!("line {line_number} {name} must be 0 or 1")),
        None => Err(format!("line {line_number} is missing {name}")),
    }
}

fn is_kana_surface(surface: &str) -> bool {
    !surface.is_empty()
        && surface.chars().all(|character| {
            matches!(
                character as u32,
                0x30a1..=0x30fa
                    | 0x30fd
                    | 0x30fe
                    | 0x30fc
                    | 0x3041..=0x3094
                    | 0x309d
                    | 0x309e
            )
        })
}

fn common_prefix_len(left: &[u8], right: &[u8]) -> usize {
    left.iter()
        .zip(right)
        .take_while(|(left_byte, right_byte)| left_byte == right_byte)
        .count()
}

fn checked_increment(value: u32, label: &str) -> Result<u32, String> {
    value
        .checked_add(1)
        .ok_or_else(|| format!("{label} exceeds u32"))
}

fn signature_hash(flags: u8, edges: &[Edge]) -> u64 {
    let mut hasher = StableHasher::new();
    flags.hash(&mut hasher);
    edges.hash(&mut hasher);
    hasher.finish()
}

fn collision_probe(base: u64, attempt: u64) -> u64 {
    let mut value = base ^ attempt.wrapping_mul(0x9e37_79b9_7f4a_7c15);
    value ^= value >> 30;
    value = value.wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value ^= value >> 27;
    value = value.wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

struct StableHasher(u64);

impl StableHasher {
    fn new() -> Self {
        Self(0xcbf2_9ce4_8422_2325)
    }
}

impl Hasher for StableHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 ^= u64::from(byte);
            self.0 = self.0.wrapping_mul(0x0000_0100_0000_01b3);
        }
    }
}

fn write_u16(writer: &mut dyn Write, value: u16) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn write_u32(writer: &mut dyn Write, value: u32) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}
