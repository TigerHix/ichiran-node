# Feasibility Analysis: In-Memory Segmentation

## Executive Summary

**FEASIBLE but complex**: It's possible to replace the 1.8GB PostgreSQL database with lighter in-memory structures for segmentation, but it would require significant refactoring and comes with trade-offs.

**Key Finding**: Most database size is NOT used for segmentation itself - it's used for scoring, conjugation lookup, and metadata. The core segmentation only needs ~100-200MB of data.

## Current Architecture

### Database Size Breakdown (1.8GB total)

```
Table                   Size    Rows      Purpose
------------------------------------------------------------------
conj_source_reading     520MB   ?         Conjugation source readings (for scoring)
kanji_text              410MB   1.6M      Kanji word forms
kana_text               287MB   1.1M      Kana word forms  
entry                   171MB   900K      Entry metadata (root_p, etc)
entry_reading_sig       117MB   ?         Deduplication signatures
conjugation             106MB   693K      Conjugation links
conj_prop               86MB    693K      Conjugation properties (POS tags)
sense_prop              55MB    ?         Sense properties (misc, field, etc)
gloss                   42MB    428K      English translations
sense                   21MB    247K      Word senses
restricted_readings     1MB     ?         Reading restrictions
```

### What's Actually Used for Segmentation

#### Phase 1: Word Lookup (findSubstringWords)
**Location**: `packages/core/src/dict/lookup.ts` lines 74-132

**Process**:
1. Generate all substrings of input (up to MAX_WORD_LENGTH=50)
2. Query: `SELECT * FROM kanji_text WHERE text IN (...)`
3. Query: `SELECT * FROM kana_text WHERE text IN (...)`
4. Return matched words with full row data

**Required Data**:
- `text` (string, avg 7 chars)
- `seq` (integer, 4 bytes)
- `ord` (integer, for ranking)
- `common` (integer, nullable - frequency score)
- `common_tags` (string - "[ichi1][news1]" etc)
- `conjugate_p` (boolean - can this conjugate?)
- `nokanji` (boolean - kana-only word?)
- `best_kana`/`best_kanji` (string, nullable - best reading)

#### Phase 2: Scoring (calcScore)
**Location**: `packages/core/src/dict/scoring.ts` lines 257-691

**Database Queries During Scoring**:
1. `entry` table - get `root_p`, `primary_nokanji` flags
2. `conjugation` table - get conjugation links (from, via)
3. `conj_prop` table - get conjugation properties (POS, type)
4. `conj_source_reading` table - get original forms
5. `sense_prop` table - check for 'uk' (usually kana) tags
6. Various POS and arch queries

#### Phase 3: Path Finding
**Location**: `packages/core/src/dict/segmentation.ts`

Uses scoring to rank different segmentation paths. No additional DB queries.

## Memory Requirements Estimation

### Minimal Approach (Segmentation Only)

**Data Needed**: Just text→seq mapping
- 2.7M entries (1.6M kanji + 1.1M kana)
- Avg text length: 7 chars = ~21 bytes UTF-8
- Seq: 4 bytes
- Total raw: 2.7M × 25 bytes = **67.5 MB**

**Trie Structure Overhead**:
- Node pointers: ~16 bytes per node
- Shared prefixes reduce redundancy
- Estimated: **150-250 MB**

### Realistic Approach (with Scoring Metadata)

**Additional fields per entry**:
- ord: 4 bytes
- common: 4 bytes + null flag
- conjugate_p: 1 byte
- nokanji: 1 byte
- best_kana/best_kanji: ~10 bytes avg
- common_tags: ~15 bytes avg

**Total per entry**: ~60 bytes
**Total**: 2.7M × 60 bytes = **162 MB** raw

**With trie structure**: **300-500 MB**

### Full Approach (with Conjugations)

Would need to include:
- Conjugation links (693K entries)
- Conjugation properties (693K entries)
- Entry metadata (900K entries)
- Sense properties for 'uk' tags

**Estimated**: **800MB - 1.2GB** in memory

## Proposed Architecture

### Option A: Pure In-Memory (Radical Redesign)

**Pros**:
- No PostgreSQL dependency
- Faster cold start
- Easier deployment
- ~200-500MB memory footprint

**Cons**:
- Complete rewrite of segmentation
- Need to reimplement scoring without SQL
- Conjugation data becomes complex nested structures
- Would need to keep conjugations in memory or query from XML/JSON

**Implementation**:
```typescript
// Trie for text lookup
class WordTrie {
  root: TrieNode;
  
  // Returns all words matching substring
  findSubstrings(text: string): Map<string, WordEntry[]>
}

interface WordEntry {
  text: string;
  seq: number;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugateP: boolean;
  nokanji: boolean;
  bestReading: string | null;
}

// Separate structures for conjugations
class ConjugationIndex {
  bySeq: Map<number, Conjugation[]>;
  props: Map<number, ConjProp[]>;
}
```

### Option B: Hybrid (Pragmatic)

Keep PostgreSQL for complex queries but optimize:

**Phase 1**: In-memory trie for initial word lookup
- Replaces findSubstringWords()
- ~200-500MB memory
- Much faster than SQL IN queries

**Phase 2**: Keep PostgreSQL for:
- Conjugation queries (complex joins)
- Scoring metadata (entry, sense_prop)
- Best readings calculation

**Pros**:
- 80% of benefit for 20% of effort
- Faster segmentation (lookup is main bottleneck)
- Can still iterate on scoring without memory concerns

**Cons**:
- Still requires PostgreSQL
- Dual storage of word forms

### Option C: SQLite In-Memory

**Replace PostgreSQL with SQLite in-memory database**:
- Load from compact binary format (~200MB)
- In-memory for performance
- Still use SQL queries (minimal code changes)
- No external PostgreSQL dependency

**Pros**:
- Minimal code changes
- No external dependencies
- ~200MB memory + SQLite overhead
- SQL queries work as-is

**Cons**:
- Still need to maintain schema
- SQLite in-memory may be slower than PostgreSQL
- Initial load time from binary format

## Data Source Analysis

### JMdict XML (59MB)

Can parse and extract:
- Entry sequence numbers
- Kanji readings (k_ele/keb)
- Kana readings (r_ele/reb)
- Priority tags (ke_pri, re_pri)
- Glosses (sense/gloss) - but not needed for segmentation

**Missing from XML**:
- Conjugations (generated by load-conjugations)
- Secondary conjugations (generated by load-secondary-conjugations)
- Best readings (calculated by best-readings)
- Common scores (calculated from priority tags)

### Build-Time Generation

We'd still need to:
1. Parse JMdict XML
2. Generate conjugations (expensive - see load-conjugations.ts)
3. Generate secondary conjugations (very expensive - see load-secondary-conjugations.ts)
4. Calculate best readings
5. Serialize to efficient binary format

**Build time remains similar**, just output format changes.

## Performance Comparison

### Current (PostgreSQL)

**Cold start**:
- PostgreSQL already running: ~0ms
- First query: ~50-200ms (table cache)

**Warm queries**:
- findSubstringWords for 10-char input: ~5-20ms
- Scoring queries: ~10-50ms per path
- Total segmentation: ~50-200ms

### Estimated (In-Memory Trie)

**Cold start**:
- Load trie from binary: ~100-500ms
- First query: ~1-5ms

**Warm queries**:
- findSubstrings for 10-char input: ~1-5ms (10-20x faster)
- Scoring queries: depends on architecture
  - Option A: ~5-10ms (in-memory lookups)
  - Option B: ~10-50ms (still uses PostgreSQL)
  - Option C: ~8-30ms (SQLite in-memory)

**Speedup**: 2-10x for typical queries

## Recommendations

### Short Term: Keep PostgreSQL
The database works well and is only 1.8GB. Unless deployment/dependency management is a critical issue, the effort to rewrite isn't justified.

### Medium Term: Hybrid Approach (Option B)
If performance is critical:
1. Build in-memory trie for word lookup (200-500MB)
2. Keep PostgreSQL for conjugations and scoring
3. Measure performance improvement
4. Gradually move more to in-memory if justified

### Long Term: Pure In-Memory (Option A)
Only if:
- Need to eliminate PostgreSQL dependency entirely
- Deployment environments are memory-constrained but can't run PostgreSQL
- Performance is absolutely critical (embedded devices, etc)
- Team has bandwidth for significant refactoring

## Implementation Estimate

### Option A (Pure In-Memory): ~3-4 weeks
- Week 1: Design and implement trie structure, load from XML
- Week 2: Reimplement conjugation lookup without SQL
- Week 3: Rewrite scoring to use in-memory structures
- Week 4: Testing, optimization, benchmarking

### Option B (Hybrid): ~1 week
- Days 1-2: Implement trie structure, load from PostgreSQL
- Days 3-4: Integrate into findSubstringWords path
- Day 5: Testing, benchmarking, optimization

### Option C (SQLite): ~1-2 weeks
- Week 1: Design binary format, SQLite loader, schema port
- Week 2: Testing, performance tuning

## Risks

1. **Complexity**: In-memory structures harder to debug than SQL
2. **Memory usage**: 500MB-1GB per process (multiplied by workers)
3. **Update latency**: Reloading trie vs PostgreSQL hot updates
4. **Query expressiveness**: Custom code vs SQL flexibility
5. **Data consistency**: Managing conjugations, best readings in memory

## Conclusion

**Yes, it's feasible** to use in-memory structures for segmentation, but:

1. The main database size (1.8GB) is dominated by conjugation data (712MB) and source readings (520MB), not base word forms
2. You could reduce memory to ~200-500MB for core segmentation
3. Scoring requires additional data (conjugations, metadata), pushing it to 800MB-1.2GB for full feature parity
4. The hybrid approach (Option B) offers best ROI: faster lookup, keep PostgreSQL for complex queries
5. Consider whether the 1.8GB database is actually a problem - modern servers handle this easily

**Recommended**: Start with hybrid approach (Option B) if performance is critical, otherwise stick with PostgreSQL.

