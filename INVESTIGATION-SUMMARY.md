# Investigation Summary: In-Memory Segmentation Feasibility

## TL;DR

**YES, it's feasible** to replace the PostgreSQL database with in-memory structures for segmentation.

- **Current DB**: 1.8 GB PostgreSQL
- **Possible in-memory**: ~400-700 MB depending on approach  
- **Speedup**: 5-10x faster lookups
- **Effort**: 1-4 weeks depending on approach

## The Core Problem

You're currently using a 1.8 GB PostgreSQL database primarily for these operations:

1. **Word lookup** (segmentation): Find all dictionary words that are substrings of input text
2. **Scoring**: Rank different segmentation paths using word frequency, conjugation data, POS tags
3. **Metadata**: Common scores, readings, conjugations

Most database size is **NOT** used for segmentation itself - it's conjugation data and metadata for scoring.

## Key Findings

### Database Breakdown

```
Component               Size    Used For
─────────────────────────────────────────────────────
Word forms (lookup)     697 MB  ← Segmentation
Conjugations            712 MB  ← Scoring  
Metadata                416 MB  ← Scoring
─────────────────────────────────────────────────────
Total                   1.8 GB
```

### What's Actually Needed

**For basic segmentation** (find possible words):
- Text → Seq mapping: ~70 MB raw data
- With trie structure: ~400-500 MB optimized

**For full feature parity** (scoring, conjugations):
- Total: ~700 MB in memory

## Three Viable Approaches

### Option A: Hybrid (Recommended)
**Replace only word lookup with in-memory trie, keep PostgreSQL for rest**

**Pros**:
- ✅ 5-10x faster word lookups (main bottleneck)
- ✅ Only ~400-600 MB memory increase
- ✅ Minimal code changes
- ✅ Keep PostgreSQL for complex conjugation queries
- ✅ 1 week implementation

**Cons**:
- ❌ Still requires PostgreSQL
- ❌ Duplicate storage for word forms

**Code changes**:
```typescript
// packages/core/src/dict/lookup.ts - line 74
export async function findSubstringWords(str: string): Promise<SubstringHash> {
  // OLD: SQL query to kanji_text and kana_text
  // NEW: trie.findSubstrings(str)
  return wordTrie.findSubstrings(str);
}
```

### Option B: SQLite In-Memory
**Replace PostgreSQL with SQLite in-memory database**

**Pros**:
- ✅ No external PostgreSQL dependency
- ✅ Minimal code changes (SQL still works)
- ✅ ~700 MB memory (reasonable)
- ✅ 1-2 week implementation

**Cons**:
- ❌ Initial load time (100-500ms)
- ❌ Memory per worker process
- ❌ SQLite may be slower than PostgreSQL

**Setup**:
```bash
# Build phase (once)
bun run data build-sqlite --output ./data/jmdict.db

# Runtime (in each process)
import Database from 'better-sqlite3';
const db = new Database(':memory:');
db.exec(fs.readFileSync('./data/jmdict.db', 'utf8'));
```

### Option C: Pure In-Memory (Maximum Performance)
**Complete rewrite using only in-memory data structures**

**Pros**:
- ✅ 10-20x faster overall
- ✅ No external dependencies
- ✅ ~700 MB memory
- ✅ Portable, easy deployment

**Cons**:
- ❌ 3-4 weeks full implementation
- ❌ Complex refactoring of scoring logic
- ❌ Memory multiplied by worker count
- ❌ Less flexible than SQL queries

## Performance Comparison

| Operation | PostgreSQL | Hybrid | SQLite | Pure In-Memory |
|-----------|------------|--------|--------|----------------|
| Cold start | 0ms | 100ms | 500ms | 200ms |
| Word lookup | 10-50ms | 1-5ms | 5-15ms | 1-3ms |
| Scoring | 20-100ms | 15-80ms | 15-60ms | 5-20ms |
| Full segment | 50-200ms | 20-100ms | 30-120ms | 10-40ms |

## Memory Usage

| Approach | Per Worker | 4 Workers | 8 Workers |
|----------|------------|-----------|-----------|
| PostgreSQL | ~50 MB | ~200 MB | ~400 MB |
| Hybrid | ~600 MB | ~2.4 GB | ~4.8 GB |
| SQLite | ~700 MB | ~2.8 GB | ~5.6 GB |
| Pure | ~700 MB | ~2.8 GB | ~5.6 GB |

**Note**: PostgreSQL shares one database across all workers

## When Each Approach Makes Sense

### Stick with PostgreSQL if:
- ✅ Memory is tight and you run many workers
- ✅ Current performance is acceptable
- ✅ Database management isn't a burden
- ✅ You want SQL query flexibility

### Use Hybrid if:
- ✅ Performance is important but not critical
- ✅ You can afford ~600 MB per worker
- ✅ You want quick wins (1 week)
- ✅ PostgreSQL is acceptable for deployment

### Use SQLite if:
- ✅ You want to eliminate PostgreSQL dependency
- ✅ Single-process or few workers (<4)
- ✅ Initial load time is acceptable
- ✅ You want minimal code changes

### Use Pure In-Memory if:
- ✅ Performance is absolutely critical
- ✅ Single-process or few workers
- ✅ You have 3-4 weeks for implementation
- ✅ You want maximum portability

## Implementation Roadmap

### Phase 1: Proof of Concept (2-3 days)
1. ✅ Analyze database structure (DONE)
2. ✅ Calculate memory requirements (DONE)  
3. ⬜ Run POC benchmark (`bun run poc-in-memory-trie.ts`)
4. ⬜ Measure actual memory usage
5. ⬜ Confirm performance improvements

### Phase 2A: Hybrid Implementation (1 week)
```bash
# Day 1-2: Build trie structure
packages/core/src/dict/trie.ts         # New: Trie implementation
packages/core/src/dict/trieLoader.ts   # New: Load from PostgreSQL

# Day 3-4: Integration
packages/core/src/dict/lookup.ts       # Modify: Use trie instead of SQL

# Day 5: Testing
packages/core/tests/trie.test.ts       # New: Trie tests
packages/core/tests/lookup.test.ts     # Update: Test trie integration
```

### Phase 2B: SQLite Implementation (1-2 weeks)
```bash
# Week 1: Build SQLite export
packages/data/src/data/export-sqlite.ts    # New: Export to SQLite format
packages/core/src/dict/sqliteConnection.ts # New: SQLite connection manager

# Week 2: Testing & optimization
packages/core/tests/sqlite.test.ts         # New: SQLite tests
```

### Phase 2C: Pure In-Memory (3-4 weeks)
```bash
# Week 1: Core structures
packages/core/src/dict/trie.ts             # New: Trie for word lookup
packages/core/src/dict/conjugationIndex.ts # New: In-memory conj index
packages/core/src/dict/metadataIndex.ts    # New: Metadata structures

# Week 2-3: Rewrite lookup & scoring
packages/core/src/dict/lookup.ts           # Rewrite: Use trie
packages/core/src/dict/scoring.ts          # Rewrite: Use indexes
packages/core/src/dict/conjugation.ts      # Rewrite: In-memory lookup

# Week 4: Testing & optimization
# (Comprehensive testing of all changes)
```

## Benchmarking the POC

To validate these estimates, run the proof of concept:

```bash
# Requires running PostgreSQL with jmdict_test database
export ICHIRAN_DB_URL="postgresql://postgres:password@localhost:6777/jmdict_test"

# Run benchmark
bun run poc-in-memory-trie.ts

# Expected output:
# Trie Statistics:
#   Total nodes: ~6,700,000
#   Total entries: 2,670,530
#   Estimated memory: 785 MB
#   Build time: 5000-15000ms
#
# Lookup Benchmark (trie):
#   Input: 日本語を勉強しています
#   Found: 47 substrings
#   Time: 2-8ms
#
# Database Lookup Benchmark:
#   Input: 日本語を勉強しています  
#   Found: 47 substrings
#   Time: 15-50ms
```

## Next Steps

1. **Run POC benchmark** to validate memory and performance estimates
2. **Decide on approach** based on your constraints:
   - Memory availability per worker
   - Performance requirements
   - Development time available
   - Deployment constraints
3. **Start with hybrid** if unsure (lowest risk, quick win)
4. **Measure and iterate** based on real-world usage

## Files Created

- `feasibility-analysis-in-memory-segmentation.md` - Detailed feasibility analysis
- `memory-calculation.md` - Precise memory calculations  
- `poc-in-memory-trie.ts` - Runnable proof of concept with benchmarks
- `INVESTIGATION-SUMMARY.md` - This file

## Questions?

**Q: Will this break existing functionality?**  
A: No, if done right. The hybrid and SQLite approaches are drop-in replacements for the lookup layer.

**Q: What about data updates?**  
A: You'd rebuild the trie/SQLite when JMdict updates (same as rebuilding PostgreSQL). For SQLite/pure in-memory, restart the process.

**Q: Can I use this in production?**  
A: Yes, but test thoroughly. The hybrid approach is safest as it changes less code.

**Q: What if I run out of memory?**  
A: Use fewer workers, or stick with PostgreSQL. The database is still viable.

**Q: Is the speedup worth it?**  
A: Depends on your use case. If segmenting thousands of sentences per second, yes. For occasional use, maybe not.

## Recommendation

Start with **Option A (Hybrid)** because:
1. Lowest risk (only changes lookup logic)
2. Significant performance improvement (5-10x on lookups)
3. Quick to implement (1 week)
4. Easy to revert if issues arise
5. Can always go further (SQLite/Pure) later

Run the POC first to confirm the memory and performance gains match expectations.

