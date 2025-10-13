# Detailed Memory Calculation for In-Memory Segmentation

## Actual Database Statistics

```
kanji_text:  1,596,591 entries, avg 7.06 chars, max 28 chars
kana_text:   1,073,939 entries, avg 7.99 chars, max 37 chars
Total:       2,670,530 entries
```

## Memory Per Entry

### Minimal Approach (Text + Seq only)

```javascript
interface MinimalEntry {
  text: string;    // 7-8 chars avg × 3 bytes/char UTF-8 = ~22 bytes
  seq: number;     // 4 bytes (32-bit int)
}
// Total: ~26 bytes per entry
```

**Raw data**: 2,670,530 × 26 = **69.4 MB**

### Standard Approach (All Fields Needed for Scoring)

```javascript
interface StandardEntry {
  text: string;         // ~22 bytes (avg 7.5 chars UTF-8)
  seq: number;          // 4 bytes
  ord: number;          // 4 bytes
  common: number|null;  // 4 bytes + 1 byte null flag = 5 bytes
  commonTags: string;   // avg ~15 bytes ("[ichi1][news1]")
  conjugateP: boolean;  // 1 byte
  nokanji: boolean;     // 1 byte
  type: 'kanji'|'kana'; // 1 byte (enum)
}
// Total: ~53 bytes per entry
```

**Raw data**: 2,670,530 × 53 = **141.5 MB**

## Trie Structure Overhead

### Node Structure

```javascript
class TrieNode {
  children: Map<string, TrieNode>;  // ~24 bytes base Map
                                     // + ~16 bytes per child entry
  entries: Array<Entry>;             // ~24 bytes base Array
                                     // + 8 bytes per entry reference
}
```

### Calculating Node Count

For Japanese text, we need to estimate:
- Average branch factor (children per node)
- Average depth

**Observations**:
- Japanese has ~2000 common kanji + 100 kana characters
- Words share common prefixes (e.g., 食べる, 食べ, 食べ物)
- Average word length: 7.5 characters

**Conservative estimate**:
- Total unique prefixes: ~5-10% of total entries
- Approximate nodes: 2.67M entries × 7.5 chars / 3 (sharing factor) = ~6.7M nodes

**Node memory**:
```
Base node: 48 bytes (Map overhead + Array overhead)
Average children: 2.5 × 16 bytes = 40 bytes
Entry references: 1.0 × 8 bytes = 8 bytes
Total per node: ~96 bytes
```

**Total node overhead**: 6.7M × 96 = **643 MB**

### Combined Total (Trie + Data)

| Approach | Raw Data | Trie Nodes | Total Memory |
|----------|----------|------------|--------------|
| Minimal  | 69 MB    | 643 MB     | **712 MB**   |
| Standard | 142 MB   | 643 MB     | **785 MB**   |

## Optimization: Compact Trie

We can reduce memory significantly with a more efficient trie:

### Optimized Node Structure

```javascript
class CompactTrieNode {
  // Use typed arrays for better memory density
  children: Uint32Array;        // Child indices (4 bytes each)
  childChars: Uint16Array;      // Unicode code points (2 bytes each)
  entryIndices: Uint32Array;    // Indices into entry array (4 bytes each)
}
```

**Savings**:
- No Map overhead (saves ~24 bytes)
- Direct indices instead of pointers (saves ~8 bytes per child)
- Compact storage (saves ~20 bytes per node)

**Optimized node**: ~48 bytes (vs 96 bytes)

**Total with optimization**: 
- 6.7M nodes × 48 bytes = 322 MB
- Plus data: 142 MB
- **Total: 464 MB**

## Further Optimization: Prefix Compression

For very long shared prefixes, we can use:
- PATRICIA trie (compressed prefixes)
- Directed Acyclic Word Graph (DAWG)

**Expected savings**: 20-40%
**Final estimate**: **300-400 MB**

## Comparison with PostgreSQL

### Current PostgreSQL Storage

```
kanji_text table: 410 MB (includes indexes)
kana_text table:  287 MB (includes indexes)
Total:            697 MB
```

### In-Memory vs PostgreSQL

| Metric | PostgreSQL | In-Memory (Standard) | In-Memory (Optimized) |
|--------|------------|----------------------|-----------------------|
| Storage| 697 MB     | 785 MB               | 464 MB                |
| Indexes| Included   | N/A (implicit)       | N/A (implicit)        |
| Lookup | ~10-50ms   | ~1-5ms               | ~1-3ms                |
| Cold   | 0ms (always warm) | ~100-500ms (load) | ~50-200ms (load)   |

## Real-World Example

Let's trace through actual lookup for "食べ物":

### PostgreSQL Approach

```sql
-- Generate substrings: 食, 食べ, 食べ物, べ, べ物, 物
SELECT * FROM kanji_text WHERE text IN ('食', '食べ', '食べ物', 'べ', 'べ物', '物');
-- Returns: ~10-20 matching entries
-- Time: ~5-15ms
```

### Trie Approach

```
Input: "食べ物"

Position 0:
  食 -> node1 -> entries: [seq1058160, seq1354420] ✓
  食べ -> node2 -> entries: [seq10094023] ✓
  食べ物 -> node3 -> entries: [seq1358370, seq2009890] ✓

Position 1:
  べ -> node4 -> entries: [] (no match)
  べ物 -> (no path in trie)

Position 2:
  物 -> node5 -> entries: [seq1585790, seq1586050] ✓

Results: 7 entries found
Time: ~0.5-2ms (in-memory pointer chasing)
```

## Additional Data Structures Needed

Beyond the basic trie, we'd need:

### 1. Entry Table (for lookups by seq)
```javascript
Map<number, Entry> // seq -> full entry data
// ~142 MB (same as entry data in trie)
```

### 2. Conjugation Index
```javascript
Map<number, Conjugation[]> // seq -> conjugations
// 693K conjugations × ~40 bytes = ~28 MB
```

### 3. Conjugation Properties
```javascript
Map<number, ConjProp[]> // conj_id -> properties
// 693K props × ~30 bytes = ~21 MB
```

### 4. Sense Properties (for 'uk' tags)
```javascript
Map<number, SenseProp[]> // seq -> sense properties
// ~55 MB (from current database)
```

## Total Memory for Full Feature Parity

| Component | Memory |
|-----------|--------|
| Word Trie (optimized) | 464 MB |
| Entry lookup table | 142 MB |
| Conjugations | 28 MB |
| Conj properties | 21 MB |
| Sense properties | 55 MB |
| **Total** | **710 MB** |

## Comparison: Database vs In-Memory

| Metric | PostgreSQL | In-Memory |
|--------|------------|-----------|
| Total size | 1,825 MB | 710 MB |
| Segmentation data | ~697 MB | ~464 MB |
| Startup time | 0ms (external) | 100-500ms |
| Lookup speed | 10-50ms | 1-5ms |
| Memory per worker | ~50 MB | 710 MB |
| Scalability | Shared DB | Per-process |

## Key Insights

1. **Trie overhead dominates**: The trie structure (643 MB) is larger than the data itself (142 MB)
2. **Optimization matters**: Compact trie reduces memory by ~40% (643 MB → 322 MB)
3. **Total is reasonable**: ~700 MB for full feature parity vs 1.8 GB database
4. **Performance win**: 5-10x faster lookups
5. **Trade-off**: Memory per worker vs shared database

## Recommendation

For typical deployments:
- **If single-process or few workers**: In-memory is feasible (700 MB × workers)
- **If many workers (10+)**: PostgreSQL more memory-efficient (shared)
- **If performance critical**: In-memory worth the memory cost
- **If memory constrained**: Hybrid approach (trie only, PostgreSQL for rest)

## Hybrid Approach Memory

Keep only the word lookup trie in memory:

| Component | Storage |
|-----------|---------|
| Word Trie (optimized) | 464 MB |
| Entry lookup (if needed) | 142 MB |
| **Subtotal** | **606 MB** |
| Conjugations | PostgreSQL |
| Sense properties | PostgreSQL |
| **Total DB size** | ~1.1 GB |

**Best of both worlds**: Fast lookups with reasonable memory, complex queries in PostgreSQL.

