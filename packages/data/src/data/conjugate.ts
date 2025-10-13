/**
 * Conjugation generation system
 * Ported from ~/ichiran/dict-load.lisp lines 316-480
 */

import { getConnection } from '@ichiran/core';
import {
  getPosIndex,
  getPosByIndex,
  getConjRules,
  constructConjugation,
  DO_NOT_CONJUGATE,
  DO_NOT_CONJUGATE_SEQ,
  POS_WITH_CONJ_RULES,
  SECONDARY_CONJUGATION_TYPES_FROM,
  SECONDARY_CONJUGATION_TYPES
} from './conj-rules.js';
import { createEntrySeqGenerator, resetEntrySeqGenerator } from './schema.js';

// Parallel batch size for conjugation processing
const CONJUGATION_PARALLELISM = 10;

// PostgreSQL has ~32K parameter limit; 1000 rows × ~10 columns = ~10K params (safe margin)
const BATCH_INSERT_CHUNK_SIZE = 1000;

/**
 * Normalize a string to NFKC for consistent comparison
 * Mirrors Lisp's Unicode normalization behavior
 */
function normalizeNFKC(text: string): string {
  return text.normalize('NFKC').trim();
}

// Note: DB-level deduplication uses SQL INTERSECT queries (dict-load.lisp:387-408)
// Cache still uses signature-based keys for fast in-memory lookups

/**
 * In-memory cache of existing entry readings for fast duplicate detection
 * Maps "kanji1,kanji2|kana1,kana2" -> seq number
 */
class ReadingsCache {
  private cache = new Map<string, number>();

  /**
   * Builds cache key from kanji and kana readings
   * Uses NFKC normalization for consistency
   */
  private static makeKey(kanjiReadings: string[], kanaReadings: string[]): string {
    const kanjiPart = kanjiReadings.map(normalizeNFKC).sort().join(',');
    const kanaPart = kanaReadings.map(normalizeNFKC).sort().join(',');
    return `${kanjiPart}|${kanaPart}`;
  }

  /**
   * Pre-loads all existing entries into cache
   */
  async initialize(): Promise<void> {
    const sql = getConnection();
    console.log('Building readings cache for fast duplicate detection...');

    const startTime = Date.now();

    // Get all entries with their readings grouped
    const entries = await sql<{ seq: number; kanjiReadings: string[]; kanaReadings: string[] }[]>`
      SELECT
        e.seq,
        COALESCE(array_agg(DISTINCT k.text ORDER BY k.text) FILTER (WHERE k.text IS NOT NULL), '{}') as kanji_readings,
        array_agg(DISTINCT r.text ORDER BY r.text) as kana_readings
      FROM entry e
      INNER JOIN kana_text r ON e.seq = r.seq
      LEFT JOIN kanji_text k ON e.seq = k.seq
      GROUP BY e.seq
    `;

    for (const entry of entries) {
      const key = ReadingsCache.makeKey(entry.kanjiReadings, entry.kanaReadings);
      // If duplicate keys exist, keep the lowest seq (matches Lisp sort behavior)
      if (!this.cache.has(key) || entry.seq < this.cache.get(key)!) {
        this.cache.set(key, entry.seq);
      }
    }

    const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
    console.log(`✓ Cached ${this.cache.size.toLocaleString()} unique reading patterns in ${elapsed}s`);
  }

  /**
   * Finds existing entry with these exact readings (O(1) lookup)
   * Returns seq if found, null otherwise
   */
  findExisting(kanjiReadings: string[], kanaReadings: string[]): number | null {
    const key = ReadingsCache.makeKey(kanjiReadings, kanaReadings);
    return this.cache.get(key) ?? null;
  }

  /**
   * Adds new entry to cache
   */
  add(seq: number, kanjiReadings: string[], kanaReadings: string[]): void {
    const key = ReadingsCache.makeKey(kanjiReadings, kanaReadings);
    this.cache.set(key, seq);
  }
}

/**
 * Gets the next available sequence number from PostgreSQL sequence
 * Thread-safe for parallel conjugation generation
 */
export async function getNextSeq(): Promise<number> {
  const sql = getConnection();
  const result = await sql<{ nextval: number }[]>`
    SELECT nextval('entry_seq_generator') as nextval
  `;
  return result[0].nextval;
}

/**
 * Allocates a batch of sequence numbers at once (concurrency-safe)
 * Returns an array of actual sequence values reserved for this batch
 */
export async function allocateSeqBatch(count: number): Promise<number[]> {
  if (count <= 0) return [];
  const sql = getConnection();

  // Get actual nextval() for each sequence - these are properly reserved
  const result = await sql<{ seq: number }[]>`
    SELECT nextval('entry_seq_generator') as seq
    FROM generate_series(1, ${count})
  `;

  return result.map(r => r.seq);
}

/**
 * Gets all readings (kanji + kana) for an entry
 * Ported from dict-errata.lisp:257-261 get-all-readings
 * Returns NFKC-normalized readings for consistent comparison
 */
export async function getAllReadings(seq: number): Promise<string[]> {
  const sql = getConnection();
  const results = await sql<{ text: string }[]>`
    SELECT text FROM kanji_text WHERE seq = ${seq}
    UNION
    SELECT text FROM kana_text WHERE seq = ${seq}
  `;
  return results.map(r => normalizeNFKC(r.text));
}

/**
 * Conjugated reading item from conjugation matrix
 */
interface ConjugatedReading {
  text: string;         // Conjugated form
  kanjiFlag: boolean;   // true if kanji, false if kana
  sourceText: string;   // Original reading
  ord: number;          // Order in original entry
  onum: number;         // Order number from conjugation rule
}

/**
 * Conjugation matrix: (pos_id, conj_id) -> 2x2 array [neg][fml]
 */
type ConjugationMatrix = Map<string, ConjugatedReading[][][]>;

/**
 * Generates conjugation matrix for a single entry
 * Ported from dict-load.lisp:316-340 conjugate-entry-inner
 *
 * @param seq - Entry sequence number
 * @param options - Optional filters
 * @returns Matrix of conjugated readings keyed by (pos_id, conj_id)
 */
export async function conjugateEntryInner(
  seq: number,
  options: {
    conjTypes?: number[];  // Filter to specific conjugation types
    asPosi?: string[];     // Override POS tags (for secondary conjugations)
  } = {}
): Promise<ConjugationMatrix> {
  const { conjTypes, asPosi } = options;
  const sql = getConnection();

  // Get POS tags for this entry
  let posi: string[];
  if (asPosi) {
    posi = asPosi;
  } else {
    const posResults = await sql<{ text: string }[]>`
      SELECT DISTINCT text FROM sense_prop
      WHERE tag = 'pos' AND seq = ${seq}
    `;
    posi = posResults.map(r => r.text);
  }

  const conjMatrix: ConjugationMatrix = new Map();

  // Load restricted readings (Lisp: only allowed kanji↔kana pairs)
  const restrictedPairs = await sql<{ reading: string; text: string }[]>`
    SELECT reading, text FROM restricted_readings
    WHERE seq = ${seq}
  `;
  const hasRestrictions = restrictedPairs.length > 0;

  for (const pos of posi) {
    const posId = getPosIndex(pos);
    if (!posId) continue;

    const rules = getConjRules(posId);
    if (!rules.length || DO_NOT_CONJUGATE.includes(pos)) {
      continue;
    }

    // Get conjugatable readings for this entry
    const readings = await sql<{ text: string; ord: number; kanjiFlag: boolean }[]>`
      SELECT text, ord, true as kanji_flag FROM kanji_text
      WHERE seq = ${seq} AND conjugate_p = true
      UNION ALL
      SELECT text, ord, false as kanji_flag FROM kana_text
      WHERE seq = ${seq} AND conjugate_p = true
    `;

    for (const reading of readings) {
      for (const rule of rules) {
        const conjId = rule.conj;

        // Filter by conjugation type if specified
        if (conjTypes && !conjTypes.includes(conjId)) {
          continue;
        }

        const key = `${posId},${conjId}`;
        const conjText = constructConjugation(reading.text, rule);

        // Apply restricted readings filter (Lisp: only allowed kanji↔kana pairs)
        // If entry has restrictions and this is a kanji reading, check if it pairs with any allowed kana
        if (hasRestrictions && reading.kanjiFlag) {
          // Check if this kanji reading pairs with any allowed kana reading
          const hasAllowedPair = restrictedPairs.some(rp => rp.text === reading.text);
          if (!hasAllowedPair) {
            continue; // Skip this reading - not in allowed kanji set
          }
        }

        // Initialize 2x2 matrix if not exists
        if (!conjMatrix.has(key)) {
          conjMatrix.set(key, [
            [[], []],  // [neg=false][fml=false/true]
            [[], []]   // [neg=true][fml=false/true]
          ]);
        }

        const matrix = conjMatrix.get(key)!;
        const negIdx = rule.neg ? 1 : 0;
        const fmlIdx = rule.fml ? 1 : 0;

        matrix[negIdx][fmlIdx].push({
          text: conjText,
          kanjiFlag: reading.kanjiFlag,
          sourceText: reading.text,
          ord: reading.ord,
          onum: rule.onum
        });
      }
    }
  }

  return conjMatrix;
}

/**
 * Creates new entries for conjugated forms
 * Ported from dict-load.lisp:342-363 conjugate-entry-outer
 *
 * @param seqFrom - Source entry sequence number
 * @param options - Optional parameters
 */
export async function conjugateEntryOuter(
  seqFrom: number,
  options: {
    via?: number;        // Intermediate conjugation (for secondary conjugations)
    conjTypes?: number[]; // Filter to specific conjugation types
    asPosi?: string[];    // Override POS tags
    cache?: ReadingsCache; // Optional cache for batch operations
  } = {}
): Promise<void> {
  const { via, conjTypes, asPosi, cache } = options;

  // Use cached batch version if cache provided
  if (cache) {
    const seq = via || seqFrom;
    const conjMatrix = await conjugateEntryInner(seq, { conjTypes, asPosi });
    const originalReadings = await getAllReadings(seq);

    interface ConjToInsert {
      readings: ConjugatedReading[];
      pos: string;
      conjType: number;
      neg: boolean | null;
      fml: boolean | null;
    }
    const toInsert: ConjToInsert[] = [];

    for (const [key, matrix] of conjMatrix.entries()) {
      const [posIdStr, conjIdStr] = key.split(',');
      const posId = parseInt(posIdStr);
      const conjId = parseInt(conjIdStr);
      const pos = getPosByIndex(posId);
      if (!pos) continue;

      const ignoreNeg = !matrix[1][0].length && !matrix[1][1].length;
      const ignoreFml = !matrix[0][1].length && !matrix[1][1].length;

      for (let ii = 0; ii < 4; ii++) {
        const neg = ii >= 2;
        const fml = ii % 2 === 1;
        const negIdx = neg ? 1 : 0;
        const fmlIdx = fml ? 1 : 0;

        let readings = matrix[negIdx][fmlIdx];
        readings = readings.filter(r => !originalReadings.includes(normalizeNFKC(r.text)));

        if (readings.length === 0) continue;
        
        // Skip negative-stem (type 52) for v5r-i (ある) - Lisp alignment
        // v5r-i has irregular negative, so negative-stem should not be generated
        if (conjId === 52 && pos === 'v5r-i') continue;

        toInsert.push({
          readings,
          pos,
          conjType: conjId,
          neg: ignoreNeg ? null : neg,
          fml: ignoreFml ? null : fml
        });
      }
    }

    // Use cache-based batch processing
    const itemsWithSeq: Array<ConjToInsert & { seq: number; isNew: boolean }> = [];
    const newEntryIndices: number[] = [];
    
    for (let i = 0; i < toInsert.length; i++) {
      const conj = toInsert[i];
      const uniqueKanjiReadings = [...new Set(conj.readings.filter(r => r.kanjiFlag).map(r => r.text))];
      const uniqueKanaReadings = [...new Set(conj.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
      
      const existingSeq = cache.findExisting(uniqueKanjiReadings, uniqueKanaReadings);
      if (existingSeq !== null) {
        itemsWithSeq.push({ ...conj, seq: existingSeq, isNew: false });
      } else {
        newEntryIndices.push(i);
      }
    }
    
    const allocatedSeqs = newEntryIndices.length > 0 ? await allocateSeqBatch(newEntryIndices.length) : [];
    for (let i = 0; i < newEntryIndices.length; i++) {
      const idx = newEntryIndices[i];
      const conj = toInsert[idx];
      const assignedSeq = allocatedSeqs[i];
      
      itemsWithSeq.push({ ...conj, seq: assignedSeq, isNew: true });
    }
    
    // Insert batch (DB signature upsert will handle dedupe and count actual inserts)
    await insertConjugationsBatch(itemsWithSeq.map(item => ({
      readings: item.readings,
      options: {
        seq: item.seq,
        from: seqFrom,
        pos: item.pos,
        conjType: item.conjType,
        neg: item.neg,
        fml: item.fml,
        via: via || null
      },
      isNew: item.isNew
    })), cache);
    
    // Publish to cache AFTER successful insert (Lisp semantics: only real rows exist)
    for (const item of itemsWithSeq) {
      const uniqueKanjiReadings = [...new Set(item.readings.filter(r => r.kanjiFlag).map(r => r.text))];
      const uniqueKanaReadings = [...new Set(item.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
      cache.add(item.seq, uniqueKanjiReadings, uniqueKanaReadings);
    }
  } else {
    // Non-cached version: one-at-a-time for compatibility
    const seq = via || seqFrom;
    const conjMatrix = await conjugateEntryInner(seq, { conjTypes, asPosi });
    const originalReadings = await getAllReadings(seq);

    interface ConjToInsert {
      readings: ConjugatedReading[];
      pos: string;
      conjType: number;
      neg: boolean | null;
      fml: boolean | null;
    }
    const toInsert: ConjToInsert[] = [];

    for (const [key, matrix] of conjMatrix.entries()) {
      const [posIdStr, conjIdStr] = key.split(',');
      const posId = parseInt(posIdStr);
      const conjId = parseInt(conjIdStr);
      const pos = getPosByIndex(posId);
      if (!pos) continue;

      const ignoreNeg = !matrix[1][0].length && !matrix[1][1].length;
      const ignoreFml = !matrix[0][1].length && !matrix[1][1].length;

      for (let ii = 0; ii < 4; ii++) {
        const neg = ii >= 2;
        const fml = ii % 2 === 1;
        const negIdx = neg ? 1 : 0;
        const fmlIdx = fml ? 1 : 0;

        let readings = matrix[negIdx][fmlIdx];
        readings = readings.filter(r => !originalReadings.includes(normalizeNFKC(r.text)));

        if (readings.length === 0) continue;
        
        // Skip negative-stem (type 52) for v5r-i (ある) - Lisp alignment
        // v5r-i has irregular negative, so negative-stem should not be generated
        if (conjId === 52 && pos === 'v5r-i') continue;

        toInsert.push({
          readings,
          pos,
          conjType: conjId,
          neg: ignoreNeg ? null : neg,
          fml: ignoreFml ? null : fml
        });
      }
    }

    let nextSeq = await getNextSeq();
    for (const conj of toInsert) {
      const created = await insertConjugation(conj.readings, {
        seq: nextSeq,
        from: seqFrom,
        pos: conj.pos,
        conjType: conj.conjType,
        neg: conj.neg,
        fml: conj.fml,
        via: via || null
      });
      
      if (created) {
        nextSeq = await getNextSeq();
      }
    }
  }
}

/**
 * Lexicographic comparison helper for sorting readings
 * Ported from dict-load.lisp:365-374 lex-compare
 */
function lexCompare(a: ConjugatedReading, b: ConjugatedReading): number {
  // Sort by: ord, then onum
  if (a.ord !== b.ord) return a.ord - b.ord;
  if (a.onum !== b.onum) return a.onum - b.onum;
  return 0;
}

/**
 * Inserts a conjugation, reusing existing entries if possible
 * Ported from dict-load.lisp:376-423 insert-conjugation
 *
 * @param readings - Conjugated readings to insert
 * @param options - Conjugation metadata
 * @returns true if new entry created, false if existing entry reused
 */
export async function insertConjugation(
  readings: ConjugatedReading[],
  options: {
    seq: number;
    from: number;
    pos: string;
    conjType: number;
    neg: boolean | null;
    fml: boolean | null;
    via: number | null;
  }
): Promise<boolean> {
  const sql = getConnection();
  let { seq, from, pos, conjType, neg, fml, via } = options;

  // Sort readings lexicographically
  const sortedReadings = [...readings].sort(lexCompare);

  const sourceReadings: Array<[string, string]> = [];
  const kanjiReadings: string[] = [];
  const kanaReadings: string[] = [];

  for (const reading of sortedReadings) {
    sourceReadings.push([reading.text, reading.sourceText]);
    if (reading.kanjiFlag) {
      kanjiReadings.push(reading.text);
    } else {
      kanaReadings.push(reading.text);
    }
  }

  // Must have at least one kana reading
  if (kanaReadings.length === 0) {
    return false;
  }

  // Remove duplicates
  const uniqueKanjiReadings = [...new Set(kanjiReadings)];
  const uniqueKanaReadings = [...new Set(kanaReadings)];

  // Find existing entries with exactly these readings
  let seqCandidates: number[];

  if (uniqueKanjiReadings.length > 0) {
    // Entry has both kanji and kana - use SQL INTERSECT like Lisp (dict-load.lisp:391-401)
    const results = await sql<{ seq: number }[]>`
      SELECT seq FROM kanji_text
      WHERE text IN ${sql(uniqueKanjiReadings)}
      GROUP BY seq
      HAVING COUNT(id) = ${uniqueKanjiReadings.length}
      INTERSECT
      SELECT seq FROM kana_text
      WHERE text IN ${sql(uniqueKanaReadings)}
      GROUP BY seq
      HAVING COUNT(id) = ${uniqueKanaReadings.length}
      ORDER BY seq
    `;
    seqCandidates = results.map(r => r.seq);
  } else {
    // Entry has only kana
    const results = await sql<{ seq: number }[]>`
      SELECT r.seq
      FROM kana_text r
      LEFT JOIN kanji_text k ON r.seq = k.seq
      WHERE k.text IS NULL
        AND r.text IN ${sql(uniqueKanaReadings)}
      GROUP BY r.seq
      HAVING COUNT(r.id) = ${uniqueKanaReadings.length}
    `;
    seqCandidates = results.map(r => r.seq).sort((a, b) => a - b);
  }

  // Don't self-conjugate
  if (seqCandidates.includes(from) || (via && seqCandidates.includes(via))) {
    return false;
  }

  // Reuse existing entry or create new one
  if (seqCandidates.length > 0) {
    seq = seqCandidates[0];
  } else {
    // Create new entry (with conflict handling for parallel execution)
    await sql`
      INSERT INTO entry (seq, content, root_p, n_kanji, n_kana, primary_nokanji)
      VALUES (${seq}, '', false, 0, 0, false)
      ON CONFLICT (seq) DO NOTHING
    `;

    // Determine if this conjugation can be further conjugated
    // Only plain affirmative forms (neg=false/null, fml=false/null) can be secondarily conjugated
    const conjugateP = SECONDARY_CONJUGATION_TYPES_FROM.includes(conjType)
                       && (!neg || neg === null)
                       && (!fml || fml === null);

    // Sort readings lexicographically and deduplicate to get deterministic ord
    // (dict-load.lisp:379, 411-418)
    const kanjiReadingsOrdered: Array<{ text: string; ord: number }> = [];
    const kanaReadingsOrdered: Array<{ text: string; ord: number }> = [];
    const seenKanji = new Set<string>();
    const seenKana = new Set<string>();

    for (const reading of sortedReadings) {
      if (reading.kanjiFlag) {
        if (!seenKanji.has(reading.text)) {
          kanjiReadingsOrdered.push({ text: reading.text, ord: kanjiReadingsOrdered.length });
          seenKanji.add(reading.text);
        }
      } else {
        if (!seenKana.has(reading.text)) {
          kanaReadingsOrdered.push({ text: reading.text, ord: kanaReadingsOrdered.length });
          seenKana.add(reading.text);
        }
      }
    }

    // Batch insert kanji readings
    if (kanjiReadingsOrdered.length > 0) {
      const kanjiRows = kanjiReadingsOrdered.map(({ text, ord }) => ({
        seq,
        text,
        ord,
        common: null,
        commonTags: '',
        conjugateP,
        nokanji: false
      }));
      await sql`
        INSERT INTO kanji_text ${sql(kanjiRows, 'seq', 'text', 'ord', 'common', 'commonTags', 'conjugateP', 'nokanji')}
        ON CONFLICT (seq, text) DO NOTHING
      `;
    }

    // Batch insert kana readings
    if (kanaReadingsOrdered.length > 0) {
      const kanaRows = kanaReadingsOrdered.map(({ text, ord }) => ({
        seq,
        text,
        ord,
        common: null,
        commonTags: '',
        conjugateP,
        nokanji: false
      }));
      await sql`
        INSERT INTO kana_text ${sql(kanaRows, 'seq', 'text', 'ord', 'common', 'commonTags', 'conjugateP', 'nokanji')}
        ON CONFLICT (seq, text) DO NOTHING
      `;
    }
  }

  // Insert or get conjugation record (dict-load.lisp:420-425)
  const existingConj = via
    ? await sql<{ id: number }[]>`
        SELECT id FROM conjugation
        WHERE "from" = ${from} AND seq = ${seq} AND via = ${via}
      `
    : await sql<{ id: number }[]>`
        SELECT id FROM conjugation
        WHERE "from" = ${from} AND seq = ${seq} AND via IS NULL
      `;

  let conjId: number;
  const isNewConj = existingConj.length === 0;
  if (!isNewConj) {
    conjId = existingConj[0].id;
  } else {
    const newConj = await sql<{ id: number }[]>`
      INSERT INTO conjugation (seq, "from", via)
      VALUES (${seq}, ${from}, ${via})
      RETURNING id
    `;
    conjId = newConj[0].id;
  }

  // Insert conj_prop if not exists (dict-load.lisp:426-433)
  // Skip SELECT for new conjugations - they can't have existing props
  if (!isNewConj) {
    const existingProp = await sql<{ id: number }[]>`
      SELECT id FROM conj_prop
      WHERE conj_id = ${conjId}
        AND conj_type = ${conjType}
        AND pos = ${pos}
        AND neg IS NOT DISTINCT FROM ${neg}
        AND fml IS NOT DISTINCT FROM ${fml}
    `;

    if (existingProp.length === 0) {
      await sql`
        INSERT INTO conj_prop (conj_id, conj_type, pos, neg, fml)
        VALUES (${conjId}, ${conjType}, ${pos}, ${neg}, ${fml})
      `;
    }
  } else {
    // New conjugation - just insert
    await sql`
      INSERT INTO conj_prop (conj_id, conj_type, pos, neg, fml)
      VALUES (${conjId}, ${conjType}, ${pos}, ${neg}, ${fml})
    `;
  }

  // Insert conj_source_reading records (dict-load.lisp:428-442)
  // Only query existing if this conjugation already existed
  let newSourceReadings = sourceReadings;
  if (!isNewConj) {
    const existingSourceReadings = await sql<{ text: string; sourceText: string }[]>`
      SELECT text, source_text FROM conj_source_reading
      WHERE conj_id = ${conjId}
    `;

    const existingSet = new Set(
      existingSourceReadings.map(r => `${r.text}:${r.sourceText}`)
    );

    newSourceReadings = sourceReadings.filter(
      ([text, sourceText]) => !existingSet.has(`${text}:${sourceText}`)
    );
  }

  // Batch insert source readings
  if (newSourceReadings.length > 0) {
    await sql`
      INSERT INTO conj_source_reading (conj_id, text, source_text)
      VALUES ${sql(newSourceReadings.map(([text, sourceText]) => [conjId, text, sourceText]))}
      ON CONFLICT (conj_id, text, source_text) DO NOTHING
    `;
  }

  return seqCandidates.length === 0;
}

/**
 * Conjugates a single entry using cache for duplicate detection and batch operations
 */
async function conjugateEntryOuterWithCache(seq: number, cache: ReadingsCache): Promise<{ cacheHits: number; newEntries: number }> {
  const conjMatrix = await conjugateEntryInner(seq);
  const originalReadings = await getAllReadings(seq);

  interface ConjToInsert {
    readings: ConjugatedReading[];
    pos: string;
    conjType: number;
    neg: boolean | null;
    fml: boolean | null;
  }
  const toInsert: ConjToInsert[] = [];

  for (const [key, matrix] of conjMatrix.entries()) {
    const [posIdStr, conjIdStr] = key.split(',');
    const posId = parseInt(posIdStr);
    const conjId = parseInt(conjIdStr);
    const pos = getPosByIndex(posId);
    if (!pos) continue;

    const ignoreNeg = !matrix[1][0].length && !matrix[1][1].length;
    const ignoreFml = !matrix[0][1].length && !matrix[1][1].length;

    for (let ii = 0; ii < 4; ii++) {
      const neg = ii >= 2;
      const fml = ii % 2 === 1;
      const negIdx = neg ? 1 : 0;
      const fmlIdx = fml ? 1 : 0;

      let readings = matrix[negIdx][fmlIdx];
      readings = readings.filter(r => !originalReadings.includes(r.text));

      if (readings.length === 0) continue;
      
      // Skip negative-stem (type 52) for v5r-i (ある) - Lisp alignment
      // v5r-i has irregular negative, so negative-stem should not be generated
      if (conjId === 52 && pos === 'v5r-i') continue;

      toInsert.push({
        readings,
        pos,
        conjType: conjId,
        neg: ignoreNeg ? null : neg,
        fml: ignoreFml ? null : fml
      });
    }
  }

  // Batch check cache and allocate sequences deterministically
  const itemsWithSeq: Array<ConjToInsert & { seq: number; isNew: boolean }> = [];
  let cacheHits = 0;
  let newEntries = 0;
  
  // First pass: identify cache hits vs new entries
  const newEntryIndices: number[] = [];
  for (let i = 0; i < toInsert.length; i++) {
    const conj = toInsert[i];
    const uniqueKanjiReadings = [...new Set(conj.readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(conj.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
    
    const existingSeq = cache.findExisting(uniqueKanjiReadings, uniqueKanaReadings);
    if (existingSeq !== null) {
      // Cache hit - reuse existing seq
      itemsWithSeq.push({ ...conj, seq: existingSeq, isNew: false });
      cacheHits++;
    } else {
      // New entry needed - mark index for sequence allocation
      newEntryIndices.push(i);
    }
  }
  
  // Allocate actual sequences for new entries (concurrency-safe)
  const allocatedSeqs = newEntryIndices.length > 0 ? await allocateSeqBatch(newEntryIndices.length) : [];
  
  // Second pass: assign allocated sequences to new entries in order
  const newEntryIndexSet = new Set(newEntryIndices);
  let seqIdx = 0;
  for (let i = 0; i < toInsert.length; i++) {
    if (newEntryIndexSet.has(i)) {
      const conj = toInsert[i];
      const assignedSeq = allocatedSeqs[seqIdx];
      seqIdx++;
      
      itemsWithSeq.push({ ...conj, seq: assignedSeq, isNew: true });
      newEntries++;
    }
  }
  
  // Batch insert using deterministic sequences (DB signature upsert will handle dedupe)
  const actualNewEntries = await insertConjugationsBatch(itemsWithSeq.map(item => ({
    readings: item.readings,
    options: {
      seq: item.seq,
      from: seq,
      pos: item.pos,
      conjType: item.conjType,
      neg: item.neg,
      fml: item.fml,
      via: null
    },
    isNew: item.isNew
  })), cache);
  
  // Publish to cache AFTER successful insert (Lisp semantics: only real rows exist)
  for (const item of itemsWithSeq) {
    const uniqueKanjiReadings = [...new Set(item.readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(item.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
    cache.add(item.seq, uniqueKanjiReadings, uniqueKanaReadings);
  }

  return { cacheHits, newEntries: actualNewEntries };
}

/**
 * Loads primary conjugations for all conjugatable entries
 * Ported from dict-load.lisp:425-433 load-conjugations
 * Optimized with in-memory cache for duplicate detection
 * 
 * @param options - Optional configuration
 * @param options.limit - Maximum number of entries to process (for testing)
 */
export async function loadConjugations(options: { limit?: number } = {}): Promise<void> {
  const sql = getConnection();
  const { limit } = options;

  // Initialize or reset the sequence for conjugated entry seq numbers
  console.log('Initializing entry sequence generator...');
  await createEntrySeqGenerator(sql);
  await resetEntrySeqGenerator(sql);

  // Initialize cache
  const cache = new ReadingsCache();
  await cache.initialize();

  console.log('Finding conjugatable entries...');
  const allSeqs = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM sense_prop
    WHERE seq NOT IN ${sql(DO_NOT_CONJUGATE_SEQ)}
      AND tag = 'pos'
      AND text IN ${sql(POS_WITH_CONJ_RULES)}
  `;

  // Apply limit if specified
  const seqs = limit ? allSeqs.slice(0, limit) : allSeqs;

  console.log(`Processing ${seqs.length}${limit ? ` (limited from ${allSeqs.length})` : ''} entries...`);
  const startTime = Date.now();

  // Parallel processing is OK now - each entry's conjugations are processed atomically
  let batch: number[] = [];
  let count = 0;
  let errors = 0;
  let cacheHits = 0;
  let newEntries = 0;

  for (const { seq } of seqs) {
    batch.push(seq);

    // Process batch when it reaches the parallelism limit
    if (batch.length >= CONJUGATION_PARALLELISM) {
      const results = await Promise.allSettled(
        batch.map(async (s) => {
          const result = await conjugateEntryOuterWithCache(s, cache);
          return result;
        })
      );

      // Count successes and failures
      for (const result of results) {
        if (result.status === 'fulfilled') {
          count++;
          cacheHits += result.value.cacheHits;
          newEntries += result.value.newEntries;
        } else {
          errors++;
          console.error(`Error conjugating entry:`, result.reason);
          if (errors > 100) {
            throw new Error('Too many errors, aborting');
          }
        }
      }

      if (count % 100 === 0) {
        const elapsed = (Date.now() - startTime) / 1000;
        const rate = count / elapsed;
        const remaining = seqs.length - count;
        const eta = remaining / rate;
        console.log(`${count}/${seqs.length} entries (${rate.toFixed(1)}/sec, ${cacheHits} cache hits, ${newEntries} new) - ETA: ${Math.ceil(eta)}s`);
      }

      batch = [];
    }
  }

  // Process remaining entries in batch
  if (batch.length > 0) {
    const results = await Promise.allSettled(
      batch.map(async (s) => {
        const result = await conjugateEntryOuterWithCache(s, cache);
        return result;
      })
    );

    for (const result of results) {
      if (result.status === 'fulfilled') {
        count++;
        cacheHits += result.value.cacheHits;
        newEntries += result.value.newEntries;
      } else {
        errors++;
        console.error(`Error conjugating entry:`, result.reason);
      }
    }
  }

  const totalTime = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`✓ ${count} entries conjugated in ${totalTime}s`);
  console.log(`  Cache hits: ${cacheHits.toLocaleString()}, New entries: ${newEntries.toLocaleString()}`);

  if (errors > 0) {
    console.warn(`⚠ ${errors} entries failed to conjugate`);
  }
}

/**
 * Batch insert multiple conjugations with true batch DB operations
 * FIXED: Now creates conjugation links even when reusing existing entries (cache hits)
 */
async function insertConjugationsBatch(
  batch: Array<{
    readings: ConjugatedReading[];
    options: {
      seq: number;
      from: number;
      pos: string;
      conjType: number;
      neg: boolean | null;
      fml: boolean | null;
      via: number | null;
    };
    isNew: boolean;
  }>,
  _cache: ReadingsCache
): Promise<number> {
  const sql = getConnection();

  let actualNewEntries = 0;

  // Wrap entire batch in transaction for atomicity and FK integrity
  await sql.begin(async (tx) => {
    // DB-backed dedupe using SQL queries (matches Lisp dict-load.lisp:387-408)
    const dedupeResults: Array<{
      item: typeof batch[0];
      tentativeSeq: number;
      canonicalSeq: number;
      isNew: boolean;
    }> = [];
    
    for (const item of batch) {
      const { readings, options } = item;
      const uniqueKanjiReadings = [...new Set(readings.filter(r => r.kanjiFlag).map(r => r.text))];
      const uniqueKanaReadings = [...new Set(readings.filter(r => !r.kanjiFlag).map(r => r.text))];
      
      // Find existing entries with these readings (SQL INTERSECT)
      let seqCandidates: number[];
      
      if (uniqueKanjiReadings.length > 0) {
        const results = await tx<{ seq: number }[]>`
          SELECT seq FROM kanji_text
          WHERE text IN ${tx(uniqueKanjiReadings)}
          GROUP BY seq
          HAVING COUNT(id) = ${uniqueKanjiReadings.length}
          INTERSECT
          SELECT seq FROM kana_text
          WHERE text IN ${tx(uniqueKanaReadings)}
          GROUP BY seq
          HAVING COUNT(id) = ${uniqueKanaReadings.length}
          ORDER BY seq
        `;
        seqCandidates = results.map(r => r.seq);
      } else {
        const results = await tx<{ seq: number }[]>`
          SELECT r.seq
          FROM kana_text r
          LEFT JOIN kanji_text k ON r.seq = k.seq
          WHERE k.text IS NULL
            AND r.text IN ${tx(uniqueKanaReadings)}
          GROUP BY r.seq
          HAVING COUNT(r.id) = ${uniqueKanaReadings.length}
        `;
        seqCandidates = results.map(r => r.seq).sort((a, b) => a - b);
      }
      
      // Don't self-conjugate
      seqCandidates = seqCandidates.filter(s => s !== options.from && s !== options.via);
      
      let canonicalSeq: number;
      let isNew: boolean;
      
      if (seqCandidates.length > 0) {
        // Reuse existing entry
        canonicalSeq = seqCandidates[0];
        isNew = false;
      } else {
        // Will create new entry
        canonicalSeq = options.seq;
        isNew = true;
      }
      
      dedupeResults.push({
        item: { ...item, options: { ...options, seq: canonicalSeq } },
        tentativeSeq: options.seq,
        canonicalSeq,
        isNew
      });
    }
    
    // Separate based on actual DB state
    const toInsertNew = dedupeResults.filter(r => r.isNew).map(r => r.item);
    const toReuseExisting = dedupeResults.filter(r => !r.isNew).map(r => r.item);

    // Part 1: Create new entries with text records
    if (toInsertNew.length > 0) {
      // Batch INSERT entries and count actual inserts
      const entryValues = toInsertNew.map(({ options }) => ({
        seq: options.seq,
        content: '',
        root_p: false,
        n_kanji: 0,
        n_kana: 0,
        primary_nokanji: false,
      }));
      const insertedEntries = await tx`
        INSERT INTO entry ${tx(entryValues)}
        ON CONFLICT DO NOTHING
        RETURNING 1
      `;
      actualNewEntries = insertedEntries.length;

    // Batch INSERT text records
    const kanjiTexts: any[] = [];
    const kanaTexts: any[] = [];

    for (const { readings, options } of toInsertNew) {
      // Sort readings lexicographically by (ord, onum) to match Lisp (dict-load.lisp:379)
      const sortedReadings = [...readings].sort(lexCompare);

      // Separate and deduplicate while preserving sorted order
      const kanjiReadings: Array<{ text: string; ord: number }> = [];
      const kanaReadings: Array<{ text: string; ord: number }> = [];
      const seenKanji = new Set<string>();
      const seenKana = new Set<string>();

      for (const reading of sortedReadings) {
        if (reading.kanjiFlag) {
          if (!seenKanji.has(reading.text)) {
            kanjiReadings.push({ text: reading.text, ord: kanjiReadings.length });
            seenKanji.add(reading.text);
          }
        } else {
          if (!seenKana.has(reading.text)) {
            kanaReadings.push({ text: reading.text, ord: kanaReadings.length });
            seenKana.add(reading.text);
          }
        }
      }

      // Determine if this conjugation can be further conjugated
      // Ported from dict-load.lisp:415 - only checks conj-type
      const conjugateP = SECONDARY_CONJUGATION_TYPES_FROM.includes(options.conjType);

      // Insert with deterministic ord based on sorted position
      for (const { text, ord } of kanjiReadings) {
        kanjiTexts.push({
          seq: options.seq,
          text,
          ord,
          common: null,
          commonTags: '',
          conjugateP,
          nokanji: false
        });
      }

      for (const { text, ord } of kanaReadings) {
        kanaTexts.push({
          seq: options.seq,
          text,
          ord,
          common: null,
          commonTags: '',
          conjugateP,
          nokanji: false
        });
      }
    }

      if (kanjiTexts.length > 0) {
        await tx`INSERT INTO kanji_text ${tx(kanjiTexts)} 
          ON CONFLICT (seq, text) DO NOTHING`;
      }
      if (kanaTexts.length > 0) {
        await tx`INSERT INTO kana_text ${tx(kanaTexts)} 
          ON CONFLICT (seq, text) DO NOTHING`;
      }
    }

    // Ensure entry rows exist for reused seqs (FK integrity for conjugation links)
    if (toReuseExisting.length > 0) {
      const reuseEntryValues = toReuseExisting.map(({ options }) => ({
        seq: options.seq,
        content: '',
        root_p: false,
        n_kanji: 0,
        n_kana: 0,
        primary_nokanji: false,
      }));
      await tx`
        INSERT INTO entry ${tx(reuseEntryValues)}
        ON CONFLICT DO NOTHING
      `;
    }

    // Part 2: Create conjugation links for BOTH new entries AND reused entries
    const allConjugations = [
      ...toInsertNew.map(({ options }) => ({
        seq: options.seq,
        from: options.from,
        via: options.via,
      })),
      ...toReuseExisting.map(({ options }) => ({
        seq: options.seq,  // Use seq from caller (already resolved)
        from: options.from,
        via: options.via,
      }))
    ];

    if (allConjugations.length === 0) {
      return;
    }

    // Deduplicate within batch first (cannot have duplicates in VALUES list for DO UPDATE)
    const uniqueConjugations: typeof allConjugations = [];
    const seen = new Set<string>();
    for (const c of allConjugations) {
      const key = `${c.seq},${c.from},${c.via ?? 'null'}`;
      if (!seen.has(key)) {
        seen.add(key);
        uniqueConjugations.push(c);
      }
    }
    
    // Batch INSERT with upsert (using generated column constraint)
    const conjResults = await tx`
      INSERT INTO conjugation ${tx(uniqueConjugations)}
      ON CONFLICT ON CONSTRAINT conjugation_from_seq_via_n_unique DO UPDATE
        SET via = EXCLUDED.via
      RETURNING id, seq, "from", via
    `;

    // Create map of (seq, from, via) -> conj_id
    const conjKeyToId = new Map<string, number>();
    for (const row of conjResults) {
      const key = `${row.seq},${row.from},${row.via ?? 'null'}`;
      conjKeyToId.set(key, row.id);
    }

  // Part 3: Insert conj_prop for all conjugations
  const propValues: any[] = [];
  
  for (const { options } of toInsertNew) {
    const key = `${options.seq},${options.from},${options.via ?? 'null'}`;
    const conjId = conjKeyToId.get(key);
    if (conjId) {
      propValues.push({
        conjId,
        conjType: options.conjType,
        pos: options.pos,
        neg: options.neg,
        fml: options.fml,
      });
    }
  }

  for (const { options } of toReuseExisting) {
    const key = `${options.seq},${options.from},${options.via ?? 'null'}`;
    const conjId = conjKeyToId.get(key);
    if (conjId) {
      propValues.push({
        conjId,
        conjType: options.conjType,
        pos: options.pos,
        neg: options.neg,
        fml: options.fml,
      });
    }
  }

    if (propValues.length > 0) {
      await tx`
        INSERT INTO conj_prop ${tx(propValues)}
        ON CONFLICT ON CONSTRAINT conj_prop_unique DO NOTHING
      `;
    }

    // Part 4: Insert conj_source_reading for all conjugations
    const sourceValues: any[] = [];

    for (const { readings, options } of toInsertNew) {
      const key = `${options.seq},${options.from},${options.via ?? 'null'}`;
      const conjId = conjKeyToId.get(key);
      if (conjId) {
        for (const r of readings) {
          sourceValues.push({
            conjId,
            text: r.text,
            sourceText: r.sourceText,
          });
        }
      }
    }

    for (const { readings, options } of toReuseExisting) {
      const key = `${options.seq},${options.from},${options.via ?? 'null'}`;
      const conjId = conjKeyToId.get(key);
      if (conjId) {
        for (const r of readings) {
          sourceValues.push({
            conjId,
            text: r.text,
            sourceText: r.sourceText,
          });
        }
      }
    }

    if (sourceValues.length > 0) {
      await tx`
        INSERT INTO conj_source_reading ${tx(sourceValues)}
        ON CONFLICT ON CONSTRAINT conj_source_reading_unique DO NOTHING
      `;
    }
  }); // End transaction
  
  return actualNewEntries;
}

/**
 * Loads secondary conjugations (causative-te, potential-past, etc.)
 * Ported from dict-load.lisp:435-450 load-secondary-conjugations
 * Optimized with in-memory cache for duplicate detection
 *
 * @param options - Optional filters
 */
export async function loadSecondaryConjugations(options: {
  from?: number[];  // Limit to specific source entries
} = {}): Promise<void> {
  const sql = getConnection();
  const { from } = options;

  // Initialize cache
  const cache = new ReadingsCache();
  await cache.initialize();

  console.log('Finding secondary conjugation candidates...');

  // Build WHERE clause for optional 'from' filter
  const baseConditions = sql`
    conj_prop.conj_type IN ${sql(SECONDARY_CONJUGATION_TYPES_FROM)}
    AND conj_prop.pos NOT IN ('vs-i', 'vs-s')
    AND conj.via IS NULL
    AND (conj_prop.neg = false OR conj_prop.neg IS NULL)
    AND (conj_prop.fml = false OR conj_prop.fml IS NULL)
  `;

  const toConj = from && from.length > 0
    ? await sql<{ seqFrom: number; seq: number; conjType: number }[]>`
        SELECT DISTINCT ON (conj.from, conj.seq)
          conj.from as seq_from,
          conj.seq,
          conj_prop.conj_type
        FROM conjugation conj
        LEFT JOIN conj_prop ON conj.id = conj_prop.conj_id
        WHERE conj.from IN ${sql(from)}
          AND ${baseConditions}
      `
    : await sql<{ seqFrom: number; seq: number; conjType: number }[]>`
        SELECT DISTINCT ON (conj.from, conj.seq)
          conj.from as seq_from,
          conj.seq,
          conj_prop.conj_type
        FROM conjugation conj
        LEFT JOIN conj_prop ON conj.id = conj_prop.conj_id
        WHERE ${baseConditions}
      `;

  console.log(`Processing ${toConj.length} secondary conjugations...`);
  const startTime = Date.now();

  let count = 0;
  let errors = 0;
  let cacheHits = 0;
  let newEntries = 0;

  // Batch process for speed - collect all conjugations first,  allocate sequences, then batch insert
  interface SecondaryBatch {
    seqFrom: number;
    seq: number;
    conjType: number;
    readings: ConjugatedReading[];
    pos: string;
    neg: boolean | null;
    fml: boolean | null;
  }
  const allSecondaryConjs: SecondaryBatch[] = [];

  // First: generate all conjugation matrices
  for (const { seqFrom, seq, conjType } of toConj) {
    try {
      const asPosi = conjType === 53 ? ['v5s'] : ['v1'];
      const conjMatrix = await conjugateEntryInner(seq, {
        conjTypes: SECONDARY_CONJUGATION_TYPES,
        asPosi
      });

      const originalReadings = await getAllReadings(seq);

      for (const [key, matrix] of conjMatrix.entries()) {
        const [posIdStr, conjIdStr] = key.split(',');
        const posId = parseInt(posIdStr);
        const conjId = parseInt(conjIdStr);
        const pos = getPosByIndex(posId);
        if (!pos) continue;

        const ignoreNeg = !matrix[1][0].length && !matrix[1][1].length;
        const ignoreFml = !matrix[0][1].length && !matrix[1][1].length;

        for (let ii = 0; ii < 4; ii++) {
          const neg = ii >= 2;
          const fml = ii % 2 === 1;
          const negIdx = neg ? 1 : 0;
          const fmlIdx = fml ? 1 : 0;

          let readings = matrix[negIdx][fmlIdx];
          readings = readings.filter(r => !originalReadings.includes(normalizeNFKC(r.text)));

          if (readings.length === 0) continue;
          
          // Skip negative-stem (type 52) for v5r-i (ある) - Lisp alignment
          // v5r-i has irregular negative, so negative-stem should not be generated
          if (conjId === 52 && pos === 'v5r-i') continue;

          allSecondaryConjs.push({
            seqFrom,
            seq,
            conjType: conjId,
            readings,
            pos,
            neg: ignoreNeg ? null : neg,
            fml: ignoreFml ? null : fml
          });
        }
      }

      count++;
      if (count % 500 === 0) {
        console.log(`${count}/${toConj.length} matrices generated...`);
      }
    } catch (error) {
      errors++;
      console.error(`Error generating conjugation matrix:`, error);
    }
  }

  console.log(`Generated ${allSecondaryConjs.length} conjugations from ${count} source entries`);

  // Second: batch check cache and allocate sequences
  const conjsWithSeq: Array<SecondaryBatch & { assignedSeq: number; isNew: boolean }> = [];
  const newIndices: number[] = [];
  
  for (let i = 0; i < allSecondaryConjs.length; i++) {
    const conj = allSecondaryConjs[i];
    const uniqueKanjiReadings = [...new Set(conj.readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(conj.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
    
    const existingSeq = cache.findExisting(uniqueKanjiReadings, uniqueKanaReadings);
    if (existingSeq !== null) {
      conjsWithSeq.push({ ...conj, assignedSeq: existingSeq, isNew: false });
      cacheHits++;
    } else {
      newIndices.push(i);
    }
  }
  
  const allocatedSeqs = newIndices.length > 0 ? await allocateSeqBatch(newIndices.length) : [];
  for (let i = 0; i < newIndices.length; i++) {
    const idx = newIndices[i];
    const conj = allSecondaryConjs[idx];
    const assignedSeq = allocatedSeqs[i];
    
    const uniqueKanjiReadings = [...new Set(conj.readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(conj.readings.filter(r => !r.kanjiFlag).map(r => r.text))];
    cache.add(assignedSeq, uniqueKanjiReadings, uniqueKanaReadings);
    
    conjsWithSeq.push({ ...conj, assignedSeq, isNew: true });
    newEntries++;
  }

  console.log(`Inserting ${conjsWithSeq.length} secondary conjugations (${newEntries} new, ${cacheHits} reused)...`);

  // Third: batch insert in chunks to avoid MAX_PARAMETERS_EXCEEDED
  let actualNewEntries = 0;
  for (let i = 0; i < conjsWithSeq.length; i += BATCH_INSERT_CHUNK_SIZE) {
    const chunk = conjsWithSeq.slice(i, i + BATCH_INSERT_CHUNK_SIZE);
    const chunkNewEntries = await insertConjugationsBatch(chunk.map(conj => ({
      readings: conj.readings,
      options: {
        seq: conj.assignedSeq,
        from: conj.seqFrom,
        pos: conj.pos,
        conjType: conj.conjType,
        neg: conj.neg,
        fml: conj.fml,
        via: conj.seq
      },
      isNew: conj.isNew
    })), cache);
    actualNewEntries += chunkNewEntries;
    
    if ((i + BATCH_INSERT_CHUNK_SIZE) < conjsWithSeq.length) {
      console.log(`  ${i + chunk.length}/${conjsWithSeq.length} inserted...`);
    }
  }

  const totalTime = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`✓ ${count} secondary conjugations generated in ${totalTime}s`);
  console.log(`  Cache hits: ${cacheHits.toLocaleString()}, New entries: ${actualNewEntries.toLocaleString()}`);

  if (errors > 0) {
    console.warn(`⚠ ${errors} secondary conjugations failed`);
  }
}
