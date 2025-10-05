/**
 * Conjugation generation system
 * Ported from ~/ichiran/dict-load.lisp lines 316-480
 */

import { getConnection } from '../conn.js';
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

/**
 * In-memory cache of existing entry readings for fast duplicate detection
 * Maps "kanji1,kanji2|kana1,kana2" -> seq number
 */
class ReadingsCache {
  private cache = new Map<string, number>();

  /**
   * Builds cache key from kanji and kana readings
   */
  private static makeKey(kanjiReadings: string[], kanaReadings: string[]): string {
    const kanjiPart = kanjiReadings.sort().join(',');
    const kanaPart = kanaReadings.sort().join(',');
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
 * Allocates a batch of sequence numbers at once (thread-safe)
 * Returns the starting sequence number and count
 */
export async function allocateSeqBatch(count: number): Promise<number> {
  if (count <= 0) return 0;
  const sql = getConnection();

  // Get the last value after advancing by count
  const result = await sql<{ lastValue: number }[]>`
    SELECT last_value FROM (
      SELECT nextval('entry_seq_generator') as last_value
      FROM generate_series(1, ${count})
    ) AS batch
    ORDER BY last_value DESC
    LIMIT 1
  `;

  // Return the starting value (last - count + 1)
  return result[0].lastValue - count + 1;
}

/**
 * Gets all readings (kanji + kana) for an entry
 * Ported from dict-errata.lisp:257-261 get-all-readings
 */
export async function getAllReadings(seq: number): Promise<string[]> {
  const sql = getConnection();
  const results = await sql<{ text: string }[]>`
    SELECT text FROM kanji_text WHERE seq = ${seq}
    UNION
    SELECT text FROM kana_text WHERE seq = ${seq}
  `;
  return results.map(r => r.text);
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
  } = {}
): Promise<void> {
  const { via, conjTypes, asPosi } = options;

  // For secondary conjugations, conjugate from the intermediate form
  const seq = via || seqFrom;

  const conjMatrix = await conjugateEntryInner(seq, { conjTypes, asPosi });
  const originalReadings = await getAllReadings(seq);

  // First pass: collect all conjugations to insert and count them
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

    // Check if negative/formal forms are missing
    const ignoreNeg = !matrix[1][0].length && !matrix[1][1].length;
    const ignoreFml = !matrix[0][1].length && !matrix[1][1].length;

    // Process each cell of the 2x2 matrix
    for (let ii = 0; ii < 4; ii++) {
      const neg = ii >= 2;
      const fml = ii % 2 === 1;
      const negIdx = neg ? 1 : 0;
      const fmlIdx = fml ? 1 : 0;

      let readings = matrix[negIdx][fmlIdx];

      // Filter out readings that match the original entry
      readings = readings.filter(r => !originalReadings.includes(r.text));

      if (readings.length === 0) continue;

      toInsert.push({
        readings,
        pos,
        conjType: conjId,
        neg: ignoreNeg ? null : neg,
        fml: ignoreFml ? null : fml
      });
    }
  }

  // Allocate all sequences at once (single DB roundtrip)
  const startSeq = await allocateSeqBatch(toInsert.length);

  // Second pass: insert with pre-allocated sequences
  for (let i = 0; i < toInsert.length; i++) {
    const conj = toInsert[i];
    await insertConjugation(conj.readings, {
      seq: startSeq + i,
      from: seqFrom,
      pos: conj.pos,
      conjType: conj.conjType,
      neg: conj.neg,
      fml: conj.fml,
      via: via || null
    });
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
      INSERT INTO entry (seq, content)
      VALUES (${seq}, '')
      ON CONFLICT (seq) DO NOTHING
    `;

    // Determine if this conjugation can be further conjugated
    // Only plain affirmative forms (neg=false/null, fml=false/null) can be secondarily conjugated
    const conjugateP = SECONDARY_CONJUGATION_TYPES_FROM.includes(conjType)
                       && (!neg || neg === null)
                       && (!fml || fml === null);

    // Batch insert kanji readings (dict-load.lisp:411-414)
    if (uniqueKanjiReadings.length > 0) {
      const kanjiRows = uniqueKanjiReadings.map((text, i) => ({
        seq,
        text,
        ord: i,
        common: null,
        conjugateP
      }));
      await sql`
        INSERT INTO kanji_text ${sql(kanjiRows, 'seq', 'text', 'ord', 'common', 'conjugateP')}
      `;
    }

    // Batch insert kana readings (dict-load.lisp:415-418)
    if (uniqueKanaReadings.length > 0) {
      const kanaRows = uniqueKanaReadings.map((text, i) => ({
        seq,
        text,
        ord: i,
        common: null,
        conjugateP
      }));
      await sql`
        INSERT INTO kana_text ${sql(kanaRows, 'seq', 'text', 'ord', 'common', 'conjugateP')}
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

      toInsert.push({
        readings,
        pos,
        conjType: conjId,
        neg: ignoreNeg ? null : neg,
        fml: ignoreFml ? null : fml
      });
    }
  }

  // Allocate all sequences at once
  const startSeq = await allocateSeqBatch(toInsert.length);

  // Use batch insert for better performance (same as loadSecondaryConjugations)
  const batchResults = await insertConjugationsBatch(toInsert.map((conj, i) => ({
    readings: conj.readings,
    options: {
      seq: startSeq + i,
      from: seq,
      pos: conj.pos,
      conjType: conj.conjType,
      neg: conj.neg,
      fml: conj.fml,
      via: null
    }
  })), cache);

  return { cacheHits: batchResults.cacheHits, newEntries: batchResults.newEntries };
}

/**
 * Loads primary conjugations for all conjugatable entries
 * Ported from dict-load.lisp:425-433 load-conjugations
 * Optimized with in-memory cache for duplicate detection
 */
export async function loadConjugations(): Promise<void> {
  const sql = getConnection();

  // Initialize or reset the sequence for conjugated entry seq numbers
  console.log('Initializing entry sequence generator...');
  await createEntrySeqGenerator(sql);
  await resetEntrySeqGenerator(sql);

  // Initialize cache
  const cache = new ReadingsCache();
  await cache.initialize();

  console.log('Finding conjugatable entries...');
  const seqs = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM sense_prop
    WHERE seq NOT IN ${sql(DO_NOT_CONJUGATE_SEQ)}
      AND tag = 'pos'
      AND text IN ${sql(POS_WITH_CONJ_RULES)}
  `;

  console.log(`Processing ${seqs.length} entries...`);
  const startTime = Date.now();

  // Parallel loading configuration (matching load-jmdict pattern)
  const parallelism = 10;  // Process 10 entries in parallel
  let batch: number[] = [];
  let count = 0;
  let errors = 0;
  let cacheHits = 0;
  let newEntries = 0;

  for (const { seq } of seqs) {
    batch.push(seq);

    // Process batch when it reaches the parallelism limit
    if (batch.length >= parallelism) {
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
  }>,
  cache: ReadingsCache
): Promise<{ cacheHits: number; newEntries: number }> {
  const sql = getConnection();
  let cacheHits = 0;
  let newEntries = 0;

  // Filter out cache hits
  const toInsert = batch.filter(({ readings, options }) => {
    const uniqueKanjiReadings = [...new Set(readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(readings.filter(r => !r.kanjiFlag).map(r => r.text))];

    if (cache.findExisting(uniqueKanjiReadings, uniqueKanaReadings) !== null) {
      cacheHits++;
      return false;
    }

    // Add to cache with the seq we'll be using
    cache.add(options.seq, uniqueKanjiReadings, uniqueKanaReadings);
    return true;
  });

  if (toInsert.length === 0) {
    return { cacheHits, newEntries: 0 };
  }

  newEntries = toInsert.length;

  // Batch INSERT entries (entry table only has seq, content will be empty string)
  const entryValues = toInsert.map(({ options }) => ({
    seq: options.seq,
    content: '',
    rootP: false,
    nKanji: 0,
    nKana: 0,
    primaryNokanji: false,
  }));
  await sql`
    INSERT INTO entry ${sql(entryValues)}
    ON CONFLICT DO NOTHING
  `;

  // Batch INSERT text records
  const kanjiTexts: any[] = [];
  const kanaTexts: any[] = [];

  for (const { readings, options } of toInsert) {
    // Get unique readings
    const uniqueKanjiReadings = [...new Set(readings.filter(r => r.kanjiFlag).map(r => r.text))];
    const uniqueKanaReadings = [...new Set(readings.filter(r => !r.kanjiFlag).map(r => r.text))];

    // Determine if this conjugation can be further conjugated
    // Only plain affirmative forms (neg=false/null, fml=false/null) can be secondarily conjugated
    const conjugateP = SECONDARY_CONJUGATION_TYPES_FROM.includes(options.conjType)
                       && (!options.neg || options.neg === null)
                       && (!options.fml || options.fml === null);

    // Use sequential indices for ord, not original reading ord
    for (let i = 0; i < uniqueKanjiReadings.length; i++) {
      kanjiTexts.push({
        seq: options.seq,
        text: uniqueKanjiReadings[i],
        ord: i,
        common: null,
        conjugateP
      });
    }

    for (let i = 0; i < uniqueKanaReadings.length; i++) {
      kanaTexts.push({
        seq: options.seq,
        text: uniqueKanaReadings[i],
        ord: i,
        common: null,
        conjugateP
      });
    }
  }

  if (kanjiTexts.length > 0) {
    await sql`INSERT INTO kanji_text ${sql(kanjiTexts)} ON CONFLICT DO NOTHING`;
  }
  if (kanaTexts.length > 0) {
    await sql`INSERT INTO kana_text ${sql(kanaTexts)} ON CONFLICT DO NOTHING`;
  }

  // Batch INSERT conjugation records and get IDs
  const conjValues = toInsert.map(({ options }) => ({
    seq: options.seq,
    from: options.from,
    via: options.via,
  }));
  const conjResults = await sql`
    INSERT INTO conjugation ${sql(conjValues)}
    ON CONFLICT DO NOTHING
    RETURNING id, seq
  `;

  // Create map of seq -> conj_id
  const seqToConjId = new Map<number, number>();
  for (const row of conjResults) {
    seqToConjId.set(row.seq, row.id);
  }

  // Batch INSERT conj_prop using conj_id (one row per conjugation)
  const propValues = toInsert
    .map(({ options }) => {
      const conjId = seqToConjId.get(options.seq);
      if (!conjId) return null;
      return {
        conjId,
        conjType: options.conjType,
        pos: options.pos,
        neg: options.neg,
        fml: options.fml,
      };
    })
    .filter((v): v is NonNullable<typeof v> => v !== null);
  if (propValues.length > 0) {
    await sql`INSERT INTO conj_prop ${sql(propValues)} ON CONFLICT DO NOTHING`;
  }

  // Batch INSERT conj_source_reading using conj_id (one row per reading)
  const sourceValues = toInsert.flatMap(({ readings, options }) => {
    const conjId = seqToConjId.get(options.seq);
    if (!conjId) return [];
    return readings.map(r => ({
      conjId,
      text: r.text,
      sourceText: r.sourceText,
    }));
  });
  if (sourceValues.length > 0) {
    await sql`INSERT INTO conj_source_reading ${sql(sourceValues)} ON CONFLICT DO NOTHING`;
  }

  return { cacheHits, newEntries };
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

  for (const { seqFrom, seq, conjType } of toConj) {
    try {
      // Treat as v5s for causative-su, v1 otherwise
      const asPosi = conjType === 9 ? ['v5s'] : ['v1'];  // 9 = causative-su

      // Generate conjugations using existing conjugateEntryInner
      const conjMatrix = await conjugateEntryInner(seq, {
        conjTypes: SECONDARY_CONJUGATION_TYPES,
        asPosi
      });

      const originalReadings = await getAllReadings(seq);

      // Collect all conjugations to insert
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

          toInsert.push({
            readings,
            pos,
            conjType: conjId,
            neg: ignoreNeg ? null : neg,
            fml: ignoreFml ? null : fml
          });
        }
      }

      // Allocate all sequences at once
      const startSeq = await allocateSeqBatch(toInsert.length);

      // Batch insert all conjugations for this source entry
      const batchResults = await insertConjugationsBatch(toInsert.map((conj, i) => ({
        readings: conj.readings,
        options: {
          seq: startSeq + i,
          from: seqFrom,
          pos: conj.pos,
          conjType: conj.conjType,
          neg: conj.neg,
          fml: conj.fml,
          via: seq
        }
      })), cache);

      cacheHits += batchResults.cacheHits;
      newEntries += batchResults.newEntries;

      count++;

      if (count % 100 === 0 || count === toConj.length) {
        const elapsed = (Date.now() - startTime) / 1000;
        const rate = count / elapsed;
        const remaining = toConj.length - count;
        const eta = remaining / rate;
        console.log(`${count}/${toConj.length} secondary (${rate.toFixed(1)}/sec, ${cacheHits} cache hits, ${newEntries} new) - ETA: ${Math.ceil(eta)}s`);
      }
    } catch (error) {
      errors++;
      console.error(`Error generating secondary conjugation:`, error);
    }
  }

  const totalTime = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`✓ ${count} secondary conjugations generated in ${totalTime}s`);
  console.log(`  Cache hits: ${cacheHits.toLocaleString()}, New entries: ${newEntries.toLocaleString()}`);

  if (errors > 0) {
    console.warn(`⚠ ${errors} secondary conjugations failed`);
  }
}
