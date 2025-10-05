/**
 * Database errata and correction system
 * Ported from ~/ichiran/dict-errata.lisp
 *
 * This module provides functions to correct known issues in JMDict data
 * and add missing entries/readings that are commonly needed.
 */

import { getConnection } from '../conn.js';
import { testWord } from '../characters.js';
import { conjugateEntryOuter } from './conjugate.js';
import type { SenseProp, Conjugation, Entry } from '../types.js';

/**
 * Adds a reading to an existing entry
 * Ported from dict-errata.lisp:37-50 add-reading
 *
 * @param seq - Entry sequence number
 * @param reading - Reading text to add
 * @param options - Optional parameters
 */
export async function addReading(
  seq: number,
  reading: string,
  options: {
    common?: number | null;
    conjugateP?: boolean;
    table?: 'kanji_text' | 'kana_text';
  } = {}
): Promise<void> {
  const sql = getConnection();
  const { common = null, conjugateP = true, table } = options;

  // Determine table based on character type
  const isKana = testWord(reading, 'kana');
  const targetTable = table || (isKana ? 'kana_text' : 'kanji_text');

  // Check if reading already exists
  const existing = await sql`
    SELECT id FROM ${sql(targetTable)}
    WHERE seq = ${seq} AND text = ${reading}
  `;

  if (existing.length > 0) {
    return; // Already exists
  }

  // Get max ord for this entry
  const maxOrdResult = await sql<{ maxOrd: number | null }[]>`
    SELECT MAX(ord) as max_ord FROM ${sql(targetTable)}
    WHERE seq = ${seq}
  `;
  const maxOrd = maxOrdResult[0]?.maxOrd;
  const ord = maxOrd === null ? 0 : maxOrd + 1;

  // Insert new reading
  await sql`
    INSERT INTO ${sql(targetTable)} (seq, text, ord, common, conjugate_p)
    VALUES (${seq}, ${reading}, ${ord}, ${common}, ${conjugateP})
  `;

  // Update entry counts
  const nField = isKana ? 'n_kana' : 'n_kanji';
  await sql`
    UPDATE entry
    SET ${sql(nField)} = ${sql(nField)} + 1
    WHERE seq = ${seq}
  `;
}

/**
 * Replaces one reading with another
 * Ported from dict-errata.lisp:51-60 replace-reading
 *
 * @param seq - Entry sequence number
 * @param readingFrom - Original reading to replace
 * @param readingTo - New reading
 */
export async function replaceReading(
  seq: number,
  readingFrom: string,
  readingTo: string
): Promise<void> {
  const sql = getConnection();

  const isKana = testWord(readingFrom, 'kana');
  const table = isKana ? 'kana_text' : 'kanji_text';

  const result = await sql`
    UPDATE ${sql(table)}
    SET text = ${readingTo}
    WHERE seq = ${seq} AND text = ${readingFrom}
  `;

  if (result.count > 0) {
    await resetReadings(seq);
  }
}

/**
 * Resets reading field for entries
 * Ported from dict-errata.lisp:72-76 reset-readings
 */
async function resetReadings(...seqs: number[]): Promise<void> {
  const sql = getConnection();

  if (seqs.length === 0) return;

  // Get all readings for these entries
  const readings = await sql<{ id: number; text: string; ord: number; seq: number }[]>`
    SELECT id, text, ord, seq FROM kanji_text WHERE seq IN ${sql(seqs)}
    UNION ALL
    SELECT id, text, ord, seq FROM kana_text WHERE seq IN ${sql(seqs)}
  `;

  // Update reading field for each
  for (const reading of readings) {
    const readingValue = `${reading.text} ${reading.ord}`;
    await sql`
      UPDATE ${sql(testWord(reading.text, 'kana') ? 'kana_text' : 'kanji_text')}
      SET reading = ${readingValue}
      WHERE id = ${reading.id}
    `;
  }
}

/**
 * Deletes a reading from an entry
 * Ported from dict-errata.lisp:78-95 delete-reading
 *
 * @param seq - Entry sequence number
 * @param reading - Reading text to delete
 * @param options - Optional parameters
 */
export async function deleteReading(
  seq: number,
  reading: string,
  options: {
    table?: 'kanji_text' | 'kana_text';
  } = {}
): Promise<void> {
  const sql = getConnection();

  const isKana = testWord(reading, 'kana');
  const table = options.table || (isKana ? 'kana_text' : 'kanji_text');

  // Delete the reading
  const result = await sql`
    DELETE FROM ${sql(table)}
    WHERE seq = ${seq} AND text = ${reading}
  `;

  const deleted = result.count;

  if (deleted > 0) {
    // Update entry counts
    const nField = isKana ? 'n_kana' : 'n_kanji';
    await sql`
      UPDATE entry
      SET ${sql(nField)} = ${sql(nField)} - ${deleted}
      WHERE seq = ${seq}
    `;

    // Reorder remaining readings
    const remaining = await sql<{ id: number }[]>`
      SELECT id FROM ${sql(table)}
      WHERE seq = ${seq}
      ORDER BY ord
    `;

    for (let i = 0; i < remaining.length; i++) {
      await sql`
        UPDATE ${sql(table)}
        SET ord = ${i}
        WHERE id = ${remaining[i].id}
      `;
    }

    await resetReadings(seq);
  }
}

/**
 * Deletes a sense property (e.g., "uk" misc tag)
 * Ported from dict-errata.lisp:138-140 delete-sense-prop
 *
 * @param seq - Entry sequence number
 * @param tag - Property tag (e.g., "misc", "pos")
 * @param text - Property value (e.g., "uk")
 */
export async function deleteSenseProp(
  seq: number,
  tag: string,
  text: string
): Promise<void> {
  const sql = getConnection();

  await sql`
    DELETE FROM sense_prop
    WHERE seq = ${seq} AND tag = ${tag} AND text = ${text}
  `;
}

/**
 * Adds a sense property to an existing sense
 * Ported from dict-errata.lisp:142-146 add-sense-prop
 *
 * @param seq - Entry sequence number
 * @param senseOrd - Sense order number (0-indexed)
 * @param tag - Property tag (e.g., "misc", "pos")
 * @param text - Property value (e.g., "uk")
 */
export async function addSenseProp(
  seq: number,
  senseOrd: number,
  tag: string,
  text: string
): Promise<void> {
  const sql = getConnection();

  // Find the sense
  const senses = await sql<{ id: number }[]>`
    SELECT id FROM sense
    WHERE seq = ${seq} AND ord = ${senseOrd}
  `;

  if (senses.length === 0) {
    return; // Sense doesn't exist
  }

  const senseId = senses[0].id;

  // Check if property already exists
  const existing = await sql`
    SELECT id FROM sense_prop
    WHERE sense_id = ${senseId} AND tag = ${tag} AND text = ${text}
  `;

  if (existing.length === 0) {
    await sql`
      INSERT INTO sense_prop (sense_id, tag, text, ord, seq)
      VALUES (${senseId}, ${tag}, ${text}, 0, ${seq})
    `;
  }
}

/**
 * Checks if a sense with given POS and glosses already exists
 * Ported from dict-load.lisp:80-91 sense-exists-p
 */
async function senseExistsP(
  seq: number,
  positions: string[],
  glosses: string[]
): Promise<boolean> {
  const sql = getConnection();

  // Get all senses for this entry with their properties and glosses
  const senses = await sql<{
    senseId: number;
    ord: number;
    glossText: string;
    propTag: string;
    propText: string;
  }[]>`
    SELECT
      s.id as sense_id,
      s.ord,
      g.text as gloss_text,
      sp.tag as prop_tag,
      sp.text as prop_text
    FROM sense s
    LEFT JOIN gloss g ON g.sense_id = s.id
    LEFT JOIN sense_prop sp ON sp.sense_id = s.id
    WHERE s.seq = ${seq}
    ORDER BY s.ord, g.ord, sp.ord
  `;

  // Group by sense
  const sensesMap = new Map<number, { ord: number; glosses: string[]; pos: string[] }>();

  for (const row of senses) {
    if (!sensesMap.has(row.senseId)) {
      sensesMap.set(row.senseId, { ord: row.ord, glosses: [], pos: [] });
    }
    const sense = sensesMap.get(row.senseId)!;

    if (row.glossText && !sense.glosses.includes(row.glossText)) {
      sense.glosses.push(row.glossText);
    }
    if (row.propTag === 'pos' && row.propText && !sense.pos.includes(row.propText)) {
      sense.pos.push(row.propText);
    }
  }

  // Check if any sense matches
  for (const sense of sensesMap.values()) {
    const posMatches = JSON.stringify(sense.pos.sort()) === JSON.stringify([...positions].sort());
    const glossesMatch = JSON.stringify(sense.glosses.sort()) === JSON.stringify([...glosses].sort());

    if (posMatches && glossesMatch) {
      return true;
    }
  }

  return false;
}

/**
 * Adds a new sense to an entry
 * Ported from dict-load.lisp:93-110 add-new-sense
 *
 * @param seq - Entry sequence number
 * @param positions - POS tags (e.g., ["n"], ["v5r"])
 * @param glosses - English glosses
 * @returns Sense ID and ord, or undefined if already exists
 */
export async function addNewSense(
  seq: number,
  positions: string[],
  glosses: string[]
): Promise<{ senseId: number; ord: number } | undefined> {
  const sql = getConnection();

  // Check if sense already exists
  if (await senseExistsP(seq, positions, glosses)) {
    return undefined;
  }

  // Get last sense to determine new ord
  const lastSense = await sql<{ ord: number }[]>`
    SELECT ord FROM sense
    WHERE seq = ${seq}
    ORDER BY ord DESC
    LIMIT 1
  `;

  const ord = lastSense.length > 0 ? lastSense[0].ord + 1 : 0;

  // Get last POS from previous senses
  const lastPosResult = await sql<{ propText: string }[]>`
    SELECT sp.text as prop_text
    FROM sense s
    LEFT JOIN sense_prop sp ON sp.sense_id = s.id
    WHERE s.seq = ${seq} AND sp.tag = 'pos'
    ORDER BY s.ord DESC, sp.ord DESC
    LIMIT 1
  `;

  const lastPos = lastPosResult.length > 0 ? [lastPosResult[0].propText] : null;

  // Create new sense
  const newSense = await sql<{ id: number }[]>`
    INSERT INTO sense (seq, ord)
    VALUES (${seq}, ${ord})
    RETURNING id
  `;

  const senseId = newSense[0].id;

  // Add glosses
  for (let gord = 0; gord < glosses.length; gord++) {
    await sql`
      INSERT INTO gloss (sense_id, text, ord)
      VALUES (${senseId}, ${glosses[gord]}, ${gord})
    `;
  }

  // Add POS tags if different from last sense
  if (!lastPos || JSON.stringify(lastPos) !== JSON.stringify(positions)) {
    for (let sord = 0; sord < positions.length; sord++) {
      await sql`
        INSERT INTO sense_prop (sense_id, tag, text, ord, seq)
        VALUES (${senseId}, ${'pos'}, ${positions[sord]}, ${sord}, ${seq})
      `;
    }
  }

  return { senseId, ord };
}

/**
 * Sets the common field for a reading
 * Ported from dict-errata.lisp:168-171 set-common
 *
 * @param table - Either 'kanji_text' or 'kana_text'
 * @param seq - Entry sequence number
 * @param text - Reading text
 * @param common - Common value (frequency indicator), null to unset
 */
export async function setCommon(
  table: 'kanji_text' | 'kana_text',
  seq: number,
  text: string,
  common: number | null
): Promise<void> {
  const sql = getConnection();

  await sql`
    UPDATE ${sql(table)}
    SET common = ${common}
    WHERE seq = ${seq} AND text = ${text}
  `;
}

/**
 * Sets the primary_nokanji field for an entry
 * Ported from dict-errata.lisp:224-227 set-primary-nokanji
 *
 * @param seq - Entry sequence number
 * @param value - Boolean value for primary_nokanji
 */
export async function setPrimaryNokanji(
  seq: number,
  value: boolean
): Promise<void> {
  const sql = getConnection();

  await sql`
    UPDATE entry
    SET primary_nokanji = ${value}
    WHERE seq = ${seq}
  `;
}

/**
 * Sets primary_nokanji flag on entry and marks specific reading as nokanji
 * Ported from dict-errata.lisp:229-235 add-primary-nokanji
 *
 * @param seq - Entry sequence number
 * @param reading - Reading text to mark as nokanji
 */
export async function addPrimaryNokanji(
  seq: number,
  reading: string
): Promise<void> {
  const sql = getConnection();

  // Set primary_nokanji on entry
  await setPrimaryNokanji(seq, true);

  // Determine table based on character type
  const isKana = testWord(reading, 'kana');
  const table = isKana ? 'kana_text' : 'kanji_text';

  // Set nokanji flag on the reading
  await sql`
    UPDATE ${sql(table)}
    SET nokanji = true
    WHERE seq = ${seq} AND text = ${reading}
  `;
}

/**
 * Deletes senses matching a filter predicate
 * Ported from dict-errata.lisp:131-136 delete-senses
 *
 * @param seq - Entry sequence number
 * @param filter - Predicate function to test sense properties
 */
async function deleteSenses(seq: number, filter: (prop: SenseProp) => boolean): Promise<void> {
  const sql = getConnection();

  // Get all sense properties for this entry
  const senseProps = await sql<SenseProp[]>`
    SELECT * FROM sense_prop
    WHERE seq = ${seq}
  `;

  // Filter to get matching properties
  const matchingProps = senseProps.filter(filter);

  if (matchingProps.length === 0) {
    return;
  }

  // Get unique sense IDs
  const senseIds = [...new Set(matchingProps.map(prop => prop.senseId))];

  // Delete sense properties, glosses, and senses
  await sql`
    DELETE FROM sense_prop
    WHERE sense_id IN ${sql(senseIds)}
  `;

  await sql`
    DELETE FROM gloss
    WHERE sense_id IN ${sql(senseIds)}
  `;

  await sql`
    DELETE FROM sense
    WHERE id IN ${sql(senseIds)}
  `;
}

/**
 * Rearranges readings so those with prefix come first
 * Ported from dict-errata.lisp:229-239 rearrange-readings
 *
 * @param seq - Entry sequence number
 * @param table - Table name (kanji_text or kana_text)
 * @param prefix - Prefix to match
 */
async function rearrangeReadings(seq: number, table: 'kanji_text' | 'kana_text', prefix: string): Promise<void> {
  const sql = getConnection();

  // Count readings with prefix
  const offsetResult = await sql<{ count: string }[]>`
    SELECT COUNT(id) as count
    FROM ${sql(table)}
    WHERE seq = ${seq} AND text LIKE ${prefix + '%'}
  `;
  const offset = parseInt(offsetResult[0].count, 10);

  // Get all readings ordered by current ord
  const readings = await sql<{ id: number; text: string }[]>`
    SELECT id, text
    FROM ${sql(table)}
    WHERE seq = ${seq}
    ORDER BY ord
  `;

  let cnt1 = -1;
  let cnt2 = offset - 1;

  // Update ord values: prefix readings get 0, 1, 2... and others follow
  for (const reading of readings) {
    const hasPrefix = reading.text.startsWith(prefix);
    const newOrd = hasPrefix ? ++cnt1 : ++cnt2;

    await sql`
      UPDATE ${sql(table)}
      SET ord = ${newOrd}
      WHERE id = ${reading.id}
    `;
  }
}

/**
 * Rearranges readings with prefix for entry and all conjugations
 * Ported from dict-errata.lisp:241-244 rearrange-readings-conj
 *
 * @param seq - Entry sequence number
 * @param table - Table name (kanji_text or kana_text)
 * @param prefix - Prefix to match
 */
export async function rearrangeReadingsConj(seq: number, table: 'kanji_text' | 'kana_text', prefix: string): Promise<void> {
  const sql = getConnection();

  // Rearrange for the main entry
  await rearrangeReadings(seq, table, prefix);

  // Get all conjugated entries
  const conjugatedSeqs = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM conjugation
    WHERE "from" = ${seq}
  `;

  // Rearrange for each conjugated entry
  for (const { seq: conjSeq } of conjugatedSeqs) {
    await rearrangeReadings(conjSeq, table, prefix);
  }
}

/**
 * Deletes conjugation links and optionally the entry if no other links exist
 * Ported from dict-errata.lisp:198-215 delete-conjugation
 *
 * @param seq - Entry sequence number (conjugated form)
 * @param from - Source entry sequence number
 * @param via - Optional via entry sequence number
 */
async function deleteConjugation(seq: number, from: number, via: number | null = null): Promise<void> {
  const sql = getConnection();

  // Find conjugations matching the criteria
  const conj = await sql<Conjugation[]>`
    SELECT * FROM conjugation
    WHERE seq = ${seq} AND "from" = ${from} AND via IS NOT DISTINCT FROM ${via}
  `;

  if (conj.length === 0) {
    return;
  }

  const conjIds = conj.map(c => c.id);

  // Get the entry to check if it should be deleted
  const entry = await sql<Entry[]>`
    SELECT * FROM entry
    WHERE seq = ${seq}
  `;

  if (entry.length === 0) {
    return;
  }

  // Check if entry should be deleted (not root and no other conjugations)
  const otherConj = await sql<Conjugation[]>`
    SELECT * FROM conjugation
    WHERE seq = ${seq} AND id NOT IN ${sql(conjIds)}
  `;

  const deleteEntry = !entry[0].rootP && otherConj.length === 0;

  // Delete conjugation-related records
  await sql`
    DELETE FROM conj_prop
    WHERE conj_id IN ${sql(conjIds)}
  `;

  await sql`
    DELETE FROM conj_source_reading
    WHERE conj_id IN ${sql(conjIds)}
  `;

  await sql`
    DELETE FROM conjugation
    WHERE id IN ${sql(conjIds)}
  `;

  // Delete entry if it has no other conjugations and is not root
  if (deleteEntry) {
    await sql`
      DELETE FROM entry
      WHERE seq = ${seq}
    `;
  }
}

/**
 * Calculates root difference between base text and reading
 * Helper function for add-conj-reading
 * Ported from dict-errata.lisp:97-104 root-diff
 */
function rootDiff(baseText: string, reading: string): [number, number] {
  const lb = baseText.length;
  const lr = reading.length;

  for (let ib = lb - 1, ir = lr - 1; ib >= 0 && ir >= 0; ib--, ir--) {
    if (baseText[ib] !== reading[ir]) {
      return [ib + 1, ir + 1];
    }
  }

  // All matched, return remaining lengths
  if (lr >= lb) {
    return [0, lr - lb];
  } else {
    return [lb - lr, 0];
  }
}

/**
 * Creates a function to transform text based on root difference
 * Ported from dict-errata.lisp:106-109 root-diff-fn
 */
function rootDiffFn(baseText: string, reading: string): (text: string) => string {
  const [b, r] = rootDiff(baseText, reading);
  return (text: string) => reading.substring(0, r) + text.substring(b);
}

/**
 * Adds a reading to conjugated forms
 * Ported from dict-errata.lisp:111-129 add-conj-reading
 *
 * @param seq - Entry sequence number
 * @param reading - Reading to add to conjugated forms
 */
export async function addConjReading(seq: number, reading: string): Promise<void> {
  const sql = getConnection();

  const isKana = testWord(reading, 'kana');
  const table = isKana ? 'kana_text' : 'kanji_text';

  // Get base text (ord=0)
  const baseTextResult = await sql<{ text: string }[]>`
    SELECT text FROM ${sql(table)}
    WHERE seq = ${seq} AND ord = 0
  `;

  if (baseTextResult.length === 0) {
    return;
  }

  const baseText = baseTextResult[0].text;
  const diffFn = rootDiffFn(baseText, reading);

  // Get all conjugations from this entry
  const conjugations = await sql<Conjugation[]>`
    SELECT * FROM conjugation
    WHERE "from" = ${seq}
  `;

  for (const conj of conjugations) {
    const conjSeq = conj.seq;

    // Get entry for updating counts
    const entry = await sql<Entry[]>`
      SELECT * FROM entry
      WHERE seq = ${conjSeq}
    `;

    if (entry.length === 0) {
      continue;
    }

    // Get base reading for conjugated entry
    const base = await sql<{ id: number; text: string; conjugateP: boolean }[]>`
      SELECT id, text, conjugate_p FROM ${sql(table)}
      WHERE seq = ${conjSeq} AND ord = 0
    `;

    if (base.length === 0) {
      continue;
    }

    const newText = diffFn(base[0].text);

    // Check if reading already exists
    const existing = await sql`
      SELECT id FROM ${sql(table)}
      WHERE seq = ${conjSeq} AND text = ${newText}
    `;

    if (existing.length > 0) {
      continue;
    }

    // Get max ord
    const maxOrdResult = await sql<{ maxOrd: number | null }[]>`
      SELECT MAX(ord) as max_ord FROM ${sql(table)}
      WHERE seq = ${conjSeq}
    `;
    const maxOrd = maxOrdResult[0]?.maxOrd;

    // Get source text from conj_source_reading
    const sourceTextResult = await sql<{ sourceText: string }[]>`
      SELECT source_text FROM conj_source_reading
      WHERE conj_id = ${conj.id} AND text = ${base[0].text}
    `;

    const sourceText = sourceTextResult.length > 0 ? sourceTextResult[0].sourceText : base[0].text;
    const newSourceText = diffFn(sourceText);

    // Insert new reading
    // Note: Lisp uses maxord directly (not +1), which matches database behavior
    await sql`
      INSERT INTO ${sql(table)} (text, seq, ord, common, conjugate_p)
      VALUES (${newText}, ${conjSeq}, ${maxOrd}, NULL, ${base[0].conjugateP})
    `;

    // Insert conj_source_reading
    await sql`
      INSERT INTO conj_source_reading (conj_id, text, source_text)
      VALUES (${conj.id}, ${newText}, ${newSourceText})
    `;

    // Update entry counts
    const nField = isKana ? 'n_kana' : 'n_kanji';
    await sql`
      UPDATE entry
      SET ${sql(nField)} = ${sql(nField)} + 1
      WHERE seq = ${conjSeq}
    `;
  }
}

/**
 * Replaces reading prefix in entry and conjugations
 * Ported from dict-errata.lisp:62-70 replace-reading-conj
 *
 * @param seq - Entry sequence number
 * @param table - Table name (kanji_text or kana_text)
 * @param prefixFrom - Prefix to replace
 * @param prefixTo - New prefix
 */
export async function replaceReadingConj(
  seq: number,
  table: 'kanji_text' | 'kana_text',
  prefixFrom: string,
  prefixTo: string
): Promise<void> {
  const sql = getConnection();

  // Get all sequences: the entry itself and all conjugated entries
  const conjugatedSeqs = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM conjugation
    WHERE "from" = ${seq}
  `;

  const seqs = [seq, ...conjugatedSeqs.map(c => c.seq)];

  // Get all readings with the prefix
  const readings = await sql<{ id: number; seq: number; text: string }[]>`
    SELECT id, seq, text
    FROM ${sql(table)}
    WHERE seq IN ${sql(seqs)} AND text LIKE ${prefixFrom + '%'}
  `;

  const toUpdate: number[] = [];

  for (const reading of readings) {
    // Replace prefix
    const newText = prefixTo + reading.text.substring(prefixFrom.length);

    await sql`
      UPDATE ${sql(table)}
      SET text = ${newText}
      WHERE id = ${reading.id}
    `;

    if (!toUpdate.includes(reading.seq)) {
      toUpdate.push(reading.seq);
    }
  }

  // Reset readings for all updated sequences
  if (toUpdate.length > 0) {
    await resetReadings(...toUpdate);
  }
}

/**
 * Adds cop-da POS tag to だ and conjugates it
 * Ported from dict-errata.lisp:280-286 conjugate-da
 *
 * JMdict might have renamed cop-da to cop, but the data csvs still use cop-da
 * which causes だ to not conjugate, which is bad
 */
async function conjugateDa(seq: number = 2089020): Promise<void> {
  const sql = getConnection();

  // Check if cop-da already exists
  const existing = await sql`
    SELECT id FROM sense_prop
    WHERE seq = ${seq} AND tag = 'pos' AND text = 'cop-da'
  `;

  if (existing.length === 0) {
    await addSenseProp(seq, 0, 'pos', 'cop-da');
    await conjugateEntryOuter(seq);
  }
}

/**
 * Adds じゃ readings as colloquial alternatives to では readings for all だ conjugations
 * Ported from dict-errata.lisp:237-256 add-deha-ja-readings
 *
 * For all conjugated entries from だ (seq 2089020), if they have a では reading,
 * add a corresponding じゃ reading as a colloquial alternative.
 */
async function addDehaJaReadings(): Promise<void> {
  const sql = getConnection();
  const daSeq = 2089020;

  // Find all conjugated entries from だ
  const conjugated = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM conjugation
    WHERE "from" = ${daSeq}
  `;

  for (const { seq } of conjugated) {
    // Find kana readings with では
    const dehaReadings = await sql<{ text: string }[]>`
      SELECT text FROM kana_text
      WHERE seq = ${seq} AND text LIKE '%では%'
    `;

    for (const { text } of dehaReadings) {
      // Replace では with じゃ
      const jaReading = text.replace(/では/g, 'じゃ');

      // Check if じゃ reading already exists
      const existing = await sql`
        SELECT id FROM kana_text
        WHERE seq = ${seq} AND text = ${jaReading}
      `;

      if (existing.length === 0) {
        // Add the じゃ reading
        await addReading(seq, jaReading, { conjugateP: false });
      }
    }
  }
}

/**
 * Removes primary_nokanji flag from entries where all kana readings are hiragana and marked as nokanji
 * Ported from dict-errata.lisp:258-268 remove-hiragana-nokanji
 *
 * This corrects entries where the primary_nokanji flag was incorrectly set
 * for entries that only have hiragana readings.
 */
async function removeHiraganaNokanji(): Promise<void> {
  const sql = getConnection();

  // Find entries with primary_nokanji flag
  const entries = await sql<{ seq: number }[]>`
    SELECT seq FROM entry
    WHERE primary_nokanji = true
  `;

  for (const { seq } of entries) {
    // Get all kana readings for this entry
    const kanaReadings = await sql<{ text: string; nokanji: boolean }[]>`
      SELECT text, nokanji FROM kana_text
      WHERE seq = ${seq}
    `;

    // Check if all readings are hiragana and marked as nokanji
    const allHiraganaNokanji = kanaReadings.length > 0 && kanaReadings.every(r => {
      const isHiragana = testWord(r.text, 'hiragana');
      return isHiragana && r.nokanji;
    });

    if (allHiraganaNokanji) {
      // Remove primary_nokanji flag
      await setPrimaryNokanji(seq, false);

      // Remove nokanji flag from all kana readings
      await sql`
        UPDATE kana_text
        SET nokanji = false
        WHERE seq = ${seq}
      `;
    }
  }
}

/**
 * Adds special conjugations for ございます (polite copula)
 * Ported from dict-errata.lisp:270-278 add-gozaimasu-conjs
 *
 * Creates conjugation links for ございます entries (1612690, 2253080)
 * linking them to appropriate copula conjugations.
 */
async function addGozaimasuConjs(): Promise<void> {
  const sql = getConnection();
  const gozaimasuSeqs = [1612690, 2253080];

  for (const seq of gozaimasuSeqs) {
    // Check if entry exists
    const entry = await sql`
      SELECT seq FROM entry WHERE seq = ${seq}
    `;

    if (entry.length === 0) {
      continue;
    }

    // Add conjugation link from でございます (1005800) if it exists
    const deGozaimasuSeq = 1005800;
    const deGozaimasu = await sql`
      SELECT seq FROM entry WHERE seq = ${deGozaimasuSeq}
    `;

    if (deGozaimasu.length > 0) {
      // Check if conjugation already exists
      const existing = await sql`
        SELECT id FROM conjugation
        WHERE seq = ${seq} AND "from" = ${deGozaimasuSeq}
      `;

      if (existing.length === 0) {
        // Add conjugation link
        const conjResult = await sql<{ id: number }[]>`
          INSERT INTO conjugation (seq, "from", via)
          VALUES (${seq}, ${deGozaimasuSeq}, NULL)
          RETURNING id
        `;

        // Add conj_prop for polite copula (type 11 is polite form)
        if (conjResult.length > 0) {
          const conjId = conjResult[0].id;
          await sql`
            INSERT INTO conj_prop (conj_id, conj_type, pos, neg, fml)
            VALUES (${conjId}, 11, 'cop', false, true)
          `;
        }
      }
    }
  }
}

/**
 * Main errata application function
 * Ported from dict-errata.lisp:287-580 add-errata
 *
 * Applies all known corrections to the database:
 * - Fixes readings (add/delete)
 * - Corrects sense properties (POS, misc tags)
 * - Adjusts common field values
 * - Sets primary_nokanji flags
 */
export async function addErrata(): Promise<void> {
  console.log('Applying database errata...');

  // Run helper functions first
  console.log('  Adding deha-ja readings...');
  await addDehaJaReadings();

  console.log('  Removing hiragana nokanji flags...');
  await removeHiraganaNokanji();

  console.log('  Adding gozaimasu conjugations...');
  await addGozaimasuConjs();

  // Add cop-da POS tag and conjugate だ
  await conjugateDa();

  // Add primary_nokanji for specific entries
  await addPrimaryNokanji(1415510, 'タカ');

  // Fix primary_nokanji flags
  await setPrimaryNokanji(1538900, false); // ただ
  await setPrimaryNokanji(1580640, false); // 人
  await setPrimaryNokanji(1289030, false); // いまいち

  // Delete problematic readings
  await deleteReading(1247250, 'キミ');
  await deleteReading(1521960, 'ボツ');
  await deleteReading(2145800, 'いら');
  await deleteReading(2067160, 'たも');
  await deleteReading(2423450, 'サシ');
  await deleteReading(2574600, 'どうなん');

  // Add missing readings
  await addReading(2015370, 'ワシ');
  await addReading(1202410, 'カニ');
  await addReading(2145800, 'イラ');
  await addReading(1517840, 'ハチ');
  await addReading(2029080, 'ねぇ');
  await addReading(2089020, 'じゃ', { common: 0, conjugateP: false });

  // Set common values
  await setCommon('kana_text', 1517840, 'ハチ', 34);

  // Additional set-common calls from Lisp dict-errata.lisp lines 422-502
  await setCommon('kana_text', 1310920, 'したい', null);
  await setCommon('kana_text', 1159430, 'いたい', null);
  await setCommon('kana_text', 1523060, 'ほんと', 2);
  await setCommon('kana_text', 1577100, 'なん', 2);
  await setCommon('kana_text', 1012440, 'めく', null);
  await setCommon('kana_text', 1005600, 'しまった', null);
  await setCommon('kana_text', 2139720, 'ん', 0);
  await setCommon('kana_text', 1309910, 'してい', 0);
  await setCommon('kana_text', 1311320, 'してい', 0);
  await setCommon('kana_text', 1423310, 'なか', 1);
  await setCommon('kanji_text', 1245280, '空', 0);
  await setCommon('kana_text', 1308640, 'しない', 0);
  await setCommon('kana_text', 1579130, 'ことし', 0);
  await setCommon('kana_text', 2084660, 'いなくなった', 0);
  await setCommon('kana_text', 1570850, 'すね', null);
  await setCommon('kana_text', 1470740, 'のうち', 0);
  await setCommon('kana_text', 1156100, 'いいん', 0);
  await setCommon('kana_text', 1472520, 'はいいん', null);
  await setCommon('kana_text', 1445000, 'としん', 0);
  await setCommon('kana_text', 1408100, 'たよう', 0);
  await setCommon('kana_text', 2409180, 'ような', 0);
  await setCommon('kana_text', 1524550, 'まいそう', null);
  await setCommon('kana_text', 1925750, 'そうする', null);
  await setCommon('kana_text', 1587780, 'いる', null);
  await setCommon('kana_text', 1322180, 'いる', null);
  await setCommon('kana_text', 1391500, 'いる', null);
  await setCommon('kanji_text', 1606560, '分かる', 11);
  await setCommon('kana_text', 1606560, 'わかる', 11);
  await setCommon('kanji_text', 1547720, '来る', 11);
  await setCommon('kana_text', 1547720, 'くる', 11);
  await setCommon('kana_text', 2134680, 'それは', 0);
  await setCommon('kana_text', 2134680, 'そりゃ', 0);
  await setCommon('kana_text', 1409140, 'からだ', 0);
  await setCommon('kana_text', 1552120, 'ながす', null);
  await setCommon('kana_text', 1516930, 'ほう', 1);
  await setCommon('kana_text', 1518220, 'ほうが', null);
  await setCommon('kana_text', 1603340, 'ほうが', null);
  await setCommon('kana_text', 1158400, 'いどう', null);
  await setCommon('kana_text', 1157970, 'いどう', null);
  await setCommon('kana_text', 1599900, 'になう', null);
  await setCommon('kana_text', 1465590, 'はいる', null);
  await setCommon('kana_text', 1535930, 'とい', 0);
  await setCommon('kana_text', 1472480, 'はいらん', null);
  await setCommon('kanji_text', 2019640, '杯', 20);
  await setCommon('kana_text', 1416220, 'たち', 10);
  await setCommon('kana_text', 1402900, 'そうなん', null);
  await setCommon('kana_text', 1446980, 'いたむ', null);
  await setCommon('kana_text', 1432710, 'いたむ', null);
  await setCommon('kana_text', 1632670, 'かむ', null);
  await setCommon('kana_text', 1224090, 'きが', 40);
  await setCommon('kana_text', 1534470, 'もうこ', null);
  await setCommon('kana_text', 1739410, 'わけない', 0);
  await setCommon('kanji_text', 1416860, '誰も', 0);
  await setCommon('kana_text', 2093030, 'そっか', 0);
  await setCommon('kanji_text', 1001840, 'お兄ちゃん', 0);
  await setCommon('kanji_text', 1341350, '旬', 0);
  await setCommon('kana_text', 1188790, 'いつか', 0);
  await setCommon('kana_text', 1582900, 'もす', null);
  await setCommon('kana_text', 1577270, 'セリフ', 0);
  await setCommon('kana_text', 1375650, 'せいか', 0);
  await setCommon('kanji_text', 1363540, '真逆', null);
  await setCommon('kana_text', 1632200, 'どうか', 0);
  await setCommon('kanji_text', 1920245, '何の', 0);
  await setCommon('kana_text', 2733410, 'だよね', 0);
  await setCommon('kana_text', 1234260, 'ともに', 0);
  await setCommon('kanji_text', 2242840, '未', 0);
  await setCommon('kana_text', 1246890, 'リス', 0);
  await setCommon('kana_text', 1257270, 'やらしい', 0);
  await setCommon('kana_text', 1343100, 'とこ', 0);
  await setCommon('kana_text', 1529930, 'むこう', 14);
  await setCommon('kanji_text', 1317910, '自重', 30);
  await setCommon('kana_text', 1586420, 'あったかい', 0);
  await setCommon('kana_text', 1214190, 'かんない', null);
  await setCommon('kana_text', 1614320, 'かんない', null);
  await setCommon('kana_text', 1517220, 'ほうがい', null);
  await setCommon('kana_text', 1380990, 'せいなん', null);
  await setCommon('kana_text', 1280630, 'こうなん', null);
  await setCommon('kana_text', 1289620, 'こんなん', null);
  await setCommon('kana_text', 1204090, 'がいまい', null);
  await setCommon('kana_text', 1459170, 'ないほう', null);
  await setCommon('kana_text', 2457920, 'ですか', null);
  await setCommon('kana_text', 1228390, 'すいもの', null);
  await setCommon('kana_text', 1423240, 'きもの', 0);
  await setCommon('kana_text', 1212110, 'かんじ', 0);
  await setCommon('kana_text', 1516160, 'たから', 0);
  await setCommon('kana_text', 1575510, 'コマ', 0);
  await setCommon('kanji_text', 1603990, '街', 0);
  await setCommon('kana_text', 1548520, 'からむ', null);
  await setCommon('kana_text', 2174250, 'もしや', 0);
  await setCommon('kana_text', 1595080, 'のく', null);
  await setCommon('kana_text', 1309950, 'しどう', 0);
  await setCommon('kana_text', 1524860, 'まくら', 9);
  await setCommon('kanji_text', 1451770, '同じよう', 30);
  await setCommon('kana_text', 1244210, 'くない', 0);
  await setCommon('kana_text', 1898260, 'どうし', 11);
  await setCommon('kanji_text', 1407980, '多分', 1);
  await setCommon('kana_text', 1579630, 'なのか', null);
  await setCommon('kana_text', 1371880, 'すいてき', null);
  await setCommon('kana_text', 1008420, 'でしょ', 0);
  await setCommon('kana_text', 1928670, 'だろ', 0);
  await setCommon('kanji_text', 1000580, '彼', null);
  await setCommon('kana_text', 1546380, 'ようと', 0);
  await setCommon('kana_text', 2246510, 'なさそう', 0);
  await setCommon('kanji_text', 2246510, '無さそう', 0);
  await setCommon('kana_text', 1579110, 'きょう', 2);
  await setCommon('kana_text', 1235870, 'きょう', null);
  await setCommon('kana_text', 1587200, 'いこう', 11);
  await setCommon('kana_text', 1158240, 'いこう', 0);
  await setCommon('kana_text', 1534440, 'もうまく', null);
  await setCommon('kana_text', 1459400, 'ないよう', 0);
  await setCommon('kana_text', 1590480, 'カッコ', 0);
  await setCommon('kana_text', 1208240, 'カッコ', 0);
  await setCommon('kana_text', 1495770, 'つける', 11);
  await setCommon('kana_text', 1610400, 'つける', 12);
  await setCommon('kana_text', 1495740, 'つく', 11);
  await setCommon('kanji_text', 1495740, '付く', 11);

  // Delete sense-prop "uk" (usually-kana tag) from entries where it's inappropriate
  const deleteUkSeqs = [
    1611000, // 生る
    1305070, // 仕手
    1583470, // 品
    1446760, // しな
    1302910, // だし
    2802220, // う
    1535790, // もち
    2119750, // なんだ
    2220330, // つ
    1207600, // かけ
    1399970, // かく
    2094480, // らい
    2729170, // いる
    1580640, // 人
    1569440, // かし
    2423450, // さし
    1578850, // 行く
    1609500, // 罹る
    1444150, // 吐く
    1546640, // 要る
    1314490, // ことなく
    2643710, // やす
    1611260, // はねる
    2208960, // かける
    1155020, // もって
    1208240, // かっこ
    1207590, // かかる
    1279680, // かまう
    1469810, // ないし
    1474370, // むく
    1609300, // うたう
    1612920, // ひく
    2827450, // まめ
    1333570, // たかる
    1610400, // つける
    2097190, // つく
  ];

  for (const seq of deleteUkSeqs) {
    await deleteSenseProp(seq, 'misc', 'uk');
  }

  // Add sense-prop "uk" to entries that need it
  await addSenseProp(1394680, 0, 'misc', 'uk'); // そういう
  await addSenseProp(2272830, 0, 'misc', 'uk'); // すごく
  await addSenseProp(1270680, 0, 'misc', 'uk'); // ごめんなさい
  await addSenseProp(1541560, 0, 'misc', 'uk'); // ありがたい
  await addSenseProp(1739410, 1, 'misc', 'uk'); // わけない
  await addSenseProp(1207610, 0, 'misc', 'uk'); // かける
  await addSenseProp(2424410, 0, 'misc', 'uk'); // やつめ
  await addSenseProp(1387080, 0, 'misc', 'uk'); // セミ
  await addSenseProp(1509350, 0, 'misc', 'uk'); // くせ
  await addSenseProp(1637460, 0, 'misc', 'uk'); // はやる

  // Add sense-prop "prt" (particle) to entries that need it
  await addSenseProp(2425930, 0, 'pos', 'prt'); // なの
  await addSenseProp(2457930, 0, 'pos', 'prt'); // わね

  // Additional delete-sense-prop call
  await deleteSenseProp(2629920, 'pos', 'adv-to'); // とん

  // Delete senses for なり (seq 2611370)
  await deleteSenses(2611370, () => true); // (constantly t) - delete all senses
  const sql = getConnection();
  // Set root_p to false
  await sql`
    UPDATE entry
    SET root_p = false
    WHERE seq = 2611370
  `;
  await deleteReading(2611370, '為り');

  // Rearrange readings for 包む (tsutsumu)
  await rearrangeReadingsConj(1584060, 'kana_text', 'つつ');
  await setCommon('kana_text', 1584060, 'つつむ', 6);

  // Rearrange readings for 増やす
  await rearrangeReadingsConj(1602880, 'kanji_text', '増や');

  // Delete noun sense for と
  await deleteSenses(1008490, (prop) => prop.tag === 'pos' && prop.text === 'n');

  // Delete prt sense for たい
  await deleteSenses(2017560, (prop) => prop.tag === 'pos' && prop.text === 'prt');

  // Delete adj stem conjugation for ない and しい
  await deleteConjugation(2029110, 2257550);
  await deleteConjugation(2086640, 2684620); // しい

  // Apply time-stamped errata in chronological order
  console.log('  Applying Feb 2017 errata...');
  await addErrataFeb17();
  console.log('  Applying Jan 2018 errata...');
  await addErrataJan18();
  console.log('  Applying Mar 2018 errata...');
  await addErrataMar18();
  console.log('  Applying Aug 2018 errata...');
  await addErrataAug18();
  console.log('  Applying Jan 2019 errata...');
  await addErrataJan19();
  console.log('  Applying Apr 2019 errata...');
  await addErrataApr19();
  console.log('  Applying Jan 2020 errata...');
  await addErrataJan20();
  console.log('  Applying Apr 2020 errata...');
  await addErrataApr20();
  console.log('  Applying Jul 2020 errata...');
  await addErrataJul20();
  console.log('  Applying Jan 2021 errata...');
  await addErrataJan21();
  console.log('  Applying May 2021 errata...');
  await addErrataMay21();
  console.log('  Applying Jan 2022 errata...');
  await addErrataJan22();
  console.log('  Applying Dec 2023 errata...');
  await addErrataDec23();
  console.log('  Applying Jan 2025 errata...');
  await addErrataJan25();
  console.log('  Applying counter errata...');
  await addErrataCounters();

  console.log('✓ Errata applied successfully');
}

/**
 * Adds glosses to a sense
 * Ported from dict-errata.lisp:158-166 add-gloss
 *
 * @param seq - Entry sequence number
 * @param ord - Sense order number
 * @param texts - Gloss texts to add
 */
export async function addGloss(
  seq: number,
  ord: number,
  ...texts: string[]
): Promise<void> {
  const sql = getConnection();

  // Get sense ID
  const senseResult = await sql<{ id: number }[]>`
    SELECT id FROM sense
    WHERE seq = ${seq} AND ord = ${ord}
    LIMIT 1
  `;

  if (senseResult.length === 0) return;

  const senseId = senseResult[0].id;

  // Get existing glosses
  const existingGlosses = await sql<{ text: string; ord: number }[]>`
    SELECT text, ord FROM gloss
    WHERE sense_id = ${senseId}
    ORDER BY ord DESC
  `;

  const glossesText = existingGlosses.map(g => g.text);
  let maxOrd = existingGlosses.length > 0 ? existingGlosses[0].ord + 1 : 0;

  for (const newText of texts) {
    if (!glossesText.includes(newText)) {
      await sql`
        INSERT INTO gloss (sense_id, text, ord)
        VALUES (${senseId}, ${newText}, ${maxOrd})
      `;
      maxOrd++;
    }
  }
}

/**
 * Errata updates from February 2017
 * Ported from dict-errata.lisp:583-657
 */
async function addErrataFeb17(): Promise<void> {
  await setCommon('kana_text', 2136890, 'とする', null);
  await setCommon('kana_text', 2100900, 'となる', null);
  await setCommon('kana_text', 1006200, 'すべき', null);
  await setCommon('kana_text', 2683060, 'なのです', null);
  await setCommon('kana_text', 2683060, 'なんです', null);
  await setCommon('kana_text', 1001200, 'おい', null);
  await setCommon('kana_text', 1001200, 'おおい', null);
  await setCommon('kanji_text', 1441840, '伝い', 0);
  await setCommon('kanji_text', 1409140, '身体', 0);
  await setCommon('kanji_text', 2830705, '身体', null);
  await setCommon('kana_text', 1009040, 'どきっと', 0);
  await setCommon('kana_text', 2261300, 'するべき', null);
  await setCommon('kana_text', 2215430, 'には', null);
  await setCommon('kana_text', 2210140, 'まい', null);
  await setCommon('kana_text', 2192950, 'なさい', null);
  await setCommon('kana_text', 2143350, 'かも', null);
  await setCommon('kana_text', 2106890, 'そのよう', null);
  await setCommon('kana_text', 2084040, 'すれば', null);
  await setCommon('kana_text', 2036080, 'うつ', null);
  await setCommon('kana_text', 1922760, 'という', null);
  await setCommon('kana_text', 1632520, 'ふん', null);
  await setCommon('kana_text', 1631750, 'がる', null);
  await setCommon('kana_text', 1394680, 'そういう', null);
  await setCommon('kana_text', 1208840, 'かつ', null);
  await setCommon('kana_text', 1011430, 'べき', null);
  await setCommon('kana_text', 1008340, 'である', null);
  await setCommon('kana_text', 1007960, 'ちんちん', null);
  await setCommon('kana_text', 1301230, 'さんなん', null);
  await setCommon('kanji_text', 1311010, '氏', 20);
  await setCommon('kana_text', 1311010, 'うじ', 20);
  await setCommon('kanji_text', 2101130, '氏', 21);
  await setCommon('kana_text', 1155180, 'いない', 10);
  await setCommon('kanji_text', 1609450, '思いきって', 0);
  await setCommon('kanji_text', 1309320, '思いきる', 0);
  await setCommon('kana_text', 1312880, 'メス', 15);
  await setCommon('kana_text', 1312880, 'めす', null);
  await setCommon('kana_text', 2061540, 'ぶっちゃける', 0);
  await setCommon('kana_text', 2034520, 'ですら', 0);
  await setCommon('kana_text', 1566210, 'いずれ', 9);

  await deleteSenseProp(2021030, 'misc', 'uk'); // 摂る（とる）
  await deleteSenseProp(1586730, 'misc', 'uk'); // 粗 (あら)
  await deleteSenseProp(1441400, 'misc', 'uk'); // 点く （つく）

  await addSenseProp(1569590, 0, 'misc', 'uk'); // 痙攣 けいれん
  await addSenseProp(1590540, 0, 'misc', 'uk'); // 仮名 かな
  await addSenseProp(1430200, 0, 'misc', 'uk'); // いただき

  await setPrimaryNokanji(1374550, false); // すごい
  await setPrimaryNokanji(1591900, false); // きれい
  await setPrimaryNokanji(1000230, false); // あかん
  await setPrimaryNokanji(1517810, false); // もやし
  await setPrimaryNokanji(1585410, false); // まま

  await addReading(1029150, 'えっち');
  await addReading(1363740, 'マネ');
  await setCommon('kana_text', 1363740, 'マネ', 9);

  await setCommon('kanji_text', 1000420, '彼の', null);
  await setCommon('kanji_text', 2219590, '元', 10);
  await setCommon('kana_text', 2219590, 'もと', 10);
  await setCommon('kana_text', 1394760, 'さほど', 0);
  await setCommon('kana_text', 1529560, 'なし', 10);
  await setCommon('kana_text', 1436830, 'ていない', null);
  await setCommon('kana_text', 1057580, 'さぼる', 0);
  await setCommon('kanji_text', 1402420, '走り', null);
  await setCommon('kana_text', 1402420, 'はしり', null);
  await setCommon('kana_text', 1209540, 'かる', null);
  await setCommon('kana_text', 1244840, 'かる', null);
  await setCommon('kana_text', 1280640, 'こうは', 0);
  await setCommon('kana_text', 1158960, 'いほう', 0);

  await deleteSenseProp(2122310, 'pos', 'prt'); // え
}

/**
 * Errata updates from January 2018
 * Ported from dict-errata.lisp:659-708
 */
async function addErrataJan18(): Promise<void> {
  await setCommon('kanji_text', 2067770, '等', null);
  await setCommon('kana_text', 2067770, 'ら', null);
  await setCommon('kanji_text', 1242230, '近よる', 38);
  await setCommon('kanji_text', 1315120, '字', 0);
  await setCommon('kana_text', 1315120, 'あざ', 0);
  await setCommon('kanji_text', 1315130, '字', 5);
  await setCommon('kana_text', 1315130, 'じ', 0);
  await setCommon('kana_text', 1005530, 'しっくり', 0);
  await setCommon('kana_text', 1554850, 'りきむ', null);
  await setCommon('kana_text', 2812650, 'ゲー', 0);
  await setCommon('kana_text', 2083340, 'やろう', 0);
  await setCommon('kana_text', 2083340, 'やろ', 0);
  await setCommon('kana_text', 1008730, 'とろ', null);
  await setCommon('kana_text', 1457840, 'ないかい', null);
  await setCommon('kana_text', 2829697, 'いかん', 0);
  await setCommon('kana_text', 2157330, 'おじゃま', 9);
  await setCommon('kana_text', 1199800, 'かいらん', null);
  await setCommon('kana_text', 2719580, 'いらん', 0);
  await setCommon('kana_text', 1808040, 'めちゃ', 0);
  await setCommon('kana_text', 1277450, 'すき', 9);
  await setCommon('kana_text', 1006460, 'ズレる', 0);
  await setCommon('kanji_text', 1522290, '本会議', 0);
  await setCommon('kana_text', 1522290, 'ほんかいぎ', 0);
  await setCommon('kana_text', 1220570, 'きたい', 10);
  await setCommon('kana_text', 1221020, 'きたい', 11);
  await setCommon('kana_text', 2083990, 'ならん', 0);
  await setCommon('kanji_text', 2518850, '切れ', 0);
  await setCommon('kanji_text', 1221900, '基地外', 0);
  await setCommon('kana_text', 1379380, 'せいと', 10);
  await setCommon('kanji_text', 1203280, '外に', null);
  await setCommon('kanji_text', 1383690, '後継ぎ', 0);
  await setCommon('kana_text', 2083600, 'すまん', 0);

  await addReading(1384840, 'キレ', { common: 0 });

  await deleteSenseProp(1303400, 'misc', 'uk'); // 撒く/まく
  await deleteSenseProp(1434020, 'misc', 'uk'); // 吊る/つる
  await deleteSenseProp(1196520, 'misc', 'uk'); // かすむ
  await deleteSenseProp(1414190, 'misc', 'uk'); // 大人しい

  await addSenseProp(1188380, 0, 'misc', 'uk'); // なんでもかんでも
  await addSenseProp(1258330, 0, 'misc', 'uk'); // いぬ
  await addSenseProp(2217330, 0, 'misc', 'uk'); // わい

  await setPrimaryNokanji(1258330, false); // いぬ
  await setPrimaryNokanji(1588930, false); // おかず

  await addSenseProp(1445160, 0, 'pos', 'ctr'); // 度
}

/**
 * Errata updates from March 2018
 * Ported from dict-errata.lisp:710-740
 */
async function addErrataMar18(): Promise<void> {
  await setPrimaryNokanji(1565440, false);

  await setCommon('kana_text', 1207610, 'かける', 0);
  await setCommon('kanji_text', 1236100, '強いる', null);
  await setCommon('kana_text', 1236100, 'しいる', null);
  await setCommon('kana_text', 1451750, 'おんなじ', 0);
  await setCommon('kanji_text', 2068330, '事故る', 0);
  await setCommon('kana_text', 1579260, 'きのう', 2);
  await setCommon('kanji_text', 2644980, '柔らかさ', 0);
  await setCommon('kana_text', 2644980, 'やわらかさ', 0);
  await setCommon('kana_text', 2083610, 'ベタ', 0);
  await setCommon('kana_text', 2083610, 'べた', 0);
  await setCommon('kana_text', 1119610, 'ベタ', null);
  await setCommon('kana_text', 1004840, 'コロコロ', 0);
  await setCommon('kana_text', 1257040, 'ケンカ', 0);
  await setCommon('kana_text', 1633840, 'ごとき', 0);

  await addSenseProp(1238460, 0, 'misc', 'uk'); // そば

  await deleteSenseProp(1896380, 'misc', 'uk'); // 出
  await deleteSenseProp(1157000, 'misc', 'uk'); // 易しい
  await deleteSenseProp(1576360, 'misc', 'uk'); // 逸れる

  await addSenseProp(1468900, 0, 'pos', 'ctr'); // 年生
  await addSenseProp(1241380, 0, 'pos', 'ctr'); // 斤
  await addSenseProp(1241380, 1, 'pos', 'ctr');

  // add sense for な
  await addNewSense(2029110, ['prt'], ['indicates な-adjective']);
}

/**
 * Errata updates from August 2018
 * Ported from dict-errata.lisp:742-760
 */
async function addErrataAug18(): Promise<void> {
  await setCommon('kana_text', 1593870, 'さらう', 0);
  await setCommon('kana_text', 2141690, 'ふざけんな', 0);
  await setCommon('kana_text', 1214770, 'かん', null);
  await setCommon('kanji_text', 1214770, '観', null);
  await setCommon('kanji_text', 2082780, '意味深', 0);
  await setCommon('kana_text', 2209180, 'とて', 0);
  await setCommon('kana_text', 1574640, 'ロバ', 0);

  await addReading(2722640, 'オケ');
  await addPrimaryNokanji(2722640, 'オケ');
  await setCommon('kana_text', 2722640, 'オケ', 0);
  await addSenseProp(2722640, 0, 'misc', 'uk');
  await addSenseProp(1527140, 0, 'misc', 'uk');

  await addSenseProp(1208870, 0, 'misc', 'uk'); // かなう

  await deleteSenseProp(1598660, 'misc', 'uk'); // とかす
}

/**
 * Errata updates from January 2019
 * Ported from dict-errata.lisp:762-785
 */
async function addErrataJan19(): Promise<void> {
  await setCommon('kanji_text', 2017470, '塗れ', 0);
  await setCommon('kana_text', 2722660, 'すげぇ', 0);

  await addSenseProp(2756830, 0, 'misc', 'uk');

  await deleteSenseProp(1604890, 'misc', 'uk'); // 目

  await addReading(1008370, 'デカい', { common: 0 });
  await addConjReading(1008370, 'デカい');
  await addReading(1572760, 'クドい');
  await addConjReading(1572760, 'クドい');
  await addReading(1003620, 'ギュっと');

  await deleteReading(2424520, '去る者は追わず、来たる者は拒まず');
  await deleteReading(2570040, '朝焼けは雨、夕焼けは晴れ');
  await deleteReading(2833961, '梅は食うとも核食うな、中に天神寝てござる');
  await deleteReading(2834318, '二人は伴侶、三人は仲間割れ');
  await deleteReading(2834363, '墨は餓鬼に磨らせ、筆は鬼に持たせよ');

  await setPrimaryNokanji(1631830, false); // くせに

  await deleteSenseProp(1270350, 'misc', 'arch'); // ござる
}

/**
 * Errata updates from April 2019
 * Ported from dict-errata.lisp:787-804
 */
async function addErrataApr19(): Promise<void> {
  await setCommon('kanji_text', 1538750, '癒やす', 0);
  await setCommon('kanji_text', 1538750, '癒す', 0);
  await setCommon('kana_text', 1538750, 'いやす', 0);
  await setCommon('kana_text', 2147610, 'いなくなる', 0);

  await setCommon('kana_text', 1346290, 'マス', 37);
  await addSenseProp(1346290, 3, 'misc', 'uk');
  await setPrimaryNokanji(1346290, true);

  await setPrimaryNokanji(1409110, false);

  await deleteReading(2081610, 'スレ違'); // this was added by mistake in the previous errata
  await setPrimaryNokanji(2081610, false); // this was added by mistake in the previous errata

  await addSenseProp(1615340, 0, 'misc', 'uk');
  await addSenseProp(1658480, 0, 'pos', 'ctr');
}

/**
 * Errata updates from January 2020
 * Ported from dict-errata.lisp:806-849
 */
async function addErrataJan20(): Promise<void> {
  await addReading(2839843, 'うえをしたへ');
  await deleteReading(2839843, 'うえをしたえ');
  await addReading(1593170, 'コケる');
  await addConjReading(1593170, 'コケる');

  await addSenseProp(1565100, 0, 'misc', 'uk');
  await deleteSenseProp(1632980, 'misc', 'uk');
  await deleteSenseProp(1715710, 'misc', 'uk');
  await setCommon('kana_text', 1715710, 'みたところ', null);
  await setCommon('kana_text', 2841254, 'からって', null);
  await setCommon('kana_text', 2028950, 'とは', null);
  await setCommon('kanji_text', 1292400, '再開', 13);
  await setCommon('kana_text', 1292400, 'さいかい', 13);
  await setCommon('kana_text', 1306200, 'しよう', 10);
  await setCommon('kana_text', 2056930, 'つまらなさそう', 0);
  await setCommon('kanji_text', 1164710, '一段落', null);
  await setCommon('kana_text', 1570220, 'すくむ', 0);
  await setCommon('kana_text', 1352130, 'うえ', 1);
  await setCommon('kana_text', 1502390, 'もん', 0);
  await setCommon('kana_text', 2780660, 'もん', 0);
  await setCommon('kana_text', 2653620, 'がち', 0);
  await setCommon('kana_text', 2653620, 'ガチ', 0);
  await setCommon('kana_text', 1135480, 'モノ', null);
  await setCommon('kana_text', 1003000, 'カラカラ', 0);

  await setPrimaryNokanji(1495000, false); // まずい

  // (delete-sense-prop 2036080 "misc" "uk") ;; 鬱
  await addSenseProp(2510160, 0, 'misc', 'obsc'); // 鬱ぐ

  await addSenseProp(1468900, 0, 'pos', 'ctr'); // 年生
  await addSenseProp(1469050, 0, 'pos', 'ctr'); // 年度
  await addSenseProp(1469050, 1, 'pos', 'ctr'); // 年度
  await addSenseProp(1469050, 2, 'pos', 'ctr');
  await addSenseProp(1284270, 0, 'pos', 'ctr'); // 号車

  await deleteSenseProp(1245280, 'pos', 'adj-no'); // 空 から (to make it even with そら)
  await deleteSenseProp(1392570, 'pos', 'adj-no'); // 前 ぜん

  await addSenseProp(1429740, 0, 'pos', 'suf'); // 長
  await addSenseProp(1429740, 1, 'pos', 'n');
  await deleteSenseProp(2647210, 'pos', 'suf');
}

/**
 * Errata updates from April 2020
 * Ported from dict-errata.lisp:851-875
 */
async function addErrataApr20(): Promise<void> {
  await setCommon('kana_text', 1225940, 'アリ', 0);
  await setCommon('kana_text', 1568080, 'ふくろう', 0);
  await setCommon('kana_text', 1025450, 'ウイルス', null);
  await setCommon('kana_text', 1025450, 'ウィルス', null);
  await setCommon('kana_text', 1004320, 'こうゆう', 0);
  await setCommon('kana_text', 1580290, 'おとめ', 0);

  await addSenseProp(1219510, 0, 'misc', 'uk');
  await addSenseProp(1616370, 0, 'misc', 'uk');

  await addNewSense(1315920, ['ctr'], ['hours (period of)']);

  await addSenseProp(1220540, 0, 'pos', 'ctr');
  await addSenseProp(1220540, 3, 'pos', 'ctr');
  await addSenseProp(1220540, 4, 'pos', 'ctr');
  await addSenseProp(1220540, 5, 'pos', 'ctr');
  await addSenseProp(1220540, 6, 'pos', 'ctr');

  await addSenseProp(2842087, 0, 'pos', 'ctr'); // パー
  await setCommon('kana_text', 2842087, 'パー', 0);

  await addSenseProp(1956530, 1, 'pos', 'n');
}

/**
 * Errata updates from July 2020
 * Ported from dict-errata.lisp:877-898
 */
async function addErrataJul20(): Promise<void> {
  await setCommon('kana_text', 2101130, 'し', null);
  await setCommon('kanji_text', 1982860, '代', 0);
  await setCommon('kana_text', 1367020, 'ひとけ', 0);
  await setCommon('kana_text', 1002190, 'おしり', 0);
  await setCommon('kana_text', 2085020, 'もどき', 0);

  await setPrimaryNokanji(1756600, false); // がんもどき

  await addReading(2217330, 'ワイ');
  await addPrimaryNokanji(2217330, 'ワイ');
  await addSenseProp(2217330, 0, 'misc', 'uk');
  await deleteSenseProp(2217330, 'misc', 'arch');

  await addReading(1103270, 'ぱんつ');

  await addSenseProp(1586290, 0, 'misc', 'uk'); // あげく

  await addSenseProp(1257260, 0, 'misc', 'uk'); // いやがらせ

  await rearrangeReadingsConj(1980880, 'kanji_text', 'かけ直');
}

/**
 * Errata updates from January 2021
 * Ported from dict-errata.lisp:900-918
 */
async function addErrataJan21(): Promise<void> {
  await setCommon('kana_text', 2124820, 'コロナウイルス', null);
  await setCommon('kana_text', 2846738, 'なん', null);
  await setCommon('kana_text', 2083720, 'っぽい', null);
  await setCommon('kanji_text', 1012980, '遣る', null);

  await addSenseProp(1411570, 0, 'pos', 'vs'); // 変わり映え
  await addSenseProp(1613860, 0, 'pos', 'ctr'); // 回戦
  await addSenseProp(1613860, 1, 'pos', 'ctr');

  await addSenseProp(2679820, 0, 'misc', 'uk'); // しっぽく
  await deleteSenseProp(1426680, 'misc', 'uk'); // 虫
  await addSenseProp(1590390, 0, 'misc', 'uk'); // かたどる

  await deleteSenseProp(1215240, 'pos', 'ctr');
  await addSenseProp(2145410, 0, 'pos', 'ctr'); // 間

  await replaceReading(2847494, 'いきはよいといかえりはこわい', 'いきはよいよいかえりはこわい');
}

/**
 * Errata updates from May 2021
 * Ported from dict-errata.lisp:920-929
 */
async function addErrataMay21(): Promise<void> {
  await addReading(1089590, 'どんまい');

  await setCommon('kana_text', 2848303, 'てか', 0);
  await setCommon('kanji_text', 1979920, '貴方', null);

  await deleteSenseProp(1547720, 'misc', 'uk'); // 来る
  await deleteSenseProp(1495770, 'misc', 'uk'); // 付ける
  await deleteSenseProp(2611890, 'misc', 'uk'); // 蒔く
}

/**
 * Errata updates from January 2022
 * Ported from dict-errata.lisp:931-951
 */
async function addErrataJan22(): Promise<void> {
  await addReading(1566420, 'ハメる');
  await addConjReading(1566420, 'ハメる');

  // these words had no kana in jmdict
  await addReading(1161240, 'いっかねん');

  await setCommon('kana_text', 2008650, 'そうした', null);
  await addSenseProp(1188270, 0, 'pos', 'n'); // 何か
  await deleteSenseProp(1188270, 'pos', 'pn');

  await deleteSenseProp(1240530, 'pos', 'ctr'); // 玉

  await addSenseProp(1247260, 0, 'pos', 'n-suf'); // 君　くん

  await setCommon('kana_text', 1001840, 'おにいちゃん', 0);
  await setCommon('kana_text', 1806840, 'がいそう', null);
  await setCommon('kana_text', 1639750, 'こだから', null);
}

/**
 * Errata updates from December 2023
 * Ported from dict-errata.lisp:953-983
 */
async function addErrataDec23(): Promise<void> {
  // (add-reading 2220325 "ヶ" :table 'kanji-text)
  // (add-reading 2220325 "ケ" :table 'kanji-text)
  // (delete-reading 2220325 "ヶ" :table 'kana-text)
  // (delete-reading 2220325 "ケ" :table 'kana-text)
  // (add-reading 2220325 "か")

  await addSenseProp(1180540, 0, 'misc', 'uk'); // おっす
  await deleteSenseProp(2854117, 'misc', 'uk'); // おき but I boost it later with synergy
  await deleteSenseProp(2859257, 'misc', 'uk'); // あれ (imperative of 有る)
  await deleteSenseProp(1198890, 'misc', 'uk'); // 解く

  await addSenseProp(2826371, 0, 'misc', 'uk');
  await deleteSenseProp(2826371, 'misc', 'rare'); // いつなりと

  // はいかん
  await setCommon('kana_text', 1625620, 'はいかん', null);
  await setCommon('kana_text', 1625610, 'はいかん', null);
  await setCommon('kana_text', 1681460, 'はいかん', null);

  await setCommon('kanji_text', 2855480, '乙女', 0);
  await setCommon('kana_text', 2855480, 'おとめ', 0);

  await setCommon('kana_text', 1930050, 'バラす', 0);
  await setCommon('kana_text', 1582460, 'ないかい', null);
  await setCommon('kana_text', 1202300, 'かいが', 0);

  await setCommon('kanji_text', 1328740, '狩る', 0);

  await setCommon('kana_text', 1009610, 'にも', 0);
}

/**
 * Errata updates from January 2025
 * Ported from dict-errata.lisp:985-1002
 */
async function addErrataJan25(): Promise<void> {
  await deleteReading(2028930, 'ヶ', { table: 'kana_text' });
  await deleteReading(2028930, 'ケ', { table: 'kana_text' });

  await deleteSenseProp(1138570, 'pos', 'ctr'); // ラウンド
  await addSenseProp(1138570, 1, 'pos', 'ctr');
  await addSenseProp(1138570, 2, 'pos', 'ctr');
  await addSenseProp(1138570, 3, 'pos', 'ctr');

  await setCommon('kana_text', 1001120, 'うんち', 0);
  await setCommon('kana_text', 1511600, 'かたかな', 0);
  await setCommon('kana_text', 1056400, 'サウンドトラック', 0);
  await setCommon('kana_text', 1510640, 'へん', 5);

  await replaceReading(2860664, 'こどもはおやのせなかをみてそだう', 'こどもはおやのせなかをみてそだつ');
  await replaceReadingConj(2863544, 'kana_text', 'みぎにでるのは', 'みぎにでるものは');
}

/**
 * Errata updates for counter words
 * Ported from dict-errata.lisp:1005-1089
 */
async function addErrataCounters(): Promise<void> {
  const sql = getConnection();

  await deleteReading(1299960, 'さんかい');

  // mapc 'set-reading (select-dao 'kanji-text (:= 'seq 1299960))
  const kanjiTexts = await sql<{ id: number; text: string; ord: number }[]>`
    SELECT id, text, ord FROM kanji_text WHERE seq = 1299960
  `;
  for (const kt of kanjiTexts) {
    const readingValue = `${kt.text} ${kt.ord}`;
    await sql`
      UPDATE kanji_text
      SET reading = ${readingValue}
      WHERE id = ${kt.id}
    `;
  }

  await addReading(2081610, 'タテ');

  await addSenseProp(1427420, 0, 'pos', 'ctr'); // 丁目
  await addSenseProp(1397450, 0, 'pos', 'ctr'); // 組
  await addSenseProp(1397450, 1, 'pos', 'ctr'); // 組
  await addSenseProp(1351270, 0, 'pos', 'ctr'); // 章
  await addSenseProp(1351270, 1, 'pos', 'n'); // 章
  await addSenseProp(1490430, 0, 'pos', 'ctr'); // 秒
  await addSenseProp(1490430, 1, 'pos', 'ctr'); // 秒
  await addSenseProp(2020680, 0, 'pos', 'ctr'); // 時
  await addSenseProp(1502840, 0, 'pos', 'ctr'); // 分
  await addSenseProp(1502840, 1, 'pos', 'ctr'); // 分
  await addSenseProp(1373990, 0, 'pos', 'ctr'); // 世紀
  await addSenseProp(1281690, 0, 'pos', 'ctr'); // 行
  await addSenseProp(1281690, 1, 'pos', 'n');
  await addSenseProp(1042610, 1, 'pos', 'ctr'); // キロ
  await addSenseProp(1042610, 2, 'pos', 'ctr');
  await addSenseProp(1100610, 0, 'pos', 'ctr'); // パーセント

  await addNewSense(1583470, ['ctr'], ['counter for dishes']); // 品（しな）

  await addSenseProp(1411070, 0, 'pos', 'ctr'); // 袋
  await addSenseProp(1411070, 1, 'pos', 'n');

  await addSenseProp(1328810, 0, 'pos', 'ctr'); // 種

  await addSenseProp(1284220, 0, 'pos', 'ctr'); // 号
  await addSenseProp(1284220, 1, 'pos', 'n');
  await addSenseProp(1284220, 1, 'pos', 'n-suf');
  await addSenseProp(1482360, 0, 'pos', 'ctr'); // 番地
  await addSenseProp(2022640, 0, 'pos', 'ctr'); // 番
  await addSenseProp(1175570, 0, 'pos', 'ctr'); // 円
  await addSenseProp(1175570, 1, 'pos', 'n');
  await addSenseProp(1315130, 0, 'pos', 'ctr'); // 字
  await addSenseProp(1315130, 1, 'pos', 'n');
  await addSenseProp(1199640, 0, 'pos', 'ctr'); // 回転

  await addSenseProp(1047880, 0, 'pos', 'ctr'); // ケース
  await addSenseProp(1047880, 1, 'pos', 'n');

  await addSenseProp(1244080, 0, 'pos', 'ctr'); // 区
  await addSenseProp(1244080, 1, 'pos', 'ctr'); // 区
  await addSenseProp(1239700, 0, 'pos', 'ctr'); // 曲

  await addSenseProp(1294940, 0, 'pos', 'ctr'); // 才 歳
  await addSenseProp(1294940, 1, 'pos', 'suf');

  await addSenseProp(1575510, 0, 'pos', 'ctr'); // コマ
  await addSenseProp(1575510, 1, 'pos', 'n');

  await addSenseProp(1505390, 0, 'pos', 'ctr'); // 文字

  await addSenseProp(1101700, 0, 'pos', 'ctr'); // パック
  await addSenseProp(1120410, 0, 'pos', 'ctr'); // ページ
  await addSenseProp(1956400, 0, 'pos', 'ctr'); // 集
  await addSenseProp(1333450, 0, 'pos', 'ctr'); // 週
  await addSenseProp(1480050, 0, 'pos', 'ctr'); // 反
  await addSenseProp(1480050, 1, 'pos', 'ctr'); // 反
  await addSenseProp(1480050, 2, 'pos', 'ctr'); // 反

  await addSenseProp(1956530, 0, 'pos', 'ctr'); // 寸
  await addSenseProp(1324110, 0, 'pos', 'ctr'); // 尺
  await addSenseProp(1324110, 1, 'pos', 'n');
  await addSenseProp(1382450, 0, 'pos', 'ctr'); // 石
  await addSenseProp(1382450, 1, 'pos', 'ctr');

  await addSenseProp(1253800, 1, 'pos', 'ctr'); // 桁

  await addSenseProp(1297240, 0, 'pos', 'ctr'); // 作

  await addNewSense(2262420, ['ctr'], ['counter for strings']); // 弦

  await addSenseProp(1368480, 0, 'pos', 'ctr'); // 人前
  await addGloss(1368480, 0, 'for N people');

  await addSenseProp(1732510, 1, 'pos', 'ctr'); // 番手
  await addSenseProp(1732510, 2, 'pos', 'ctr');
  await addSenseProp(2086480, 1, 'pos', 'ctr'); // 頭身

  await addSenseProp(1331080, 0, 'pos', 'ctr'); // 周忌
}
