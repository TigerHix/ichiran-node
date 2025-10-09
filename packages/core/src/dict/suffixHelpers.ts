// ichiran/dict/suffixHelpers - Helper functions for suffix processing
// Extracted from dict-grammar.ts Lines 12-490
// Moved from grammar/ to dict/ to resolve circular dependencies

import { getConnection } from '../conn.js';
import { testWord, asHiragana } from '../characters.js';
import {
  WEAK_CONJ_FORMS, testConjProp, skipByConjData
} from './errata.js';
import type {
  KanaText, KanjiText, ConjData, AnyWord, ConjugationWithProp, Conjugation
} from '../types.js';

// Import from same directory (dict/) - no circular dependency!
import { wordConjData } from './conjugation.js';
import { findWordFull } from './lookup.js';

// ============================================================================
// KANA FORM HELPERS
// ============================================================================

// Line 12-17: defun get-kana-forms-conj-data-filter
async function getKanaFormsConjDataFilter(conjData: ConjData[]): Promise<number[]> {
  if (skipByConjData(conjData)) return [];

  const result: number[] = [];
  for (const cd of conjData) {
    const prop = cd.prop;
    if (!testConjProp(prop, WEAK_CONJ_FORMS)) {
      result.push(prop.conjId);
    }
  }
  return result;
}

// Line 19-32: defun get-kana-forms*
async function getKanaForms_(seq: number): Promise<KanaText[]> {
  const sql = getConnection();

  // Union of direct kana-text and conjugated forms
  const results = await sql<KanaText[]>`
    SELECT kt.* FROM kana_text kt WHERE kt.seq = ${seq}
    UNION
    SELECT kt.* FROM kana_text kt
    LEFT JOIN conjugation conj ON conj.seq = kt.seq
    WHERE conj.from = ${seq}
  `;

  // Collect all (seq, from) pairs for conjugated forms
  const conjPairs: Array<{ ktSeq: number; from: number }> = [];
  for (const kt of results) {
    if (kt.seq !== seq) {
      conjPairs.push({ ktSeq: kt.seq, from: seq });
    }
  }

  // Batch query all conjugation data at once
  let conjDataMap = new Map<number, ConjData[]>();
  if (conjPairs.length > 0) {
    const seqs = conjPairs.map(p => p.ktSeq);
    const froms = conjPairs.map(p => p.from);

    const allConjs = await sql<ConjugationWithProp[]>`
      SELECT c.*, cp.*
      FROM conjugation c
      LEFT JOIN conj_prop cp ON cp.conj_id = c.id
      WHERE c.seq = ANY(${seqs}) AND c.from = ANY(${froms})
    `;

    // Group by seq
    for (const row of allConjs) {
      const conjData: ConjData = {
        seq: row.seq,
        from: row.from,
        via: row.via,
        prop: {
          id: row.id ?? 0,
          conjId: row.conjId ?? 0,
          conjType: row.conjType ?? 0,
          pos: row.pos ?? '',
          neg: row.neg ?? null,
          fml: row.fml ?? null,
        },
        srcMap: [],
      };

      if (!conjDataMap.has(row.seq)) {
        conjDataMap.set(row.seq, []);
      }
      conjDataMap.get(row.seq)!.push(conjData);
    }
  }

  const output: KanaText[] = [];

  for (const kt of results) {
    if (kt.seq === seq) {
      // Root form
      kt.conjugations = ':root';
      output.push(kt);
    } else {
      // Conjugated form - use batched data
      const conjData = conjDataMap.get(kt.seq) || [];
      const conjIds = await getKanaFormsConjDataFilter(conjData);
      if (conjIds.length > 0) {
        kt.conjugations = conjIds;
        output.push(kt);
      }
    }
  }

  return output;
}

// Line 34-36: defun get-kana-forms
export async function getKanaForms(seq: number): Promise<KanaText[]> {
  const result = await getKanaForms_(seq);
  if (result.length === 0) {
    console.warn(`No kana forms found for: ${seq}`);
  }
  return result;
}

// Line 38-42: defun get-kana-form
export async function getKanaForm(seq: number, text: string, conj?: ':root' | number[]): Promise<KanaText | null> {
  const sql = getConnection();

  const results = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE text = ${text} AND seq = ${seq} LIMIT 1
  `;

  if (results.length > 0) {
    const res = results[0];
    if (conj !== undefined) {
      res.conjugations = conj;
    }
    return res;
  }

  return null;
}

// ============================================================================
// WORD FINDING HELPERS
// ============================================================================

// Line 44-51: defun find-word-with-conj-prop
export async function findWordWithConjProp(
  wordstr: string,
  filterFn: (cdata: ConjData) => boolean,
  options: { allowRoot?: boolean } = {}
): Promise<AnyWord[]> {
  // Use findWordFull to handle complex forms (e.g., "特化して" which might be "特化する" + suffix)
  // Note: This uses lazy import to avoid circular dependency
  const allWords = await findWordFull(wordstr);
  const result: AnyWord[] = [];

  for (const word of allWords) {
    const conjData = await wordConjData(word as any);
    const conjDataFiltered = conjData.filter(filterFn);
    const conjIds = conjDataFiltered.map(cdata => cdata.prop.conjId);

    if (conjDataFiltered.length > 0 || (conjData.length === 0 && options.allowRoot)) {
      // Clone the word to avoid mutating shared objects
      const wordClone = { ...word, conjugations: conjIds.length > 0 ? conjIds : null };
      result.push(wordClone);
    }
  }

  return result;
}

// Line 53-56: defun find-word-with-conj-type
export async function findWordWithConjType(word: string, ...conjTypes: number[]): Promise<AnyWord[]> {
  return findWordWithConjProp(word, (cdata) => {
    return conjTypes.includes(cdata.prop.conjType);
  });
}

// Line 58-73: defun pair-words-by-conj
export async function pairWordsByConj(...wordGroups: Array<KanjiText | KanaText>[]): Promise<Array<Array<KanjiText | KanaText | null>>> {
  const sql = getConnection();

  // Collect all conjugation IDs from all words
  const allConjIds = new Set<number>();
  for (const wordGroup of wordGroups) {
    for (const word of wordGroup) {
      const conjIds = word.conjugations;
      if (conjIds && conjIds !== ':root') {
        for (const conjId of conjIds) {
          allConjIds.add(conjId);
        }
      }
    }
  }

  // Batch query all conjugations at once
  const conjMap = new Map<number, { from: number; via: number }>();
  if (allConjIds.size > 0) {
    const ids = Array.from(allConjIds);
    const conjs = await sql<Pick<Conjugation, 'id' | 'from' | 'via'>[]>`
      SELECT id, "from", via FROM conjugation WHERE id = ANY(${ids})
    `;

    for (const conj of conjs) {
      conjMap.set(conj.id, {
        from: conj.from || 0,
        via: conj.via === null ? 0 : conj.via
      });
    }
  }

  // Key function: sort conjugation seqs (now uses cached data)
  const key = (word: KanjiText | KanaText): string => {
    const conjIds = word.conjugations;
    if (!conjIds || conjIds === ':root') return '';

    const pairs: Array<[number, number]> = [];
    for (const conjId of conjIds) {
      const conj = conjMap.get(conjId);
      if (conj) {
        pairs.push([conj.from, conj.via]);
      }
    }

    pairs.sort((a, b) => a[0] - b[0] || a[1] - b[1]);
    return JSON.stringify(pairs);
  };

  const bag = new Map<string, Array<KanjiText | KanaText | null>>();

  for (let idx = 0; idx < wordGroups.length; idx++) {
    const wg = wordGroups[idx];
    for (const word of wg) {
      const k = key(word);
      let arr = bag.get(k);
      if (!arr) {
        arr = new Array(wordGroups.length).fill(null);
        bag.set(k, arr);
      }
      arr[idx] = word;
    }
  }

  return Array.from(bag.values());
}

// Line 75-77: defun find-word-seq
export async function findWordSeq(word: string, ...seqs: number[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<Array<KanjiText | KanaText>>`
    SELECT * FROM ${sql(table)} WHERE text = ${word} AND seq = ANY(${seqs})
  `;

  return results;
}

// Line 79-87: defun find-word-conj-of
export async function findWordConjOf(word: string, ...seqs: number[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  // Direct matches
  const direct = await findWordSeq(word, ...seqs);

  // Conjugated forms
  const conjugated = await sql<Array<KanjiText | KanaText>>`
    SELECT kt.* FROM ${sql(table)} kt
    INNER JOIN conjugation conj ON kt.seq = conj.seq
    WHERE conj.from = ANY(${seqs}) AND kt.text = ${word}
  `;

  // Union by id
  const seen = new Set<number>();
  const result: Array<KanjiText | KanaText> = [];

  for (const item of [...direct, ...conjugated]) {
    if (!seen.has(item.id)) {
      seen.add(item.id);
      result.push(item);
    }
  }

  return result;
}

// Line 89-95: defun find-word-with-pos
export async function findWordWithPos(word: string, ...posi: string[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<Array<KanjiText | KanaText>>`
    SELECT DISTINCT kt.* FROM ${sql(table)} kt
    INNER JOIN sense_prop sp ON sp.seq = kt.seq AND sp.tag = 'pos'
    WHERE kt.text = ${word} AND sp.text = ANY(${posi})
  `;

  return results;
}

// Line 97-100: defun or-as-hiragana
export async function orAsHiragana<T>(fn: (...args: any[]) => Promise<T[]>, word: string, ...args: any[]): Promise<T[]> {
  let result = await fn(word, ...args);
  if (result.length === 0) {
    result = await fn(asHiragana(word), ...args);
  }
  return result;
}

// Line 102-106: defun find-word-with-suffix
export async function findWordWithSuffix(
  wordstr: string,
  suffixClasses: string[],
  suffixClass: Map<number, string> | null
): Promise<Array<KanjiText | KanaText>> {
  // Use findWordFull to handle complex forms
  // Note: This uses lazy import to avoid circular dependency
  const words = await findWordFull(wordstr);
  const result: Array<KanjiText | KanaText> = [];

  for (const word of words) {
    const seq = word.seq;
    if (Array.isArray(seq)) {
      const lastSeq = seq[seq.length - 1];
      const sufClass = suffixClass?.get(lastSeq);
      if (sufClass && suffixClasses.includes(sufClass)) {
        result.push(word);
      }
    }
  }

  return result;
}
