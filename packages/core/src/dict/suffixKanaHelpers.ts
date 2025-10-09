// Helper functions for suffix cache initialization
// Extracted from suffixHelpers.ts to break circular dependency
// These functions are only used by grammar/suffixCache.ts

import { getConnection } from '../conn.js';
import { testWord } from '../characters.js';
import {
  WEAK_CONJ_FORMS, testConjProp, skipByConjData
} from './errata.js';
import type {
  KanaText, KanjiText, ConjData, ConjugationWithProp
} from '../types.js';

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

// Helper function used by findWordConjOf
async function findWordSeq(word: string, ...seqs: number[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<Array<KanjiText | KanaText>>`
    SELECT * FROM ${sql(table)} WHERE text = ${word} AND seq = ANY(${seqs})
  `;

  return results;
}
