// Lookup functions extracted from dict.ts
// Handles word lookup, substring hash, word readings, and compound word creation

import { AsyncLocalStorage } from 'async_hooks';
import { getConnection } from '../conn.js';
import { testWord, asHiragana, consecutiveCharGroups } from '../characters.js';
import { findWordSuffix } from '../grammar/suffixMatcher.js';
import { findCounter } from './counters.js';
import { findWord as findWordLowLevel } from './wordQueries.js';
import type {
  KanjiText,
  KanaText,
  ProxyText,
  SubstringHash,
  SubstringHashEntry
} from '../types.js';
import { isSimpleWord } from '../types.js';

import { startTimer } from './profiling.js';

// =============================================================================
// CONSTANTS
// =============================================================================

// Line 486: defparameter *max-word-length*
export const MAX_WORD_LENGTH = 50;

// =============================================================================
// SUBSTRING HASH CONTEXT
// =============================================================================

// Substring hash for caching word lookups (Lisp: *substring-hash*)
const substringHashStorage = new AsyncLocalStorage<SubstringHash>();

function getSubstringHash(): SubstringHash | undefined {
  return substringHashStorage.getStore();
}

function withSubstringHash<T>(hash: SubstringHash, fn: () => Promise<T>): Promise<T> {
  return substringHashStorage.run(hash, fn);
}

// =============================================================================
// WORD LOOKUP FUNCTIONS
// =============================================================================

// Line 489-499: defun find-word
export async function findWord(word: string, options: { rootOnly?: boolean } = {}): Promise<(KanjiText | KanaText)[]> {
  const endTimer = startTimer('findWord');

  if (word.length > MAX_WORD_LENGTH) {
    endTimer();
    return [];
  }

  // Check substring hash first (Lisp: *substring-hash*)
  const substringHash = getSubstringHash();
  if (substringHash && !options.rootOnly) {
    const cached = substringHash.get(word);
    if (cached) {
      endTimer();
      // Return instances from cached data
      return cached.map(item => item.row as (KanjiText | KanaText));
    }
  }

  // Delegate to low-level query
  const results = await findWordLowLevel(word, options);
  endTimer();
  return results;
}

// Line 501-518: defun find-substring-words
export async function findSubstringWords(
  str: string,
  sticky: number[] = []
): Promise<SubstringHash> {
  const endTimer = startTimer('findSubstringWords');
  const sql = getConnection();
  const substringHash = new Map<string, SubstringHashEntry[]>();
  const kanaKeys: string[] = [];
  const kanjiKeys: string[] = [];

  // Build all substrings
  for (let start = 0; start < str.length; start++) {
    if (sticky.includes(start)) continue;

    for (let end = start + 1; end <= Math.min(str.length, start + MAX_WORD_LENGTH); end++) {
      if (sticky.includes(end)) continue;

      const part = str.slice(start, end);
      substringHash.set(part, []);

      if (testWord(part, 'kana')) {
        kanaKeys.push(part);
      } else {
        kanjiKeys.push(part);
      }
    }
  }

  // Query for kana matches
  const uniqueKanaKeys = [...new Set(kanaKeys)];
  if (uniqueKanaKeys.length > 0) {
    const kanaResults = await sql<KanaText[]>`
      SELECT * FROM kana_text WHERE text IN ${sql(uniqueKanaKeys)}
    `;

    for (const row of kanaResults) {
      const existing = substringHash.get(row.text) || [];
      existing.unshift({ table: 'kana_text', row }); // Use unshift to match Lisp's push (adds to front)
      substringHash.set(row.text, existing);
    }
  }

  // Query for kanji matches
  const uniqueKanjiKeys = [...new Set(kanjiKeys)];
  if (uniqueKanjiKeys.length > 0) {
    const kanjiResults = await sql<KanjiText[]>`
      SELECT * FROM kanji_text WHERE text IN ${sql(uniqueKanjiKeys)}
    `;

    for (const row of kanjiResults) {
      const existing = substringHash.get(row.text) || [];
      existing.unshift({ table: 'kanji_text', row }); // Use unshift to match Lisp's push (adds to front)
      substringHash.set(row.text, existing);
    }
  }

  endTimer();
  return substringHash;
}

// Line 520-534: defun find-words-seqs
export async function findWordsSeqs(
  words: string | string[],
  seqs: number | number[]
): Promise<(KanjiText | KanaText)[]> {
  const sql = getConnection();

  const wordList = Array.isArray(words) ? words : [words];
  const seqList = Array.isArray(seqs) ? seqs : [seqs];

  const kanaWords = wordList.filter(w => testWord(w, 'kana'));
  const kanjiWords = wordList.filter(w => !testWord(w, 'kana'));

  const results: (KanjiText | KanaText)[] = [];

  if (kanjiWords.length > 0) {
    const kw = await sql<KanjiText[]>`
      SELECT * FROM kanji_text
      WHERE text IN ${sql(kanjiWords)} AND seq IN ${sql(seqList)}
    `;
    results.push(...kw);
  }

  if (kanaWords.length > 0) {
    const rw = await sql<KanaText[]>`
      SELECT * FROM kana_text
      WHERE text IN ${sql(kanaWords)} AND seq IN ${sql(seqList)}
    `;
    results.push(...rw);
  }

  return results;
}

// Line 536-546: defun word-readings
export async function wordReadings(word: string): Promise<{ readings: string[]; romanized: string[] }> {
  const sql = getConnection();

  // Check if word is kana
  const kanaSeq = await sql<{ seq: number }[]>`
    SELECT seq FROM kana_text WHERE text = ${word}
  `;

  if (kanaSeq.length > 0) {
    // Word is already kana
    return { readings: [word], romanized: [] }; // TODO: Add romanization
  }

  // Word is kanji, get kana readings
  const kanjiSeq = await sql<{ seq: number }[]>`
    SELECT seq FROM kanji_text WHERE text = ${word}
  `;

  if (kanjiSeq.length === 0) {
    return { readings: [], romanized: [] };
  }

  const seqs = kanjiSeq.map(r => r.seq);
  const readings = await sql<{ text: string }[]>`
    SELECT text FROM kana_text
    WHERE seq IN ${sql(seqs)}
    ORDER BY id
  `;

  const readingTexts = readings.map(r => r.text);
  return { readings: readingTexts, romanized: [] }; // TODO: Add romanization
}

// Line 592-604: defun find-word-as-hiragana
export async function findWordAsHiragana(
  str: string,
  options: { exclude?: number[]; finder?: (s: string) => Promise<any[]> } = {}
): Promise<ProxyText[]> {
  const asHir = asHiragana(str);

  if (str === asHir) {
    return [];
  }

  const finder = options.finder || ((s: string) => findWord(s, { rootOnly: true }));
  const words = await finder(asHir);

  if (!words || words.length === 0) {
    return [];
  }

  const exclude = options.exclude || [];
  const result: ProxyText[] = [];

  for (const w of words) {
    if (!exclude.includes(w.seq)) {
      result.push({
        source: w,
        text: str,
        kana: str,
        conjugations: w.conjugations
      });
    }
  }

  return result;
}

// Line 632-652: defgeneric adjoin-word
// Re-export from wordQueries to maintain backward compatibility
export { adjoinWord } from './wordQueries.js';

// Line 1052-1067: defun find-word-full
/**
 * Returns heterogeneous array of all word matches.
 *
 * This function matches Lisp's duck-typed behavior where heterogeneous lists
 * are combined for performance (avoids duplicate database queries).
 *
 * **Return types:**
 * - Simple words: `KanjiText | KanaText`
 * - Compound words with suffixes: `CompoundText`
 * - Hiragana proxies: `ProxyText`
 * - Counter expressions: `CounterText`
 *
 * **Consumers should either:**
 * - Accept all types (e.g., wrap everything in Segments)
 * - Filter with type guards to desired types
 *
 * **For type-safe usage of simple words only, use `findSimpleWords()` instead.**
 *
 * @param word - The word to search for
 * @param options.asHiragana - Include hiragana variants (for katakana words)
 * @param options.counter - Include counter expressions (':auto' or number position)
 * @returns Heterogeneous array of all matching word objects
 */
export async function findWordFull(
  word: string,
  options: {
    asHiragana?: boolean;
    counter?: ':auto' | number;
  } = {}
): Promise<any[]> {
  const endTimer = startTimer('findWordFull');

  const simpleWords = await findWord(word);
  const results: any[] = [...simpleWords];

  // Add suffix matches (returns CompoundText[])
  // Note: Suffix context is inherited via AsyncLocalStorage if set by caller
  const suffixMatches = await findWordSuffix(word, {
    matches: simpleWords
  });
  results.push(...suffixMatches);

  // Add hiragana variants
  if (options.asHiragana) {
    const hiraganaWords = await findWordAsHiragana(word, {
      exclude: simpleWords.map(w => w.seq)
    });
    results.push(...hiraganaWords);
  }

  // Add counter matches (returns CounterText[])
  if (options.counter) {
    if (options.counter === ':auto') {
      const groups = consecutiveCharGroups('number', word);
      if (groups.length > 0) {
        const numStart = groups[0][0];
        const numEnd = groups[0][1];
        const number = word.slice(numStart, numEnd);
        const counter = word.slice(numEnd);
        const counterWords = await findCounter(number, counter);
        results.push(...counterWords);
      }
    } else {
      const number = word.slice(0, options.counter);
      const counter = word.slice(options.counter);
      const counterWords = await findCounter(number, counter, {
        unique: simpleWords.length === 0
      });
      results.push(...counterWords);
    }
  }

  endTimer();
  return results;
}

/**
 * Type-safe variant of findWordFull that returns only simple words.
 *
 * Filters out compound words, proxy words, and counter expressions.
 * Use this when you only need dictionary entries with seq property.
 *
 * @param word - The word to search for
 * @returns Array of simple word objects (KanjiText | KanaText)
 */
export async function findSimpleWords(word: string): Promise<(KanjiText | KanaText)[]> {
  const all = await findWordFull(word);
  return all.filter((w): w is KanjiText | KanaText => isSimpleWord(w));
}

// Export context utilities for external use
export { getSubstringHash, withSubstringHash };
