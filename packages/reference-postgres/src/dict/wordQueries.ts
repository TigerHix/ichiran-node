// dict/wordQueries - Low-level word database queries
// These functions have NO grammar dependencies and can be safely used by grammar modules
// to avoid circular dependencies.

import { getConnection } from '../conn.js';
import { testWord } from '../characters.js';
import { startTimer } from './profiling.js';
import { getText } from './utils.js';
import { getKana } from './readings.js';
import type { KanjiText, KanaText, AnyWord, CompoundText, SimpleWord, ProxyText } from '../types.js';
import { isCompoundText, isSimpleWord } from '../types.js';

// Re-export from lookup.ts for consistency
export const MAX_WORD_LENGTH = 50;

// ============================================================================
// LOW-LEVEL WORD QUERIES (no suffix/grammar processing)
// ============================================================================

/**
 * Find words by exact text match in kanji_text or kana_text tables.
 * This is the lowest-level query - no suffix processing, no counters, no hiragana variants.
 * Used by grammar modules to avoid circular dependencies.
 *
 * @param word - The text to search for
 * @param options.rootOnly - If true, only return root words (entry.root_p = true)
 * @returns Array of matching KanjiText or KanaText objects
 */
export async function findWord(word: string, options: { rootOnly?: boolean } = {}): Promise<(KanjiText | KanaText)[]> {
  const endTimer = startTimer('findWord');

  if (word.length > MAX_WORD_LENGTH) {
    console.warn(`findWord: input too long (${word.length} > ${MAX_WORD_LENGTH}) - ignoring`);
    endTimer();
    return [];
  }

  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  if (options.rootOnly) {
    const results = await sql<(KanjiText | KanaText)[]>`
      SELECT wt.* FROM ${sql(table)} wt
      INNER JOIN entry ON wt.seq = entry.seq
      WHERE wt.text = ${word} AND entry.root_p = true
    `;
    endTimer();
    return results;
  } else {
    const results = await sql<(KanjiText | KanaText)[]>`
      SELECT * FROM ${sql(table)} WHERE text = ${word}
    `;
    endTimer();
    return results;
  }
}

/**
 * Find words by sequence numbers.
 *
 * @param word - The text to search for
 * @param seqs - Sequence numbers to match
 * @returns Array of matching words
 */
export async function findWordBySeqs(word: string, seqs: number[]): Promise<(KanjiText | KanaText)[]> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<(KanjiText | KanaText)[]>`
    SELECT * FROM ${sql(table)} WHERE text = ${word} AND seq = ANY(${seqs})
  `;

  return results;
}

// ============================================================================
// WORD COMPOSITION
// ============================================================================

/**
 * Create a compound word by joining two words.
 * Moved here from lookup.ts to break circular dependency with grammar modules.
 * Has no grammar dependencies - only uses getText and getKana.
 */
export async function adjoinWord(
  word1: AnyWord,
  word2: AnyWord,
  options: {
    text?: string;
    kana?: string;
    scoreMod?: number | ((score: number) => number);
    scoreBase?: SimpleWord | ProxyText;
  } = {}
): Promise<CompoundText> {
  const text = options.text || (getText(word1) + getText(word2));
  const kana = options.kana || (await getKana(word1) + await getKana(word2));
  const scoreMod = options.scoreMod ?? 0;

  // Helper to extract seq from a word (handles compounds recursively)
  const getSeqs = (word: AnyWord): number[] => {
    if (isCompoundText(word)) {
      // CompoundText has seq as number[]
      return word.seq;
    }
    if (isSimpleWord(word)) {
      // SimpleWord (KanjiText/KanaText) has seq as number
      return [word.seq];
    }
    return [];
  };

  // Check if word1 is already a compound
  if (isCompoundText(word1)) {
    // Extend existing compound
    word1.text = text;
    word1.kana = kana;
    word1.words.push(word2 as any);
    word1.scoreMod = (Array.isArray(word1.scoreMod)
      ? [scoreMod, ...word1.scoreMod]
      : [scoreMod, word1.scoreMod]) as any;
    // Update seq array to include word2's seq
    word1.seq = [...word1.seq, ...getSeqs(word2)];
    return word1;
  }

  // Create new compound
  return {
    text,
    kana,
    primary: word1 as any,
    words: [word1 as any, word2 as any],
    seq: [...getSeqs(word1), ...getSeqs(word2)],
    scoreMod,
    scoreBase: options.scoreBase
  };
}

// Note: For conjugation data queries, import wordConjData directly from ./conjugation.js
// It has no grammar dependencies and is safe to use from grammar modules.
