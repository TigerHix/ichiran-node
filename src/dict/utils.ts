// Pure utility functions extracted from dict.ts
// These have no database dependencies and can be used independently

import { testWord } from '../characters.js';
import type {
  AnyWord,
  Segment
} from '../types.js';
import {
  isSegment,
  isCompoundText,
  isKanjiText,
  isKanaText,
  isProxyText,
  isSimpleWord,
  isWordWithText,
  isCounterText
} from '../types.js';

// =============================================================================
// TYPE UTILITIES
// =============================================================================

export type WordType = 'kanji' | 'kana' | 'gap';

// Line 12-24: defgeneric word-type and related functions
export function getWordType(obj: AnyWord | Segment | null | undefined): WordType {
  if (obj === null || obj === undefined) return 'gap';
  // Handle Segment objects - extract word
  if (isSegment(obj)) return getWordType(obj.word);
  // Line 630: compound-text word-type recurses to primary
  if (isCompoundText(obj)) return getWordType(obj.primary);
  // CounterText has its own wordType() method
  if (isCounterText(obj) && typeof (obj as any).wordType === 'function') {
    return (obj as any).wordType();
  }
  // Distinguish kanji-text vs kana-text by which best_* field they have
  // kanji-text has bestKana, kana-text has bestKanji
  if (isKanjiText(obj)) return 'kanji';
  if (isKanaText(obj)) return 'kana';
  // Fallback: test the text content
  if (isWordWithText(obj)) {
    return testWord(obj.text, 'kana') ? 'kana' : 'kanji';
  }
  return 'gap';
}

// =============================================================================
// TEXT EXTRACTION
// =============================================================================

export function getText(obj: AnyWord | Segment | null | undefined): string {
  if (!obj) return '';
  // Handle Segment objects
  if (isSegment(obj)) {
    // This is a Segment - get text from the wrapped word
    const word = obj.word;
    if (!word) return '';
    // CounterText has a getText() method that combines numberText + text
    if (isCounterText(word) && typeof (word as any).getText === 'function') {
      return (word as any).getText();
    }
    return word.text || '';
  }
  // CounterText has a getText() method that combines numberText + text
  if (isCounterText(obj) && typeof (obj as any).getText === 'function') {
    return (obj as any).getText();
  }
  if (isWordWithText(obj)) return obj.text;
  return '';
}

export function trueText(obj: AnyWord | null): string {
  if (!obj) return '';
  if (isProxyText(obj)) {
    return trueText(obj.source);
  }
  return isWordWithText(obj) ? obj.text : '';
}

// =============================================================================
// SEQ EXTRACTION
// =============================================================================

// Helper to get seq from any word type
// Corresponds to Lisp's (seq word) generic method
export function getSeq(obj: AnyWord | null | undefined): number | number[] | null {
  if (!obj) return null;

  // CounterText has getSeq() method that returns source.seq
  if (isCounterText(obj) && typeof (obj as any).getSeq === 'function') {
    return (obj as any).getSeq();
  }

  // CompoundText has seq as number[] property
  if (isCompoundText(obj)) {
    return obj.seq;
  }

  // ProxyText has source with seq
  if (isProxyText(obj) && obj.source) {
    return obj.source.seq;
  }

  // SimpleWord types (KanjiText, KanaText) have seq property directly
  if (isSimpleWord(obj)) {
    return obj.seq;
  }

  return null;
}

// =============================================================================
// LENGTH UTILITIES
// =============================================================================

// Line 681-684: defun length-multiplier
export function lengthMultiplier(length: number, power: number, lenLim: number): number {
  if (length <= lenLim) {
    return Math.pow(length, power);
  }
  return length * Math.pow(lenLim, power - 1);
}

// Line 686-700: defparameter *length-coeff-sequences* and length-multiplier-coeff
// Note: Lisp lists include the key as first element, so (:strong 1 8 24 40 60)
// has :strong at index 0. We need to match this indexing by prepending a dummy.
const LENGTH_COEFF_SEQUENCES = {
  strong: [0, 1, 8, 24, 40, 60],  // Prepend 0 to match Lisp 1-based indexing
  weak: [0, 1, 4, 9, 16, 25, 36],
  tail: [0, 4, 9, 16, 24],
  ltail: [0, 4, 12, 18, 24]
};

export function lengthMultiplierCoeff(length: number, classType: 'strong' | 'weak' | 'tail' | 'ltail'): number {
  const coeffs = LENGTH_COEFF_SEQUENCES[classType];

  if (length > 0 && length < coeffs.length) {
    return coeffs[length];
  }

  const lastCoeff = coeffs[coeffs.length - 1];
  return Math.floor(length * (lastCoeff / (coeffs.length - 1)));
}
