// Shared type definitions for Ichiran
// Common types used across dict.ts, dict-split.ts, dict-grammar.ts, and other modules

// ============================================================================
// BASE TEXT TYPES
// ============================================================================

// Line 69-72: defclass simple-text (base class for all text objects)
export interface SimpleText {
  conjugations?: number[] | ':root' | null;
  hintedp?: boolean;
}

// Line 86-98: defclass kanji-text
export interface KanjiText extends SimpleText {
  id: number;
  seq: number;
  text: string;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugateP: boolean;
  nokanji: boolean;
  bestKana: string | null;
}

// Line 128-140: defclass kana-text
export interface KanaText extends SimpleText {
  id: number;
  seq: number;
  text: string;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugateP: boolean;
  nokanji: boolean;
  bestKanji: string | null;
}

// Line 550-553: defclass proxy-text
export interface ProxyText extends SimpleText {
  text: string;
  kana: string;
  source: KanjiText | KanaText;
}

// Line 608-615: defclass compound-text
export interface CompoundText {
  text: string;
  kana: string;
  primary: KanjiText | KanaText | ProxyText;
  words: (KanjiText | KanaText | ProxyText)[];
  seq: number[]; // Array of seq values from all component words (Lisp: defmethod seq ((obj compound-text)))
  scoreBase?: any;
  scoreMod: number | number[] | ((score: number) => number);
}

// ============================================================================
// WORD UNION TYPES
// ============================================================================

// Union type for simple word objects (has guaranteed 'seq' property)
export type Word = KanjiText | KanaText | ProxyText;

// Union type for all word objects including compounds
export type AnyWord = Word | CompoundText;

// Type for reading objects used in splits and conjugations
export type Reading = KanjiText | KanaText;

// ============================================================================
// DATABASE SCHEMA TYPES
// ============================================================================

// Line 26-35: defclass entry
export interface Entry {
  seq: number;
  content: string;
  rootP: boolean;
  nKanji: number;
  nKana: number;
  primaryNokanji: boolean;
}

// Line 166-171: defclass sense
export interface Sense {
  id: number;
  seq: number;
  ord: number;
}

// Line 178-186: defclass gloss
export interface Gloss {
  id: number;
  senseId: number;
  text: string;
  ord: number;
}

// Line 197-207: defclass sense-prop
export interface SenseProp {
  id: number;
  tag: string;
  senseId: number;
  text: string;
  ord: number;
  seq: number;
}

// Line 221-227: defclass restricted-readings
export interface RestrictedReadings {
  id: number;
  seq: number;
  reading: string;
  text: string;
}

// Line 238-246: defclass conjugation
export interface Conjugation {
  id: number;
  seq: number;
  from: number;
  via: number | null;
}

// Line 262-270: defclass conj-prop
export interface ConjProp {
  id: number;
  conjId: number;
  conjType: number;
  pos: string;
  neg: boolean | null;
  fml: boolean | null;
}

// Line 309-316: defclass conj-source-reading
export interface ConjSourceReading {
  id: number;
  conjId: number;
  text: string;
  sourceText: string;
}

// Line 327: defstruct conj-data
export interface ConjData {
  seq: number;
  from: number;
  via: number | null;
  prop: ConjProp;
  srcMap: [string, string][];
}

// ============================================================================
// SEGMENTATION TYPES
// ============================================================================

// Line 674: defstruct segment
export interface Segment {
  start: number;
  end: number;
  word: AnyWord | any; // 'any' for counter-text and special cases
  score?: number;
  info?: any;
  top?: any;
  text?: string;
}

// Line 1038: defstruct segment-list
export interface SegmentList {
  segments: Segment[];
  start: number;
  end: number;
  top?: TopArray | null;
  matches: number;
}

// Line 1136-1144: defclass top-array
export interface TopArrayItem {
  score: number;
  payload: any;
}

export class TopArray {
  private array: (TopArrayItem | null)[];
  private count: number = 0;

  constructor(limit: number = 5) {
    this.array = new Array(limit).fill(null);
  }

  registerItem(score: number, payload: any): void {
    const item: TopArrayItem = { score, payload };
    const len = this.array.length;

    for (let idx = Math.min(this.count, len); idx >= 0; idx--) {
      const prevItem = idx > 0 ? this.array[idx - 1] : null;
      const done = !prevItem || prevItem.score >= score;

      if (idx < len) {
        this.array[idx] = done ? item : prevItem;
      }

      if (done) break;
    }

    this.count++;
  }

  getArray(): TopArrayItem[] {
    const result: TopArrayItem[] = [];
    const limit = Math.min(this.count, this.array.length);

    for (let i = 0; i < limit; i++) {
      const item = this.array[i];
      if (item) result.push(item);
    }

    return result;
  }

  get itemCount(): number {
    return this.count;
  }
}

// ============================================================================
// TYPE GUARDS
// ============================================================================

export function isKanjiText(word: any): word is KanjiText {
  return word && 'seq' in word && 'text' in word && 'bestKana' in word;
}

export function isKanaText(word: any): word is KanaText {
  return word && 'seq' in word && 'text' in word && 'bestKanji' in word;
}

export function isProxyText(word: any): word is ProxyText {
  return word && 'source' in word && 'text' in word && 'kana' in word;
}

export function isCompoundText(word: any): word is CompoundText {
  return word && 'words' in word && 'primary' in word && Array.isArray(word.words);
}

export function isSimpleText(word: any): word is KanjiText | KanaText {
  return isKanjiText(word) || isKanaText(word);
}