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
  scoreBase?: SimpleWord | ProxyText;
  scoreMod: number | number[] | ((score: number) => number);
}

// CounterText constructor options (for unresolved args - before function resolution)
export interface CounterOptionsUnresolved {
  text: string;
  kana: string;
  numberText: string;
  source?: ((txt: string, readings: Reading[]) => Reading | null) | Reading | null;
  ordinalp?: boolean;
  suffix?: string | null;
  accepts?: string[] | null;
  suffixDescriptions?: string[];
  digitOpts?: Array<[number | string, ...string[]]> | null;
  digitSet?: number[];  // For CounterHifumi and other special counters
  common?: number | null;
  allowed?: number[] | null;
  foreign?: boolean;
}

// CounterText constructor options (after source function resolution)
export interface CounterOptions extends Omit<CounterOptionsUnresolved, 'source'> {
  source?: Reading | null;
}

// CounterText interface - class implementation in dict/counters.ts
export interface CounterText {
  text: string;
  kana: string;
  numberText: string;
  numberValue: number;
  source: (KanjiText | KanaText) | null;
  ordinalp: boolean;
  suffix: string | null;
  acceptsSuffixes: string[] | null;
  suffixDescriptions: string[];
  digitOpts: Array<[number | string, ...string[]]> | null;
  common: number | null;
  allowed: number[] | null;
  foreign: boolean;

  // Methods
  verify(unique: boolean): boolean;
  getText(): string;
  getKanji(): string;
  getKanaBase(): string;
  getKana(): string;
  wordType(): 'kanji' | 'kana';
  getCommon(): number | null;
  getSeq(): number | null;
  getOrd(): number;
  wordConjugations(): null;
  wordConjData(): null;
  nokanji(): boolean;
  rootP(): boolean;
  valueString(): string;
  counterJoin(n: number, numberKana: string, counterKana: string): string;
}

// ============================================================================
// WORD UNION TYPES
// ============================================================================

// Type for reading objects used in splits and conjugations (DB types only)
export type Reading = KanjiText | KanaText;

// Simple word types - have seq: number directly
export type SimpleWord = KanjiText | KanaText;

// Words that have seq property directly (either number or number[])
export type WordWithDirectSeq = SimpleWord | CompoundText;

// Words that have text property
export type WordWithText = SimpleWord | ProxyText | CompoundText | CounterText;

// Words that have kana property
export type WordWithKana = ProxyText | CompoundText | CounterText;

// All word types - comprehensive union
export type AnyWord = SimpleWord | ProxyText | CompoundText | CounterText;

// Split parts - used in split functions that decompose words
export type SplitPart = KanjiText | KanaText | ProxyText | CompoundText;

// Reading match items - used in kanji reading matching
// Either a character string or a reading tuple with variable length:
// - Irregular: [char, reading, "irr"]
// - Regular: [char, reading, type, null]
// - With rendaku/gemination: [char, reading, type, rendakuFlag|null, geminated|null]
export type ReadingMatchItem = 
  | string 
  | [string, string, "irr"]
  | [string, string, string, null]
  | [string, string, string, string, null]
  | [string, string, string, null, string];

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

// Database query result types (for joined queries)
export interface ConjugationWithProp extends Conjugation {
  // ConjProp fields (from LEFT JOIN)
  conjId?: number;
  conjType?: number;
  pos?: string;
  neg?: boolean | null;
  fml?: boolean | null;
}

// ============================================================================
// SEGMENTATION TYPES
// ============================================================================

// Line 674: defstruct segment
export interface Segment {
  start: number;
  end: number;
  word: AnyWord;
  score?: number;
  info?: CalcScoreInfo;
  // eslint-disable-next-line @typescript-eslint/no-use-before-define
  top?: TopArray | null;
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
  payload: Segment[];
}

export class TopArray {
  private array: (TopArrayItem | null)[];
  private count: number = 0;

  constructor(limit: number = 5) {
    this.array = new Array(limit).fill(null);
  }

  registerItem(score: number, payload: Segment[]): void {
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
// SCORING TYPES
// ============================================================================

/**
 * Information returned by calcScore about a word's score calculation
 * This was previously typed as 'any' but has a well-defined structure
 */
export interface CalcScoreInfo {
  /** Part-of-speech tags for this word */
  posi: string[];
  /** Sequence IDs: [main seq, ...conjugation seqs] */
  seqSet: number[];
  /** Conjugation data */
  conj: ConjData[];
  /** Commonality score (0-99 for common words, null otherwise) */
  common: number | null;
  /**
   * Detailed scoring breakdown:
   * [propScore, kanjiBreak, useLengthBonus, splitInfo]
   */
  scoreInfo: [
    propScore: number,
    kanjiBreak: number[] | null,
    useLengthBonus: number,
    splitInfo: number | [number, ...number[]] | null  // null, single number, or [scoreModSplitNum, ...partScores]
  ];
  /**
   * Boolean flags: [kanjiOrKatakana, primary, common, long]
   * k = kanji or katakana, p = primary, c = common, l = long
   */
  kpcl: [boolean, boolean, boolean, boolean];
}

/**
 * Substring hash entry for caching word lookups
 */
export interface SubstringHashEntry {
  table: string;
  row: KanjiText | KanaText;
}

/**
 * Type for substring hash maps
 */
export type SubstringHash = Map<string, SubstringHashEntry[]>;

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

export function isSegment(obj: any): obj is Segment {
  return obj && typeof obj === 'object' && 'word' in obj && 'start' in obj && 'end' in obj;
}

export function isSegmentList(obj: any): obj is SegmentList {
  return obj && typeof obj === 'object' && 'segments' in obj && Array.isArray(obj.segments);
}

export function isCounterText(obj: any): boolean {
  return obj && typeof obj === 'object' && 'valueString' in obj;
}

// ============================================================================
// ENHANCED TYPE GUARDS FOR SEQ PROPERTY
// ============================================================================

/**
 * Type guard for words that have seq as a direct property (number)
 * This includes KanjiText and KanaText
 */
export function isSimpleWord(word: any): word is SimpleWord {
  return isKanjiText(word) || isKanaText(word);
}

/**
 * Type guard for words that have seq directly (either number or number[])
 * This includes SimpleWord and CompoundText
 */
export function hasDirectSeq(word: any): word is WordWithDirectSeq {
  return isSimpleWord(word) || isCompoundText(word);
}

/**
 * Type guard for words that have a source property with seq
 * This includes ProxyText and CounterText (via duck typing)
 */
export function hasSourceWithSeq(word: any): word is ProxyText {
  return isProxyText(word) || isCounterText(word);
}

// ============================================================================
// PROPERTY-SPECIFIC TYPE GUARDS
// ============================================================================

/**
 * Type guard for words with text property
 */
export function isWordWithText(obj: any): obj is WordWithText {
  return obj && typeof obj === 'object' && 'text' in obj && typeof obj.text === 'string';
}

/**
 * Type guard for all SimpleText-based types (all types that extend SimpleText interface)
 * Includes: KanjiText, KanaText, ProxyText (all have conjugations property)
 */
export function isSimpleText(obj: any): obj is KanjiText | KanaText | ProxyText {
  return isKanjiText(obj) || isKanaText(obj) || isProxyText(obj);
}

/**
 * Type guard for words with kana property
 */
export function isWordWithKana(obj: any): obj is WordWithKana {
  return obj && typeof obj === 'object' && 'kana' in obj;
}