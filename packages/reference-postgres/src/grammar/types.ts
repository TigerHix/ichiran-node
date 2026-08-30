// Grammar module type definitions
// Extracted from dict-grammar.ts (Lines 21-52)

import type {
  KanaText, KanjiText, ProxyText, CompoundText,
  SegmentList
} from '../types.js';

// Line 710: defstruct synergy
export interface Synergy {
  description: string;
  connector: string;
  score: number;
  start: number;
  end: number;
}

// Suffix value: [keyword, KanaText | null]
export type SuffixValue = [string, KanaText | null];

// Suffix cache entry: can be single value or array (when :join is used)
export type SuffixCacheEntry = SuffixValue | SuffixValue[];

// Parsed suffix: [substr, keyword, KanaText | null]
export type ParsedSuffix = [string, string, KanaText | null];

// Suffix function signature
export type SuffixFunction = (root: string, suffix: string, kf: KanaText | null) => Promise<(KanjiText | KanaText | ProxyText | CompoundText)[]>;

// Score type: can be constant or function
export type ScoreValue = number | ((root: string, suffix?: string) => number);

// Synergy function signature
export type SynergyFunction = (left: SegmentList, right: SegmentList) => Promise<Array<[SegmentList, Synergy, SegmentList]> | null>;

// Penalty function signature (async, returns Synergy or null)
export type PenaltyFunction = (left: SegmentList, right: SegmentList) => Promise<Synergy | null>;

// Segfilter function signature (sync, filters combinations)
export type SegfilterFunction = (left: SegmentList | null, right: SegmentList) => Array<[SegmentList | null, SegmentList]>;

// ============================================================================
// TYPE GUARDS
// ============================================================================

/**
 * Type guard for Synergy objects
 */
export function isSynergy(obj: any): obj is Synergy {
  return obj && typeof obj === 'object' &&
    'description' in obj && 'connector' in obj && 'score' in obj &&
    'start' in obj && 'end' in obj;
}
