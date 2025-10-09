// ichiran/grammar/suffixMatcher - Suffix lookup and matching logic
// Extracted from dict-grammar.ts Lines 900-1020

import type {
  SuffixCacheEntry,
  SuffixValue,
  ParsedSuffix
} from './types.js';
import type {
  KanaText, KanjiText, ProxyText, CompoundText
} from '../types.js';
import { initSuffixes, getSuffixCache, getSuffixClass } from './suffixCache.js';
import {
  getSuffixList,
  getSuffixUniqueOnly
} from './suffixDSL.js';
import { getSuffixContext, withSuffixContext } from '../dict/suffixContext.js';

// NOTE: Suffix definitions are registered via registerSuffixDefinitions() in init.ts
// to break circular dependencies. No direct import needed here.

// ============================================================================
// SUFFIX VALUE PARSING
// ============================================================================

// Line 900-911: defun parse-suffix-val
function parseSuffixVal(substr: string, val: SuffixCacheEntry | undefined): ParsedSuffix[] {
  if (!val) return [];

  if (Array.isArray(val) && Array.isArray(val[0])) {
    // Array of SuffixValue tuples
    return (val as SuffixValue[]).map(v => [substr, v[0], v[1]]);
  } else {
    // Single SuffixValue
    return [[substr, (val as SuffixValue)[0], (val as SuffixValue)[1]]];
  }
}

// ============================================================================
// SUFFIX MAP GENERATION
// ============================================================================

// Line 913-936: defun get-suffix-map
export async function getSuffixMap(str: string): Promise<Map<number, ParsedSuffix[]>> {
  await initSuffixes();
  const cache = getSuffixCache();
  if (!cache) throw new Error('Suffix cache not initialized');

  const result = new Map<number, ParsedSuffix[]>();

  for (let start = 0; start < str.length; start++) {
    for (let end = start + 1; end <= str.length; end++) {
      const substr = str.substring(start, end);
      const val = cache.get(substr);
      const items = parseSuffixVal(substr, val);

      for (const item of items) {
        const existing = result.get(end) || [];
        existing.push(item);
        result.set(end, existing);
      }
    }
  }

  return result;
}

// ============================================================================
// SUFFIX EXTRACTION
// ============================================================================

// Line 938-952: defun get-suffixes
export function getSuffixes(word: string): ParsedSuffix[] {
  const cache = getSuffixCache();
  if (!cache) return [];

  const results: ParsedSuffix[] = [];

  for (let start = word.length - 1; start >= 1; start--) {
    const substr = word.substring(start);
    const val = cache.get(substr);
    results.push(...parseSuffixVal(substr, val));
  }

  return results;
}

// ============================================================================
// UNIQUE MATCHING
// ============================================================================

// Line 954-963: defun match-unique
async function matchUnique(suffixClassKey: string, matches: any[]): Promise<boolean> {
  const suffixUniqueOnly = getSuffixUniqueOnly();
  const uniq = suffixUniqueOnly.get(suffixClassKey);

  if (typeof uniq === 'function') {
    return await uniq(matches);
  }

  return uniq === true;
}

// ============================================================================
// MAIN SUFFIX MATCHING
// ============================================================================

// Line 965-1020: defun find-word-suffix
export async function findWordSuffix(
  word: string,
  options: { matches?: any[] } = {}
): Promise<Array<KanjiText | KanaText | ProxyText | CompoundText>> {
  await initSuffixes();

  const matches = options.matches || [];
  let suffixes: ParsedSuffix[];

  // Check if using suffix context (from top-level segmentation)
  const ctx = getSuffixContext();
  if (ctx?.suffixMap && ctx?.suffixNextEnd !== undefined) {
    // Use pre-computed suffix map for performance
    suffixes = ctx.suffixMap.get(ctx.suffixNextEnd) || [];
  } else {
    // Compute suffixes on-demand
    suffixes = getSuffixes(word);
  }

  const results: Array<KanjiText | KanaText | ProxyText | CompoundText> = [];
  const suffixList = getSuffixList();
  const classMap = getSuffixClass();

  for (const [suffix, keyword, kf] of suffixes) {
    const suffixFn = suffixList.get(keyword);
    const suffixClassKey = kf ? (classMap?.get(kf.seq) || keyword) : keyword;
    const offset = word.length - suffix.length;

    // Validate suffix
    if (!suffixFn || offset <= 0) {
      continue;
    }
    if (matches.length > 0 && await matchUnique(suffixClassKey, matches)) {
      continue;
    }

    // Call suffix handler with updated context
    // If we have a suffix context, update suffixNextEnd for recursive calls
    // (matches Lisp's: (let ((*suffix-next-end* (and *suffix-next-end* (- *suffix-next-end* (length suffix))))) ...))
    const rootPart = word.substring(0, offset);

    let compounds: Array<KanjiText | KanaText | ProxyText | CompoundText>;
    if (ctx?.suffixNextEnd !== undefined && ctx?.suffixMap) {
      // Update suffix position for recursive suffix matching
      const newSuffixNextEnd = ctx.suffixNextEnd - suffix.length;
      compounds = await withSuffixContext(
        { suffixMap: ctx.suffixMap, suffixNextEnd: newSuffixNextEnd },
        () => suffixFn(rootPart, suffix, kf)
      );
    } else {
      // No context - handler inherits current context (if any) or uses on-demand computation
      compounds = await suffixFn(rootPart, suffix, kf);
    }

    // Lisp uses nconc - no filtering, just concatenate all results
    results.push(...compounds);
  }

  return results;
}
