// ichiran/grammar/suffixDSL - Suffix definition language infrastructure
// Extracted from dict-grammar.ts Lines 71-76, 263-395, 712-774

import type {
  SuffixFunction,
  ScoreValue
} from './types.js';
import type {
  KanaText, KanjiText, CompoundText, AnyWord, SimpleWord
} from '../types.js';
import { isCompoundText, isSimpleWord } from '../types.js';
import { getKana } from '../dict/readings.js';
import { adjoinWord } from '../dict/wordQueries.js';
import { withSuffixContext } from '../dict/suffixContext.js';

// ============================================================================
// SUFFIX REGISTRIES
// ============================================================================

// Line 72: defparameter *suffix-list*
const suffixList = new Map<string, SuffixFunction>();

// Line 75: defparameter *suffix-unique-only*
const suffixUniqueOnly = new Map<string, ((matches: any[]) => boolean | Promise<boolean>) | true>();

// ============================================================================
// DYNAMIC VARIABLES - REMOVED
// ============================================================================
// Previously used global variables to simulate Lisp's dynamic binding.
// Now replaced with AsyncLocalStorage in dict/suffixContext.ts for idiomatic TypeScript.

// ============================================================================
// REGISTRY ACCESSORS
// ============================================================================

export function getSuffixList(): Map<string, SuffixFunction> {
  return suffixList;
}

export function getSuffixUniqueOnly(): Map<string, ((matches: any[]) => boolean | Promise<boolean>) | true> {
  return suffixUniqueOnly;
}

// ============================================================================
// DSL HELPER FUNCTIONS
// ============================================================================

// Line 264-269: Remove stem characters from end of string
export function destem(str: string, stemLength: number): string {
  if (stemLength === 0) return str;
  return str.substring(0, str.length - stemLength);
}

// Line 513-516: defun apply-patch
export function applyPatch(root: string, patch: [string, string]): string {
  return root.substring(0, root.length - patch[1].length) + patch[0];
}

// adjoinWord is now imported from wordQueries.js (no circular dependency)

// ============================================================================
// DSL REGISTRATION FUNCTIONS
// ============================================================================

// Line 289-293: defmacro defsuffix
// TypeScript: Register suffix function
export function defsuffix(keyword: string, fn: SuffixFunction): void {
  suffixList.set(keyword, fn);
}

// Line 295-395: defmacro def-simple-suffix
// TypeScript: Higher-order function that creates and registers a suffix
export function defSimpleSuffix(
  _name: string,
  keyword: string,
  options: {
    stem?: number;
    score?: ScoreValue;
    connector?: string;
  },
  getPrimaryWords: (root: string, suffix: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }) => Promise<Array<AnyWord | [KanjiText | KanaText, SimpleWord | ProxyText]>>
): void {
  const stem = options.stem ?? 0;
  const score = options.score ?? 0;
  const connector = options.connector ?? '';

  const suffixFn: SuffixFunction = async (root: string, suffix: string, kf: KanaText | null) => {
    // Patch holder (mutable reference pattern from Lisp)
    let patchValue: [string, string] | null = null;
    const patch = {
      set: (p: [string, string]) => { patchValue = p; },
      get: () => patchValue
    };

    // Get primary words with appropriate context
    // When stem != 0, clear suffix context to force on-demand computation
    // (matches Lisp's: (let* ((*suffix-map-temp* ,(if (= stem 0) '*suffix-map-temp* nil))) ...))
    const primaryWords = stem === 0
      ? await getPrimaryWords(root, suffix, patch)
      : await withSuffixContext(null, () => getPrimaryWords(root, suffix, patch));

    // Line 348-364: Map over primary words to create compound texts
    const results: CompoundText[] = [];
    for (const pw of primaryWords) {
      let word: AnyWord;
      let scoreBase: SimpleWord | ProxyText | undefined = undefined;

      // Check if pw is [word, scoreBase] tuple
      if (Array.isArray(pw)) {
        scoreBase = pw[1];
        word = pw[0];
      } else {
        word = pw;
      }

      // Skip words that are not SimpleWord or CompoundText (e.g., ProxyText without seq)
      if (!(isSimpleWord(word) || isCompoundText(word))) {
        continue;
      }

      const k = await getKana(word);
      let kana: string;

      if (patchValue !== null) {
        // Apply patch: destem by patch[0].length, then append patch[1]
        const patch: [string, string] = patchValue;
        kana = destem(k, patch[0].length) + patch[1];
      } else {
        kana = destem(k, stem);
      }

      kana = kana + connector + suffix;

      // Calculate score modifier
      // In Lisp, score functions are wrapped with (constantly ...) which creates
      // a function that returns a constant value. We replicate this behavior:
      // - If score is a function, evaluate it with root/suffix to get a number,
      //   then wrap it in a constantly-like function to match Lisp behavior
      // - If score is a number, keep it as a number (don't wrap)
      // This ensures applyScoreMod uses the correct formula for each type
      let scoreMod: number | ((score: number) => number);
      if (typeof score === 'function') {
        const scoreValue = score(root, suffix);
        if (scoreValue === undefined || scoreValue === null) {
          throw new Error(`Score function returned ${scoreValue} for root="${root}", suffix="${suffix}"`);
        }
        scoreMod = (_score: number) => scoreValue; // constantly - matches Lisp
      } else {
        scoreMod = score ?? 0; // plain number - matches Lisp, default to 0 if undefined
      }

      // Skip if kf is null (no suffix kana form available)
      if (kf === null) {
        continue;
      }

      const compound = await adjoinWord(word, kf, {
        text: root + suffix,
        kana,
        scoreMod,
        scoreBase,
      });

      results.push(compound);
    }

    return results;
  };

  defsuffix(keyword, suffixFn);
}

// Line 712-774: defmacro def-abbr-suffix
// TypeScript: Higher-order function for abbreviation suffixes
export function defAbbrSuffix(
  _name: string,
  keyword: string,
  stem: number,
  getPrimaryWords: (root: string, suffix: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }) => Promise<Array<KanjiText | KanaText | CompoundText>>
): void {
  const suffixFn: SuffixFunction = async (root: string, suffix: string, _kf: KanaText | null) => {
    // Abbreviation suffixes clear suffix context to force on-demand computation
    // (matches Lisp's: (let ((*suffix-map-temp* nil)) ...))
    let patchValue: [string, string] | null = null;
    const patch = {
      set: (p: [string, string]) => { patchValue = p; },
      get: () => patchValue
    };

    // Clear suffix context for abbreviation lookup
    const primaryWords = await withSuffixContext(null, () =>
      getPrimaryWords(root, suffix, patch)
    );
    const results: Array<KanjiText | KanaText | ProxyText | CompoundText> = [];

    for (const pw of primaryWords) {
      const text = root + suffix;
      const k = await getKana(pw);

      let kana: string;
      if (patchValue !== null) {
        const patch: [string, string] = patchValue;
        kana = destem(k, patch[0].length) + patch[1];
      } else {
        kana = destem(k, stem);
      }
      kana = kana + suffix;

      // Line 565-575: Create proxy-text or modify compound-text
      if (isCompoundText(pw)) {
        // compound-text: modify in place
        pw.text = text;
        pw.kana = kana;
        results.push(pw);
      } else {
        // simple-text: create proxy-text
        const proxyText: ProxyText = {
          source: pw as KanjiText | KanaText,
          text,
          kana,
          hintedp: true
        };
        results.push(proxyText);
      }
    }

    return results;
  };

  defsuffix(keyword, suffixFn);
}

// Import ProxyText type
import type { ProxyText } from '../types.js';

// ============================================================================
// UNIQUE-ONLY REGISTRATION
// ============================================================================

export function registerSuffixUniqueOnly(keyword: string, filter: ((matches: any[]) => boolean | Promise<boolean>) | true): void {
  suffixUniqueOnly.set(keyword, filter);
}
