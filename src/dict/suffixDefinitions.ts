// ichiran/dict/suffixDefinitions - All 44 suffix definitions
// Extracted from dict-grammar.ts Lines 397-895
// Moved from grammar/ to dict/ to resolve circular dependencies
// Phase 3: Wrapped in registration function to break circular dependency

import { getConnection } from '../conn.js';
import { CONJ_ADJECTIVE_STEM, CONJ_ADVERBIAL, CONJ_NEGATIVE_STEM } from './errata.js';
import type {
  KanjiText, KanaText, CompoundText, AnyWord
} from '../types.js';
import { isSimpleWord } from '../types.js';

// Import from grammar/ (infrastructure)
import {
  defSimpleSuffix,
  defAbbrSuffix,
  registerSuffixUniqueOnly,
  applyPatch
} from '../grammar/suffixDSL.js';
import { getSuffixClass } from '../grammar/suffixCache.js';

// Import from same directory (dict/) - no circular dependency!
import {
  findWordWithConjType,
  findWordWithConjProp,
  findWordWithPos,
  findWordSeq,
  findWordConjOf,
  pairWordsByConj,
  orAsHiragana,
  findWordWithSuffix
} from './suffixHelpers.js';
import { findWordFull } from './lookup.js';
import { withSuffixContext } from './suffixContext.js';

// Track registration state
let suffixesRegistered = false;

/**
 * Register all suffix definitions.
 * Called during Ichiran initialization to break circular dependencies.
 * Safe to call multiple times - only registers once.
 */
export function registerSuffixDefinitions(): void {
  if (suffixesRegistered) return;
  suffixesRegistered = true;

  // All suffix definitions are registered below
  registerConjugationSuffixes();
  registerAbbreviationSuffixes();
}

/**
 * Check if suffix definitions have been registered
 */
export function areSuffixesRegistered(): boolean {
  return suffixesRegistered;
}

// ============================================================================
// SUFFIX-SPECIFIC HELPERS
// ============================================================================

// Line 424-430: defun te-check
async function teCheck(root: string): Promise<AnyWord[]> {
  if (root === 'で') return [];
  const lastChar = root[root.length - 1];
  if (lastChar !== 'て' && lastChar !== 'で') return [];
  return await findWordWithConjType(root, 3);
}

// Line 437-441: defun teiru-check
async function teiruCheck(root: string): Promise<AnyWord[]> {
  if (root === 'いて') return [];
  return await teCheck(root);
}

// Line 518-537: defmacro suffix-sou-base
async function suffixSouBase(root: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }): Promise<Array<KanjiText | KanaText | CompoundText>> {
  if (root.endsWith('なさ')) {
    patch.set(['い', 'さ']);
    const patchedRoot = applyPatch(root, ['い', 'さ']);
    // Clear suffix context to disable recursive suffix detection
    // (matches Lisp's (let ((*suffix-map-temp* nil)) ...))
    return await withSuffixContext(null, () =>
      findWordWithConjProp(patchedRoot, (cdata) => {
        return cdata.prop.neg !== false;
      })
    ) as Array<KanjiText | KanaText | CompoundText>;
  } else if (!['な', 'よ', 'よさ', 'に', 'き'].includes(root)) {
    return await findWordWithConjType(root, 13, CONJ_ADJECTIVE_STEM, CONJ_ADVERBIAL) as Array<KanjiText | KanaText | CompoundText>;
  }
  return [];
}

// Use low-level findWord for abbreviation lookups (no suffix recursion needed)

// ============================================================================
// CONJUGATION SUFFIXES (Lines 402-707)
// ============================================================================

function registerConjugationSuffixes(): void {
  // Line 402-406: def-simple-suffix suffix-tai
  defSimpleSuffix('suffix-tai', ':tai', { connector: '', score: 5 }, async (root) => {
    if (root === 'い') return [];
    return await findWordWithConjType(root, 13);
  });

  // Line 408-412: def-simple-suffix suffix-ren
  defSimpleSuffix('suffix-ren', ':ren', { connector: '', score: 5 }, async (root) => {
    // generic ren'youkei suffix
    return await findWordWithConjType(root, 13);
  });

  // Line 414-417: def-simple-suffix suffix-ren-
  defSimpleSuffix('suffix-ren-', ':ren-', { connector: '', score: 0 }, async (root) => {
    return await findWordWithConjType(root, 13);
  });

  // Line 419-422: def-simple-suffix suffix-neg
  defSimpleSuffix('suffix-neg', ':neg', { connector: '', score: 5 }, async (root) => {
    return await findWordWithConjType(root, 13, CONJ_NEGATIVE_STEM);
  });

  // Line 432-435: def-simple-suffix suffix-te
  defSimpleSuffix('suffix-te', ':te', { connector: '', score: 0 }, async (root) => {
    return await teCheck(root);
  });

  // Line 443-446: def-simple-suffix suffix-teiru
  defSimpleSuffix('suffix-teiru', ':teiru', { connector: '', score: 3 }, async (root) => {
    return await teiruCheck(root);
  });

  // Line 448-451: def-simple-suffix suffix-teiru+
  defSimpleSuffix('suffix-teiru+', ':teiru+', { connector: '', score: 6 }, async (root) => {
    return await teiruCheck(root);
  });

  // Line 453-456: def-simple-suffix suffix-te+space
  defSimpleSuffix('suffix-te+space', ':te+space', { connector: ' ', score: 3 }, async (root) => {
    return await teCheck(root);
  });

  // Line 458-461: def-simple-suffix suffix-kudasai
  defSimpleSuffix('suffix-kudasai', ':kudasai', { connector: ' ', score: () => 360 }, async (root) => {
    return await teCheck(root);
  });

  // Line 463-475: def-simple-suffix suffix-te-ren (teren)
  defSimpleSuffix('suffix-teren', ':teren', { connector: '', score: 4 }, async (root) => {
    if (root === 'で') return [];

    const lastChar = root[root.length - 1];
    if (lastChar === 'て' || lastChar === 'で') {
      return await findWordWithConjType(root, 3);
    } else if (root !== 'い') {
      return await findWordWithConjType(root, 13);
    }

    return [];
  });

  // Line 477-482: def-simple-suffix suffix-teii
  defSimpleSuffix('suffix-teii', ':teii', { connector: ' ', score: 1 }, async (root) => {
    const lastChar = root[root.length - 1];
    if (lastChar !== 'て' && lastChar !== 'で') return [];
    return await findWordWithConjType(root, 3);
  });

  // Line 484-494: def-simple-suffix suffix-chau
  defSimpleSuffix('suffix-chau', ':chau', { stem: 1, connector: '', score: 5 }, async (root, suffix) => {
    const firstChar = suffix[0];
    let te: string | null = null;

    if (firstChar === 'じ') te = 'で';  // HIRAGANA_LETTER_ZI
    else if (firstChar === 'ち') te = 'て';  // HIRAGANA_LETTER_TI

    if (!te) return [];
    return await findWordWithConjType(root + te, 3);
  });

  // Line 496-506: def-simple-suffix suffix-to
  defSimpleSuffix('suffix-to', ':to', { stem: 1, connector: '', score: 0 }, async (root, suffix) => {
    const firstChar = suffix[0];
    let te: string | null = null;

    if (firstChar === 'と') te = 'て';  // HIRAGANA_LETTER_TO
    else if (firstChar === 'ど') te = 'で';  // HIRAGANA_LETTER_DO

    if (!te) return [];
    return await findWordWithConjType(root + te, 3);
  });

  // Line 508-511: def-simple-suffix suffix-suru
  defSimpleSuffix('suffix-suru', ':suru', { connector: ' ', score: 5 }, async (root) => {
    return await findWordWithPos(root, 'vs');
  });

  // Line 539-550: def-simple-suffix suffix-sou
  defSimpleSuffix('suffix-sou', ':sou', {
    connector: '',
    score: (root) => {
      if (root === 'から') return 40;
      if (root === 'い') return 0;
      if (root === '出来') return 100;
      return 60;
    }
  }, async (root, _suffix, patch) => {
    return await suffixSouBase(root, patch);
  });

  // Line 552-556: def-simple-suffix suffix-sou+
  defSimpleSuffix('suffix-sou+', ':sou+', { connector: '', score: 1 }, async (root, _suffix, patch) => {
    return await suffixSouBase(root, patch);
  });

  // Line 557-560: def-simple-suffix suffix-rou
  defSimpleSuffix('suffix-rou', ':rou', { connector: '', score: 1 }, async (root) => {
    return await findWordWithConjType(root, 2);
  });

  // Line 561-564: def-simple-suffix suffix-adv
  defSimpleSuffix('suffix-adv', ':adv', { connector: '', score: 1 }, async (root) => {
    return await findWordWithConjType(root, CONJ_ADVERBIAL);
  });

  // Line 567-590: def-simple-suffix suffix-sugiru
  defSimpleSuffix('suffix-sugiru', ':sugiru', { stem: 1, connector: '', score: 5 }, async (root, _suffix, patch) => {
    let modifiedRoot: string | null = null;

    if (root === 'い') {
      return [];
    } else if (root.endsWith('なさ') || root.endsWith('無さ')) {
      patch.set(['い', 'さ']);
      modifiedRoot = applyPatch(root, ['い', 'さ']);
    } else {
      modifiedRoot = root + 'い';
    }

    if (!modifiedRoot) return [];

    const patchValue = patch.get();
    if (patchValue && modifiedRoot.length > 2) {
      return await findWordWithConjProp(modifiedRoot, (cdata) => {
        return cdata.prop.neg !== false;
      });
    } else {
      return await findWordWithPos(modifiedRoot, 'adj-i');
    }
  });

  // Line 592-597: def-simple-suffix suffix-sa
  defSimpleSuffix('suffix-sa', ':sa', { connector: '', score: 2 }, async (root) => {
    const results1 = await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
    const results2 = await findWordWithPos(root, 'adj-na');
    return [...results1, ...results2];
  });

  // Line 599-609: pushnew :sa unique-only filter
  registerSuffixUniqueOnly(':sa', async (matches: any[]) => {
    const sql = getConnection();
    const seqs = matches.map(m => m.seq).filter((s): s is number => s != null);
    if (seqs.length === 0) return false;

    const result = await sql<{ seq: number }[]>`
      SELECT seq FROM entry WHERE seq = ANY(${seqs}) AND root_p = true
    `;
    return result.length > 0;
  });

  // Line 611-614: def-simple-suffix suffix-iadj
  defSimpleSuffix('suffix-iadj', ':iadj', { connector: '', score: 1 }, async (root) => {
    return await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
  });

  // Line 616-637: def-simple-suffix suffix-garu
  defSimpleSuffix('suffix-garu', ':garu', { connector: '', score: 0 }, async (root, _suffix, patch) => {
    if (['な', 'い', 'よ'].includes(root)) return [];

    const result1 = await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
    if (result1.length > 0) return result1;

    if (root.endsWith('そ')) {
      patch.set(['う', '']);
      const patchedRoot = applyPatch(root, ['う', '']);
      // Clear suffix context to disable recursive suffix detection
      // (matches Lisp's (let ((*suffix-map-temp* nil)) ...))
      const classMap = getSuffixClass();
      return await withSuffixContext(null, () =>
        findWordWithSuffix(patchedRoot, [':sou'], classMap)
      );
    }

    return [];
  });

  // Line 639-647: def-simple-suffix suffix-ra
  defSimpleSuffix('suffix-ra', ':ra', { connector: '', score: 1 }, async (root) => {
    if (root.endsWith('ら')) return [];

    const result1 = await orAsHiragana(findWordWithPos, root, 'pn');
    if (result1.length > 0) return result1;

    return await findWordSeq(root, 1580640);
  });

  // Line 649-650: pushnew :ra unique-only
  registerSuffixUniqueOnly(':ra', true);

  // Line 652-660: def-simple-suffix suffix-rashii
  defSimpleSuffix('suffix-rashii', ':rashii', { connector: '', score: 3 }, async (root) => {
    const words1 = await findWordWithConjType(root, 2);
    const words2 = await findWordWithConjType(root + 'ら', 11);
    // Filter to only simple words for pairWordsByConj
    const simpleWords1 = words1.filter((w): w is KanjiText | KanaText => isSimpleWord(w));
    const simpleWords2 = words2.filter((w): w is KanjiText | KanaText => isSimpleWord(w));
    return (await pairWordsByConj(simpleWords1, simpleWords2)).flat().filter((w): w is KanjiText | KanaText => w !== null);
  });

  // Line 662-669: def-simple-suffix suffix-desu
  defSimpleSuffix('suffix-desu', ':desu', { connector: ' ', score: () => 200 }, async (root) => {
    if (!root.endsWith('ない') && !root.endsWith('なかった')) return [];

    return await findWordWithConjProp(root, (cdata) => {
      return cdata.prop.neg !== false;
    });
  });

  // Line 671-682: pushnew :desu unique-only filter
  registerSuffixUniqueOnly(':desu', async (matches: any[]) => {
    const sql = getConnection();
    const seqs = matches.map(m => m.seq).filter((s): s is number => s != null);
    if (seqs.length === 0) return false;

    const result = await sql<{ seq: number }[]>`
      SELECT seq FROM conjugation WHERE seq = ANY(${seqs}) AND "from" = 2755350
    `;
    // じゃない (2755350)
    return result.length < matches.length;
  });

  // Line 684-691: def-simple-suffix suffix-desho
  defSimpleSuffix('suffix-desho', ':desho', { connector: ' ', score: () => 300 }, async (root) => {
    if (!root.endsWith('ない')) return [];

    return await findWordWithConjProp(root, (cdata) => {
      return cdata.prop.neg !== false;
    });
  });

  // Line 693-696: def-simple-suffix suffix-tosuru
  defSimpleSuffix('suffix-tosuru', ':tosuru', { connector: ' ', score: 3 }, async (root) => {
    return await findWordWithConjType(root, 9);
  });

  // Line 698-701: def-simple-suffix suffix-kurai
  defSimpleSuffix('suffix-kurai', ':kurai', { connector: ' ', score: 3 }, async (root) => {
    return await findWordWithConjType(root, 2);
  });

  // Line 703-707: pushnew unique-only markers
  registerSuffixUniqueOnly(':mo', true);
  registerSuffixUniqueOnly(':nikui', true);
  registerSuffixUniqueOnly(':gai', true);
}

// ============================================================================
// ABBREVIATION SUFFIXES (Lines 776-895)
// ============================================================================

function registerAbbreviationSuffixes(): void {
  // Line 776-786: def-abbr-suffix abbr-nee
  defAbbrSuffix('abbr-nee', ':nai', 2, async (root) => {
    return await findWordWithConjProp(
      root + 'ない',
      (cdata) => {
        // 居ない (1577980) 来ない (1547720) create problems so they are blocked
        return cdata.from !== 1577980 && cdata.from !== 1547720 && cdata.prop.neg !== false;
      },
      { allowRoot: true }
    ) as Array<KanjiText | KanaText | CompoundText>;
  });

  // Line 788-805: def-abbr-suffix abbr-nx
  defAbbrSuffix('abbr-nx', ':nai-x', 2, async (root, _suffix, patch) => {
    if (root === 'せ') {
      patch.set(['しない', 'せ']);
      return await findWordConjOf('しない', 1157170) as Array<KanjiText | KanaText | CompoundText>;
    } else {
      // Lisp filter: (conj-neg (conj-data-prop cdata))
      // This checks truthiness: :null (SQL NULL) and t pass, nil/false fail
      // TypeScript equivalent: neg !== false (includes null and true, excludes false)
      const results = await findWordWithConjProp(
        root + 'ない',
        (cdata) => {
          return cdata.from !== 1157170 && cdata.prop.neg !== false;
        }
      ) as Array<KanjiText | KanaText | CompoundText>;
      return results;
    }
  });

  // Line 807-816: def-abbr-suffix abbr-n
  defAbbrSuffix('abbr-n', ':nai-n', 2, async (root) => {
    return await findWordWithConjProp(
      root + 'ない',
      (cdata) => {
        // 居ない (1577980) 来ない (1547720) create problems so they are blocked
        return cdata.from !== 1577980 && cdata.from !== 1547720 && cdata.prop.neg !== false;
      }
    ) as Array<KanjiText | KanaText | CompoundText>;
  });

  // Line 818-819: pushnew :nai-n unique-only
  registerSuffixUniqueOnly(':nai-n', true);

  // Line 821-824: def-abbr-suffix abbr-nakereba
  defAbbrSuffix('abbr-nakereba', ':nakereba', 4, async (root) => {
    return await findWordFull(root + 'なければ');
  });

  // Line 826-829: def-abbr-suffix abbr-shimasho
  defAbbrSuffix('abbr-shimasho', ':shimashou', 5, async (root) => {
    return await findWordFull(root + 'しましょう');
  });

  // Line 831-835: def-abbr-suffix abbr-dewanai
  defAbbrSuffix('abbr-dewanai', ':dewanai', 4, async (root) => {
    return await findWordFull(root + 'ではない');
  });

  // Line 837-838: pushnew :dewanai unique-only
  registerSuffixUniqueOnly(':dewanai', true);

  // Line 840-843: Commented out :eba (conflicts with noun + や)
  // defAbbrSuffix('abbr-eba', ':eba', 2, async (root) => {
  //   return await findWord(root + 'えば');
  // });

  // Line 844-876: All the ~eba abbreviations
  defAbbrSuffix('abbr-teba', ':teba', 2, async (root) => {
    return await findWordFull(root + 'てば');
  });

  defAbbrSuffix('abbr-reba', ':reba', 2, async (root) => {
    return await findWordFull(root + 'れば');
  });

  defAbbrSuffix('abbr-keba', ':keba', 2, async (root) => {
    return await findWordFull(root + 'けば');
  });

  defAbbrSuffix('abbr-geba', ':geba', 2, async (root) => {
    return await findWordFull(root + 'げば');
  });

  defAbbrSuffix('abbr-neba', ':neba', 2, async (root) => {
    return await findWordFull(root + 'ねば');
  });

  defAbbrSuffix('abbr-beba', ':beba', 2, async (root) => {
    return await findWordFull(root + 'べば');
  });

  defAbbrSuffix('abbr-meba', ':meba', 2, async (root) => {
    return await findWordFull(root + 'めば');
  });

  defAbbrSuffix('abbr-seba', ':seba', 2, async (root) => {
    return await findWordFull(root + 'せば');
  });

  // Line 877-887: pushnew unique-only markers for all ~eba forms
  registerSuffixUniqueOnly(':eba', true);
  registerSuffixUniqueOnly(':teba', true);
  registerSuffixUniqueOnly(':reba', true);
  registerSuffixUniqueOnly(':keba', true);
  registerSuffixUniqueOnly(':geba', true);
  registerSuffixUniqueOnly(':neba', true);
  registerSuffixUniqueOnly(':beba', true);
  registerSuffixUniqueOnly(':meba', true);
  registerSuffixUniqueOnly(':seba', true);

  // Line 889-892: def-abbr-suffix abbr-ii
  defAbbrSuffix('abbr-ii', ':ii', 2, async (root) => {
    return await findWordFull(root + 'いい');
  });

  // Line 894-895: pushnew :ii unique-only
  registerSuffixUniqueOnly(':ii', true);
}
