// Scoring functions extracted from dict.ts
// Handles score calculation, gap penalties, segment culling, and sticky positions

import { getConnection, defineCache } from '../conn.js';

// Import types
import type {
  Entry,
  SenseProp,
  Segment,
  SegmentList,
  CalcScoreInfo
} from '../types.js';

// Import type guards
import {
  isCompoundText,
  isProxyText,
  isCounterText,
  isSegmentList,
  isSimpleWord
} from '../types.js';

// Import utility functions
import {
  
  getWordType,
  trueText,
  lengthMultiplierCoeff
} from './utils.js';

import { startTimer } from './profiling.js';
import { wordConjData, scoreBase, getOriginalText } from './conjugation.js';

// Import cache utilities
import { calcScoreMemo, getCalcScoreCache } from './cache.js';

// Import character utilities
import {
  moraLength,
  countCharClass,
  testWord,
  getCharClass,
  KANA_CHARACTERS,
  MODIFIER_CHARACTERS,
  ITERATION_CHARACTERS,
  longVowelModifierP
} from '../characters.js';

// Import grammar utilities
import { getSuffixes } from '../grammar/suffixMatcher.js';
import type { Synergy } from '../grammar/types.js';
import { isSynergy } from '../grammar/types.js';

// Import split query function
import { getSplitImpl as getSplit } from './splitQueries.js';

// Import errata
import {
  SEMI_FINAL_PRT,
  NON_FINAL_PRT,
  COPULAE,
  SKIP_WORDS,
  FINAL_PRT,
  NO_KANJI_BREAK_PENALTY,
  WEAK_CONJ_FORMS,
  testConjProp,
  skipByConjData
} from './errata.js';

// =============================================================================
// ARCHIVED WORD DETECTION
// =============================================================================

// Line 745-757: defcache :is-arch
const isArchCache = defineCache<Set<number>>(':is-arch', async () => {
  const sql = getConnection();

  const a1 = await sql<{ seq: number }[]>`
    SELECT sense.seq FROM sense
    LEFT JOIN sense_prop sp ON sp.sense_id = sense.id
      AND sp.tag = 'misc'
      AND sp.text IN ('arch', 'obsc', 'rare')
    GROUP BY sense.seq
    HAVING EVERY(sp.id IS NOT NULL)
  `;

  const a1Seqs = a1.map(r => r.seq);

  if (a1Seqs.length === 0) {
    return new Set();
  }

  const a2 = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq FROM conjugation WHERE "from" IN ${sql(a1Seqs)}
  `;

  const allSeqs = [...a1Seqs, ...a2.map(r => r.seq)];
  return new Set(allSeqs);
});

// Line 759-760: defun is-arch
async function isArch(seq: number): Promise<boolean> {
  const cache = await isArchCache();
  return cache.has(seq);
}

// Line 762-773: defun get-non-arch-posi
async function getNonArchPosi(seqSet: number[]): Promise<string[]> {
  if (seqSet.length === 0) return [];

  const sql = getConnection();
  const result = await sql<{ text: string }[]>`
    SELECT DISTINCT sp1.text
    FROM sense_prop sp1
    LEFT JOIN sense_prop sp2
      ON sp1.sense_id = sp2.sense_id
      AND sp2.tag = 'misc'
      AND sp2.text IN ('arch', 'obsc', 'rare')
    WHERE sp1.seq IN ${sql(seqSet)}
      AND sp1.tag = 'pos'
      AND sp2.id IS NULL
  `;

  return result.map(r => r.text);
}

// =============================================================================
// SCORE CALCULATION HELPERS
// =============================================================================

// Line 702-732: defun kanji-break-penalty (now async)
async function kanjiBreakPenalty(
  kanjiBreak: number[],
  score: number,
  options: {
    info?: any;
    text?: string;
    useLength?: number;
    scoreMod?: number | number[] | ((s: number) => number);
  } = {}
): Promise<number> {
  const { info, text, useLength, scoreMod } = options;

  const end = kanjiBreak.length > 1 ? 'both' :
              kanjiBreak[0] === 0 ? 'beg' : 'end';

  let bonus = 0;
  const ratio = 2;
  const posi = info?.posi;

  if (!info) {
    return score;
  }

  const seqSet = info.seqSet || [];

  // Check no-kanji-break-penalty
  if (seqSet.some((s: number) => NO_KANJI_BREAK_PENALTY.includes(s)) ||
      (end === 'beg' && text && text.startsWith('す'))) {
    return score;
  }

  // Check vs-s/v5s with suru suffix
  if (posi && (posi.includes('vs-s') || posi.includes('v5s')) && text) {
    const suffixes = getSuffixes(text);
    const suruSuffix = suffixes.find((s: any) => s[1] === ':suru');
    if (suruSuffix) {
      const offset = moraLength(text) - moraLength(suruSuffix[0]);
      const [suffixScore] = await calcScore(suruSuffix[2], {
        useLength: useLength ? useLength - offset : undefined,
        scoreMod
      });
      return Math.min(score, suffixScore + 50);
    }
  }

  // Various bonuses
  if (end === 'beg' && posi && posi.includes('num')) {
    bonus += 5;
  }
  if (end === 'beg' && posi && (posi.includes('suf') || posi.includes('n-suf'))) {
    bonus += 10;
  }
  if (end === 'end' && posi && posi.includes('pref')) {
    bonus += 12;
  }

  if (score >= SCORE_CUTOFF) {
    return Math.max(SCORE_CUTOFF, Math.ceil(score / ratio) + bonus);
  }

  return score;
}

// Line 735-742: defgeneric apply-score-mod
function applyScoreMod(scoreMod: number | number[] | ((s: number) => number), score: number, len: number): number {
  if (typeof scoreMod === 'number') {
    return score * scoreMod * len;
  } else if (typeof scoreMod === 'function') {
    return scoreMod(score);
  } else if (Array.isArray(scoreMod)) {
    return scoreMod.reduce((sum, sm) => sum + applyScoreMod(sm, score, len), 0);
  }
  return 0;
}

// =============================================================================
// CALC SCORE INSTRUMENTATION
// =============================================================================

// INSTRUMENTATION: Track calcScore calls for DP analysis
const calcScoreCallLog: Array<{text: string; seq: number | null; final: boolean; kb: string}> = [];
let trackCalcScoreCalls = false;

export function enableCalcScoreTracking() {
  trackCalcScoreCalls = true;
  calcScoreCallLog.length = 0;
}

export function getCalcScoreCallLog() {
  return calcScoreCallLog;
}

export function getCalcScoreStats() {
  const callMap = new Map<string, number>();

  for (const call of calcScoreCallLog) {
    const key = JSON.stringify(call);
    callMap.set(key, (callMap.get(key) || 0) + 1);
  }

  const duplicates = Array.from(callMap.entries())
    .filter(([_, count]) => count > 1)
    .sort((a, b) => b[1] - a[1]);

  const totalCalls = calcScoreCallLog.length;
  const duplicateCalls = duplicates.reduce((sum, [_, count]) => sum + (count - 1), 0);

  return {
    totalCalls,
    uniqueCalls: callMap.size,
    duplicateCalls,
    cacheHitRate: (duplicateCalls / totalCalls * 100).toFixed(1) + '%',
    topDuplicates: duplicates.slice(0, 10).map(([key, count]) => ({
      count,
      params: JSON.parse(key)
    }))
  };
}

// =============================================================================
// MAIN SCORE CALCULATION
// =============================================================================

// Line 777-983: defun calc-score (FAITHFUL ASYNC PORT)
export async function calcScore(
  reading: any,
  options: {
    final?: boolean;
    useLength?: number;
    scoreMod?: number | number[] | ((s: number) => number);
    kanjiBreak?: number[];
  } = {}
): Promise<[number, CalcScoreInfo]> {
  const endTimer = startTimer('calcScore');
  const { final, useLength, scoreMod = 0, kanjiBreak } = options;

  // OPTIMIZATION: Check memoization cache first
  // Now caches all reading types with comprehensive serialization
  const memoKey = calcScoreMemo.getKey(reading, options);
  const cachedResult = calcScoreMemo.get(memoKey);
  if (cachedResult) {
    endTimer();
    return [cachedResult.score, cachedResult.info];
  }

  // INSTRUMENTATION: Log this call with all relevant params
  if (trackCalcScoreCalls) {
    calcScoreCallLog.push({
      text: reading.text || 'compound',
      seq: reading.seq || null,
      final: final || false,
      kb: JSON.stringify(kanjiBreak || []),
      useLength: useLength !== undefined ? useLength : null,
      scoreMod: typeof scoreMod === 'number' ? scoreMod : 'complex',
      ord: reading.ord || 0,
      kana: reading.kana || null,
      conjugations: reading.conjugations || null
    } as any);
  }

  let ctrMode = false;

  // Line 780-790: Handle compound-text
  if (isCompoundText(reading)) {
    const args = {
      useLength: moraLength(reading.text),
      scoreMod: reading.scoreMod
    };
    const [score, info] = await calcScore(scoreBase(reading), args);
    info.conj = await wordConjData(reading);

    let finalScore = score;
    if (kanjiBreak && kanjiBreak.length > 0) {
      finalScore = await kanjiBreakPenalty(kanjiBreak, score, {
        info,
        text: reading.text,
        ...args
      });
    }

    return [finalScore, info];
  }

  // Line 791-792: Handle counter-text
  if (isCounterText(reading)) {
    ctrMode = true;
  }

  // Line 794-806: Initialize variables
  let score = 1;
  let propScore = 0;
  // For counter-text, use getText() to get full "1倍" not just "倍"
  const text = ctrMode && isCounterText(reading) && typeof reading.getText === 'function'
    ? reading.getText()
    : reading.text;

  const kanjiP = getWordType(reading) === 'kanji';
  const katakanaP = !kanjiP && countCharClass(trueText(reading), 'katakana-uniq') > 0;
  const nKanji = countCharClass(text, 'kanji');
  const len = Math.max(1, moraLength(text));

  // For ProxyText/CounterText, use their methods if available (Lisp generic methods pattern)
  // Otherwise delegate to source for ProxyText pattern
  const seq = (typeof reading.getSeq === 'function')
    ? reading.getSeq()
    : (isProxyText(reading) && reading.source ? reading.source.seq : reading.seq);
  let ord = (typeof reading.getOrd === 'function')
    ? reading.getOrd()
    : (isProxyText(reading) && reading.source ? (reading.source.ord || 0) : (reading.ord || 0));

  // Line 803: Get entry from database (using getOrFetch pattern)
  const sql = getConnection();
  const cache = getCalcScoreCache();
  let entry: Entry | null = null;
  if (seq) {
    const endEntryQuery = startTimer('calcScore_entryQuery');
    entry = await cache.getOrFetch('entry', seq, async (sql) => {
      const rows = await sql<Entry[]>`SELECT * FROM entry WHERE seq = ${seq}`;
      return rows[0] || null;
    });
    endEntryQuery();
  }

  // Line 804-806: Calculate root-p and conj-data
  // For ProxyText, delegate conjugations to source
  const wc = (isProxyText(reading) && reading.source) ? reading.source.conjugations : reading.conjugations;
  const conjOnly = wc && wc !== ':root';
  let rootP = ctrMode || (entry && !conjOnly && entry.rootP);

  const endWordConjData = startTimer('calcScore_wordConjData');
  let conjData = await wordConjData(reading);
  endWordConjData();

  // Line 808-811: Calculate secondary-conj-p
  // IMPORTANT: Lisp MODIFIES conj-data here! If not all entries have via,
  // it deletes all entries WITH via (secondary conjugations)
  const allHaveVia = conjData.length > 0 && conjData.every(cd => cd.via !== null);
  const secondaryConjP = allHaveVia;

  // If not all have via, delete entries that have via (like Lisp's delete-if)
  if (conjData.length > 0 && !allHaveVia) {
    conjData = conjData.filter(cd => cd.via === null);
  }

  const conjOf = conjData.map(cd => cd.from);
  const conjProps = conjData.map(cd => cd.prop);
  const conjTypes = conjProps.map(p => p.conjType);

  // Line 816-819: Calculate conj-types-p
  const conjTypesP = rootP || useLength !== undefined ||
    !conjProps.every(prop => testConjProp(prop, WEAK_CONJ_FORMS));

  // Line 820-821: Build seq-set and sp-seq-set
  // Filter out undefined/null values that might come from ProxyText delegation
  const seqSet = seq ? [seq, ...conjOf].filter(s => s != null) : [];
  const spSeqSet = (seq && rootP && !useLength) ? [seq] : seqSet;

  if (seqSet.length > 0) {
    await cache.prefetchSeqs([...new Set(seqSet)]);
  }

  // Line 822-824: Query prefer-kana (using getOrFetch pattern)
  const endSensePropQuery = startTimer('calcScore_sensePropQuery');
  const preferKana: SenseProp[] = spSeqSet.length > 0
    ? await cache.getOrFetch('preferKana', spSeqSet, async (sql) => {
        return await sql<SenseProp[]>`
          SELECT id, tag, sense_id AS "senseId", text, ord, seq FROM sense_prop
          WHERE seq IN ${sql(spSeqSet)}
            AND tag = 'misc'
            AND text = 'uk'
        `;
      })
    : [];
  endSensePropQuery();

  // Line 825: Check if all seqs are arch
  const endIsArch = startTimer('calcScore_isArch');
  const isArchVal = spSeqSet.length > 0 ? (await Promise.all(spSeqSet.map(s => isArch(s)))).every(v => v) : false;
  endIsArch();

  // Line 826-827: Get non-arch posi (using getOrFetch pattern)
  const endGetNonArchPosi = startTimer('calcScore_getNonArchPosi');
  const posi: string[] = ctrMode
    ? ['ctr']
    : await cache.getOrFetch('nonArchPosi', seqSet, async (_sql) => {
        return await getNonArchPosi(seqSet);
      });
  endGetNonArchPosi();

  // Line 828-830: Initialize common
  // Use getCommon() method if available (CounterText, ProxyText have their own delegation logic)
  // Otherwise delegate to source for ProxyText pattern
  const readingCommon = (typeof reading.getCommon === 'function')
    ? reading.getCommon()
    : (isProxyText(reading) && reading.source ? reading.source.common : reading.common);
  let common: number | null = conjOnly ? null : (readingCommon ?? null);
  let commonOf = common;
  let commonP = common !== null;

  const particleP = posi.includes('prt');
  const semiFinalParticleP = seq ? SEMI_FINAL_PRT.includes(seq) : false;
  const nonFinalParticleP = seq ? NON_FINAL_PRT.includes(seq) : false;
  const pronounP = posi.includes('pn');
  const copDaP = seqSet.some(s => COPULAE.includes(s));

  const longP = len > (
    kanjiP && preferKana.length === 0 && (
      (rootP && conjData.length === 0) ||
      (useLength && conjTypes.includes(13))
    ) ? 2 :
    commonP && common !== null && common > 0 && common < 10 ? 2 :
    (conjTypes.includes(3) || conjTypes.includes(9)) && !useLength ? 4 :
    3
  );

  const noCommonBonus = particleP || !conjTypesP ||
    (!longP && posi.length === 1 && posi[0] === 'int');

  let primaryP = false;
  let useLengthBonus = 0;
  let splitInfo: number | [number, ...number[]] | null = null;

  // Line 855-858: Check skip-words
  if (seqSet.some(s => SKIP_WORDS.includes(s)) ||
      (!final && seq && FINAL_PRT.includes(seq)) ||
      (!rootP && skipByConjData(conjData))) {
    // Return empty score info for skipped words
    const emptyInfo: CalcScoreInfo = {
      posi: [],
      seqSet: [],
      conj: [],
      common: null,
      scoreInfo: [0, null, 0, null],
      kpcl: [false, false, false, false]
    };
    return [0, emptyInfo];
  }

  // Line 859-870: Handle conjugation common/ord inheritance
  if (conjData.length > 0 && !(ord === 0 && commonP)) {
    // For ProxyText, use source text (like Lisp's proxy-text get-original-text method)
    const textForLookup = (isProxyText(reading) && reading.source) ? reading.source.text : text;
    const endGetOriginalText = startTimer('calcScore_getOriginalText');
    const origTexts = await getOriginalText(conjData, textForLookup);
    endGetOriginalText();

    const conjOfData: Array<[number | null, number]> = [];
    for (const [otText, otSeq] of origTexts) {
      // Skip if otText is undefined (data integrity issue)
      if (!otText) continue;

      // Get original text object to read common and ord
      const otTable = testWord(otText, 'kana') ? 'kana_text' : 'kanji_text';
      const otRows = await sql<Array<{common: number | null, ord: number}>>`
        SELECT common, ord FROM ${sql(otTable)} WHERE seq = ${otSeq} AND text = ${otText}
      `;
      if (otRows.length > 0) {
        conjOfData.push([otRows[0].common, otRows[0].ord]);
      }
    }

    if (conjOfData.length > 0) {
      // Inherit common if not already common
      if (!commonP) {
        const conjOfCommon = conjOfData
          .map(row => row[0])
          .filter(c => c !== null && c !== undefined) as number[];

        if (conjOfCommon.length > 0) {
          common = 0;
          commonP = true;
          commonOf = conjOfCommon.sort((a, b) => {
            // Line 864-867: compare-common logic
            if (a === 0) return -1;
            if (b === 0) return 1;
            return a - b;
          })[0];
        }
      }

      // Inherit minimum ord
      const conjOfOrd = Math.min(...conjOfData.map(row => row[1]));
      if (conjOfOrd < ord) {
        ord = conjOfOrd;
      }
    }
  }

  // Line 872-888: Primary-p calculation
  if (!isArchVal) {
    primaryP = !entry ||
      // prefer-kana + conj-types-p + not kanji + (not primary-nokanji or nokanji)
      (preferKana.length > 0 && conjTypesP && !kanjiP &&
       (!entry || !entry.primaryNokanji || (isSimpleWord(reading) && reading.nokanji))) ||
      // (ord=0 or cop-da-p) + (kanji or conj-types-p) + (kanji+not-prefer-kana or common+pronoun or n-kanji=0)
      ((ord === 0 || copDaP) && (kanjiP || conjTypesP) &&
       ((kanjiP && preferKana.length === 0) || (commonP && pronounP) || (entry && entry.nKanji === 0))) ||
      // prefer-kana + kanji + ord=0 + no senses with ord=0 for prefer-kana
      (preferKana.length > 0 && kanjiP && ord === 0 && (await (async () => {
        if (preferKana.length === 0) return false;
        const senseIds = preferKana.map(pk => pk.senseId);
        const senses = await sql<Array<{id: number}>>`
          SELECT id FROM sense
          WHERE id IN ${sql(senseIds)} AND ord = 0
        `;
        return senses.length === 0;
      })()));
  }

  // Score calculations
  if (primaryP) {
    score += longP ? 10 :
             secondaryConjP && !kanjiP ? 2 :
             commonP && conjTypesP ? 5 :
             preferKana.length > 0 || !entry || (entry && entry.nKanji === 0) ? 3 :
             2;
  }

  if (particleP && (final || !semiFinalParticleP)) {
    score += 2;
    if (commonP) {
      score += 2 + len;
    }
    if (final && !nonFinalParticleP) {
      if (primaryP) score += 5;
      else if (semiFinalParticleP) score += 2;
    }
  }

  if (commonP && !noCommonBonus && common !== null) {
    let commonBonus =
      secondaryConjP && !useLength ? (kanjiP && primaryP ? 4 : 2) :
      longP || copDaP || (rootP && (kanjiP || (primaryP && len > 2))) ?
        (common === 0 ? 10 :
         !primaryP ? Math.max(15 - common, 10) :
         Math.max(20 - common, 10)) :
      kanjiP ? 8 :
      primaryP ? 4 :
      len > 2 || (common > 0 && common < 10) ? 3 :
      2;

    if (commonBonus >= 10 && conjTypes.includes(10)) {
      commonBonus -= 4;
    }
    score += commonBonus;
  }

  if (longP) {
    score = Math.max(len, score);
  }

  if (kanjiP) {
    score = Math.max(isArchVal ? 3 : 5, score);
    if (longP && (nKanji > 1 || len > 4)) {
      score += 2;
    }
  }

  if (ctrMode) {
    score = Math.max(5, score);
  }

  propScore = score;
  score = propScore * (
    lengthMultiplierCoeff(len, kanjiP || katakanaP ? 'strong' : 'weak') +
    (nKanji > 1 ? (nKanji - 1) * 5 : 0)
  );

  if (useLength) {
    useLengthBonus = propScore * lengthMultiplierCoeff(
      useLength - len,
      len > 3 && (kanjiP || katakanaP) ? 'ltail' : 'tail'
    );
    useLengthBonus += applyScoreMod(scoreMod, propScore, useLength - len);
    score += useLengthBonus;
  }

  // Line 939-974: Handle split (compound word decomposition)
  if (!ctrMode) {
    const splitResult = await getSplit(reading, conjOf);
    const split = Array.isArray(splitResult) && splitResult.length > 0 ? splitResult[0] : null;
    const scoreModSplit = Array.isArray(splitResult) && splitResult.length > 1 ? splitResult[1] : 0;
    // Extract numeric score from SplitAttrs (can be number or {score: number, ...})
    const scoreModSplitNum = typeof scoreModSplit === 'number' ? scoreModSplit : (scoreModSplit?.score ?? 0);

    if (Array.isArray(split) && split.some(s => (s as any) === ':score')) {
      // Line 943-945: Simple score modifier
      score += scoreModSplitNum;
      splitInfo = scoreModSplitNum;
    } else if (Array.isArray(split) && split.some(s => (s as any) === ':pscore')) {
      // Line 946-949: Proportional score modifier
      const newPropScore = Math.max(1, propScore + scoreModSplitNum);
      score = Math.ceil((score * newPropScore) / propScore);
      propScore = newPropScore;
    } else if (Array.isArray(split) && split.length > 0 && split.every(s => typeof s === 'object')) {
      // Line 950-974: Complex recursive split calculation
      const partScores: number[] = [];
      let slen = 0;
      let smlen = 0;

      for (let cnt = 0; cnt < split.length; cnt++) {
        const part = split[cnt];
        const last = cnt === split.length - 1;
        const ptext = part.text;
        const plen = ptext.length;
        slen += plen;
        const pmlen = moraLength(part.text);
        smlen += pmlen;

        // Check if we need to truncate the last part
        let tpart = part;
        if (last && slen > text.length) {
          const newLen = Math.max(1, plen + (text.length - slen));
          tpart = {
            ...part,
            text: ptext.slice(0, newLen),
            kana: ''
          };
        }

        // Recursive calcScore call
        const partScore = await calcScore(tpart, {
          final: final && last,
          useLength: last && useLength ? pmlen + (useLength - smlen) : undefined,
          scoreMod: last ? scoreMod : 0
        });

        partScores.push(partScore[0]);
      }

      splitInfo = [scoreModSplitNum, ...partScores];
      score = scoreModSplitNum + partScores.reduce((sum, s) => sum + s, 0);
    }
  }

  const info: CalcScoreInfo = {
    posi,
    seqSet: ctrMode ? seqSet : [seq, ...conjOf],
    conj: conjData,
    common: commonP ? commonOf : null,
    scoreInfo: [propScore, kanjiBreak ?? null, useLengthBonus, splitInfo],
    kpcl: [kanjiP || katakanaP, primaryP, commonP, longP]
  };

  if (kanjiBreak && kanjiBreak.length > 0) {
    score = await kanjiBreakPenalty(kanjiBreak, score, {
      info,
      text,
      useLength,
      scoreMod
    });
  }

  // OPTIMIZATION: Store result in memoization cache (all reading types)
  calcScoreMemo.set(memoKey, { score, info });

  endTimer();
  return [score, info];
}

// =============================================================================
// SEGMENT SCORING
// =============================================================================

// Line 985-988: defun gen-score (now async)
export async function genScore(
  segment: Segment,
  options: { final?: boolean; kanjiBreak?: number[] } = {}
): Promise<Segment> {
  const endTimer = startTimer('genScore');

  const [score, info] = await calcScore(segment.word, {
    final: options.final,
    kanjiBreak: options.kanjiBreak
  });

  segment.score = score;
  segment.info = info;

  endTimer();
  return segment;
}

// =============================================================================
// STICKY POSITIONS
// =============================================================================

// Line 990-1007: defun find-sticky-positions
export function findStickyPositions(str: string): number[] {
  // Build list of modifier character class keys
  const modifiers = [
    ...Object.keys(MODIFIER_CHARACTERS),
    ...Object.keys(ITERATION_CHARACTERS)
  ];

  // Build list of kana character class keys
  const kanaCharacterKeys = Object.keys(KANA_CHARACTERS);

  const strLen = str.length;
  const result: number[] = [];

  for (let pos = 0; pos < strLen; pos++) {
    const char = str[pos];
    const charClass = getCharClass(char);

    // Check sokuon
    if (charClass === 'sokuon' && pos < strLen - 1) {
      const nextChar = str[pos + 1];
      const nextClass = getCharClass(nextChar);

      if (kanaCharacterKeys.includes(nextClass)) {
        result.push(pos + 1);
      }
    } else if (modifiers.includes(charClass)) {
      const isEndOfString = pos === strLen - 1;
      const isLongVowel = charClass === 'longVowel';

      if (!(isEndOfString && (isLongVowel || (pos > 0 && longVowelModifierP(charClass, str[pos - 1]))))) {
        result.push(pos);
      }
    }
  }

  return result;
}

// =============================================================================
// CONSTANTS
// =============================================================================

// Line 1069: defparameter *score-cutoff*
export const SCORE_CUTOFF = 5;

// Line 1163: defparameter *gap-penalty*
export const GAP_PENALTY = -500;

// Line 1020: defparameter *identical-word-score-cutoff*
export const IDENTICAL_WORD_SCORE_CUTOFF = 1 / 2;

// Line 1349: defparameter *segment-score-cutoff*
export const SEGMENT_SCORE_CUTOFF = 2 / 3;

// =============================================================================
// GAP PENALTIES
// =============================================================================

// Line 1165-1167: defun gap-penalty
export function gapPenalty(start: number, end: number): number {
  return (end - start) * GAP_PENALTY;
}

// =============================================================================
// SEGMENT UTILITIES
// =============================================================================

// Line 1022-1025: defun compare-common
// Returns true if c1 is "better" (more common) than c2
// Lisp version: (cond ((not c2) c1) ((= c2 0) (and c1 (> c1 0))) ((and c1 (> c1 0)) (< c1 c2)))
// Note: In common values, lower numbers = more common, null = not common, 0 = most common
export function compareCommon(c1: number | null, c2: number | null): boolean {
  if (!c2 && c2 !== 0) return c1 !== null;  // If c2 is null, c1 wins if it has any value (including 0)
  if (c2 === 0) return !!(c1 && c1 > 0);    // If c2 is 0 (most common), c1 only wins if it's positive (impossible)
  if (c1 && c1 > 0) return c1 < c2;          // Both positive: lower number wins
  return false;
}

// Line 1027-1036: defun cull-segments
export function cullSegments(segments: Segment[]): Segment[] {
  if (!segments || segments.length === 0) {
    return [];
  }

  // Create a copy to avoid mutating the input
  let sorted = [...segments];

  // First stable sort by common (lower = more common, null = not common)
  // Lisp: (stable-sort segments #'compare-common :key (lambda (s) (getf (segment-info s) :common)))
  // Note: No tiebreaker - rely on stable sort to preserve original order
  sorted = sorted.sort((a, b) => {
    const commonA = a.info?.common ?? null;
    const commonB = b.info?.common ?? null;
    // Compare two common values: if A is better than B, A should come first (return -1)
    if (compareCommon(commonA, commonB)) return -1;
    if (compareCommon(commonB, commonA)) return 1;
    return 0;
  });

  // Second stable sort by score (descending)
  // Lisp: (stable-sort segments #'> :key #'segment-score)
  // Note: No tiebreaker - rely on stable sort to preserve order from previous sort
  sorted = sorted.sort((a, b) => {
    return (b.score ?? 0) - (a.score ?? 0);
  });

  const maxScore = sorted[0]?.score ?? 0;
  const cutoff = maxScore * IDENTICAL_WORD_SCORE_CUTOFF;

  return sorted.filter(seg => (seg.score ?? 0) >= cutoff);
}

// Line 1040-1046: defgeneric get-segment-score
// Line 1141-1149: defgeneric get-segment-score (with methods for segment, segment-list, and synergy)
export function getSegmentScore(seg: Segment | SegmentList | Synergy): number {
  // Method for Synergy (from dict-grammar.lisp)
  if (isSynergy(seg)) {
    return seg.score;
  }
  if (isSegmentList(seg)) {
    // SegmentList
    const firstSeg = seg.segments[0];
    return firstSeg ? firstSeg.score ?? 0 : 0;
  }
  // Segment
  return (seg as Segment).score ?? 0;
}
