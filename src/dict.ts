// ichiran/dict - Dictionary module based on JMDict
// Port of dict.lisp

import { AsyncLocalStorage } from 'async_hooks';
import { getConnection, withDb, defineCache } from './conn.js';
import {
  kanjiRegex, kanjiMatch, kanjiCrossMatch, testWord,
  moraLength, countCharClass, getCharClass, asHiragana,
  sequentialKanjiPositions, consecutiveCharGroups,
  KANA_CHARACTERS, MODIFIER_CHARACTERS, ITERATION_CHARACTERS,
  longVowelModifierP
} from './characters.js';
import { findWordSuffix, getSuffixMap, getSuffixes, getSuffixDescription, applySegfilters, getPenalties, getSynergies, type Synergy } from './dict-grammar.js';
import { getSplit, getHint, getSegsplit } from './dict-split.js';
import { findCounter, type CounterText } from './dict-counters.js';
import {
  SKIP_WORDS, FINAL_PRT, SEMI_FINAL_PRT, COPULAE, NON_FINAL_PRT,
  NO_KANJI_BREAK_PENALTY, FORCE_KANJI_BREAK,
  WEAK_CONJ_FORMS, SKIP_CONJ_FORMS, testConjProp, skipByConjData
} from './dict-errata.js';
import { getConjDescription } from './conj-description.js';
import type { Sql } from 'postgres';

// Import shared types from types.ts (single source of truth)
import type {
  Entry,
  SimpleText,
  KanjiText,
  KanaText,
  Sense,
  Gloss,
  SenseProp,
  RestrictedReadings,
  Conjugation,
  ConjProp,
  ConjSourceReading,
  ConjData,
  ProxyText,
  CompoundText,
  Segment,
  SegmentList,
  TopArrayItem,
  Reading,
  Word,
  AnyWord
} from './types.js';
import { TopArray } from './types.js';

// =============================================================================
// PERFORMANCE INSTRUMENTATION
// Enable with: ICHIRAN_PROFILE=1 or ICHIRAN_PROFILE=true
// =============================================================================
const ENABLE_PROFILING = process.env.ICHIRAN_PROFILE === '1' || process.env.ICHIRAN_PROFILE === 'true';

export const PERF_COUNTERS = {
  findWordFull: { calls: 0, time: 0 },
  findWord: { calls: 0, time: 0 },
  findWordSuffix: { calls: 0, time: 0 },
  findWordAsHiragana: { calls: 0, time: 0 },
  findCounter: { calls: 0, time: 0 },
  genScore: { calls: 0, time: 0 },
  calcScore: { calls: 0, time: 0 },
  calcScore_entryQuery: { calls: 0, time: 0 },
  calcScore_wordConjData: { calls: 0, time: 0 },
  calcScore_sensePropQuery: { calls: 0, time: 0 },
  calcScore_isArch: { calls: 0, time: 0 },
  calcScore_getNonArchPosi: { calls: 0, time: 0 },
  calcScore_getOriginalText: { calls: 0, time: 0 },
  wordConjData: { calls: 0, time: 0 },
  getOriginalText: { calls: 0, time: 0 },
  joinSubstringWords: { calls: 0, time: 0 },
  joinSubstringWords_getSuffixMap: { calls: 0, time: 0 },
  joinSubstringWords_findWord: { calls: 0, time: 0 },
  findSubstringWords: { calls: 0, time: 0 },
  findBestPath: { calls: 0, time: 0 },
  fillSegmentPath: { calls: 0, time: 0 },
};

// Inline profiling helper - no-op when profiling disabled
function startTimer(counter: keyof typeof PERF_COUNTERS): () => void {
  if (!ENABLE_PROFILING) return () => {};

  const start = performance.now();
  PERF_COUNTERS[counter].calls++;

  return () => {
    PERF_COUNTERS[counter].time += performance.now() - start;
  };
}

export function resetPerfCounters() {
  if (!ENABLE_PROFILING) return;

  for (const key in PERF_COUNTERS) {
    PERF_COUNTERS[key as keyof typeof PERF_COUNTERS].calls = 0;
    PERF_COUNTERS[key as keyof typeof PERF_COUNTERS].time = 0;
  }
}

export function printPerfCounters() {
  if (!ENABLE_PROFILING) {
    console.log('Performance profiling is disabled. Enable with ICHIRAN_PROFILE=1');
    return;
  }

  console.log('\n' + '='.repeat(80));
  console.log('PERFORMANCE COUNTERS');
  console.log('='.repeat(80));

  const sorted = Object.entries(PERF_COUNTERS)
    .filter(([_, stats]) => stats.calls > 0)
    .sort((a, b) => b[1].time - a[1].time);

  for (const [name, stats] of sorted) {
    const avg = stats.calls > 0 ? stats.time / stats.calls : 0;
    console.log(`${name.padEnd(25)} ${stats.calls.toString().padEnd(8)} calls  ${stats.time.toFixed(2).padStart(10)}ms total  ${avg.toFixed(3).padStart(8)}ms avg`);
  }
  console.log('='.repeat(80) + '\n');
}

export function isProfilingEnabled(): boolean {
  return ENABLE_PROFILING;
}
// =============================================================================

// =============================================================================
// CACHE INFRASTRUCTURE - Now in separate module for modularity
// =============================================================================

import {
  calcScoreMemo,
  getCalcScoreCache,
  resetCalcScoreCache,
  type CalcScoreResult
} from './dict-cache.js';

// Re-export for external use
export { resetCalcScoreCache };

// =============================================================================

// Line 1243-1258: defclass word-info
export class WordInfo {
  type: 'kanji' | 'kana' | 'gap';
  text: string;
  trueText?: string | null;
  kana: string | string[];
  seq?: number | number[] | null;
  conjugations?: number[] | ':root' | null;
  score: number;
  components?: WordInfo[];
  alternative?: boolean;
  primary?: boolean;
  start?: number;
  end?: number;
  counter?: [string, boolean] | null;
  skipped: number;

  constructor(data: Partial<WordInfo> & { type: WordInfo['type']; text: string; kana: WordInfo['kana'] }) {
    this.type = data.type;
    this.text = data.text;
    this.trueText = data.trueText;
    this.kana = data.kana;
    this.seq = data.seq;
    this.conjugations = data.conjugations;
    this.score = data.score ?? 0;
    this.components = data.components;
    this.alternative = data.alternative;
    this.primary = data.primary ?? true;
    this.start = data.start;
    this.end = data.end;
    this.counter = data.counter;
    this.skipped = data.skipped ?? 0;
  }

  // Line 1260-1278: defun word-info-json
  toJSON(): any {
    return {
      type: this.type.toUpperCase(),
      text: this.text,
      truetext: this.trueText,
      kana: this.kana,
      seq: this.seq,
      conjugations: this.conjugations === ':root' ? 'ROOT' : this.conjugations,
      score: this.score,
      components: this.components?.map(c => c.toJSON()),
      alternative: this.alternative,
      primary: this.primary,
      start: this.start,
      end: this.end,
      counter: this.counter,
      skipped: this.skipped
    };
  }
}

// Helper type for word types
export type WordType = 'kanji' | 'kana' | 'gap';

// Line 82: defparameter *disable-hints* - feature flag to prevent infinite recursion
// In Lisp, this is a dynamic variable with let-binding. In TypeScript, we use AsyncLocalStorage
// to provide async-safe context isolation (prevents race conditions between concurrent calls)
const disableHintsStorage = new AsyncLocalStorage<boolean>();

function getDisableHints(): boolean {
  return disableHintsStorage.getStore() ?? false;
}

function withDisableHints<T>(value: boolean, fn: () => Promise<T>): Promise<T> {
  return disableHintsStorage.run(value, fn);
}

// Substring hash for caching word lookups (Lisp: *substring-hash*)
type SubstringHashType = Map<string, Array<{ table: string; row: any }>>;
const substringHashStorage = new AsyncLocalStorage<SubstringHashType>();

function getSubstringHash(): SubstringHashType | undefined {
  return substringHashStorage.getStore();
}

function withSubstringHash<T>(hash: SubstringHashType, fn: () => Promise<T>): Promise<T> {
  return substringHashStorage.run(hash, fn);
}

// Line 12-24: defgeneric word-type and related functions
export function getWordType(obj: AnyWord | Segment | null | undefined): WordType {
  if (obj === null || obj === undefined) return 'gap';
  // Handle Segment objects - extract word
  if ('word' in obj && 'start' in obj) return getWordType(obj.word);
  // Line 630: compound-text word-type recurses to primary
  if ('primary' in obj && 'words' in obj) return getWordType(obj.primary);
  // CounterText has its own wordType() method
  if ('wordType' in obj && typeof (obj as any).wordType === 'function') {
    return (obj as any).wordType();
  }
  // Distinguish kanji-text vs kana-text by which best_* field they have
  // kanji-text has bestKana, kana-text has bestKanji
  if ('bestKana' in obj) return 'kanji';
  if ('bestKanji' in obj) return 'kana';
  // Fallback: test the text content
  if (typeof obj === 'object' && 'text' in obj) {
    return testWord(obj.text, 'kana') ? 'kana' : 'kanji';
  }
  return 'gap';
}

export function getText(obj: AnyWord | Segment | null | undefined): string {
  if (!obj) return '';
  // Handle Segment objects - check if it has 'start' to distinguish from words with 'word' field
  if ('start' in obj && 'word' in obj) {
    // This is a Segment - get text from the wrapped word
    const word = (obj as Segment).word;
    if (!word) return '';
    // CounterText has a getText() method that combines numberText + text
    if ('getText' in word && typeof (word as any).getText === 'function') {
      return (word as any).getText();
    }
    return word.text || '';
  }
  // CounterText has a getText() method that combines numberText + text
  if ('getText' in obj && typeof (obj as any).getText === 'function') {
    return (obj as any).getText();
  }
  if ('text' in obj) return obj.text;
  return '';
}

// Helper to get seq from any word type
// Corresponds to Lisp's (seq word) generic method
export function getSeq(obj: AnyWord | null | undefined): number | number[] | null {
  if (!obj) return null;

  // CounterText has getSeq() method that returns source.seq
  if ('getSeq' in obj && typeof (obj as any).getSeq === 'function') {
    return (obj as any).getSeq();
  }

  // CompoundText has primary word with seq
  if ('primary' in obj && obj.primary && 'seq' in obj.primary) {
    return obj.primary.seq;
  }

  // ProxyText has source with seq
  if ('source' in obj && obj.source && 'seq' in obj.source) {
    return obj.source.seq ?? null;
  }

  // SimpleText types (KanjiText, KanaText) have seq property directly
  if ('seq' in obj) {
    return obj.seq ?? null;
  }

  return null;
}

// Line 117-124: defun get-kanji-kana-old (fallback when best-kana-conj returns :null)
async function getKanjiKanaOld(obj: KanjiText): Promise<string> {
  const regex = kanjiRegex(obj.text);
  const sql = getConnection();
  const kts = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE seq = ${obj.seq} ORDER BY ord
  `;

  for (const kt of kts) {
    if (regex.test(kt.text)) {
      return kt.text;
    }
  }

  return kts[0]?.text || obj.text;
}

// Line 80-84, 111-115, 150-151: defmethod get-kana with :around method for hints
export async function getKana(obj: AnyWord | Segment | null | undefined): Promise<string> {
  if (!obj) return '';

  // Handle Segment objects
  if ('start' in obj && 'word' in obj) {
    // This is a Segment - get kana from the word (recursive)
    return getKana((obj as Segment).word);
  }

  // Line 80-84: :around method - try get-hint first for simple-text
  if ('seq' in obj && 'text' in obj && obj.seq !== undefined && !getDisableHints() && !('hintedp' in obj && obj.hintedp)) {
    // BUG FIX: WordInfo can have array seq, but getHint expects single number
    // If seq is an array, use the first element
    if (Array.isArray(obj.seq)) {
      if (obj.seq.length > 0) {
        const reading = { ...obj, seq: obj.seq[0] } as Reading;
        const hint = await withDisableHints(true, () => getHint(reading));
        if (hint) return hint;
      }
    } else {
      const hint = await withDisableHints(true, () => getHint(obj as Reading));
      if (hint) return hint;
    }
  }

  // Line 111-115: kanji-text method - call best-kana-conj
  if ('bestKana' in obj && 'seq' in obj) {
    const bk = await bestKanaConj(obj as KanjiText);
    if (bk === ':null') {
      return getKanjiKanaOld(obj as KanjiText);
    }
    return bk;
  }

  // Handle objects with getKana method (e.g. CounterText)
  // CounterText.getKana() adds suffix, so prefer it over getKanaBase
  if ('getKana' in obj && typeof (obj as any).getKana === 'function') {
    return (obj as any).getKana();
  }

  // Handle objects with getKanaBase method (e.g. NumberText)
  if ('getKanaBase' in obj && typeof (obj as any).getKanaBase === 'function') {
    return (obj as any).getKanaBase();
  }

  // Line 150-151: kana-text method - return text directly
  if ('kana' in obj) {
    if (typeof obj.kana === 'string') return obj.kana;
    if (Array.isArray(obj.kana)) return obj.kana as any; // For alternative words
  }
  if ('text' in obj) return obj.text;
  return '';
}

// Line 108-109, 153-155: defmethod get-kanji
export async function getKanji(obj: AnyWord | Segment | null | undefined): Promise<string | null> {
  if (!obj) return null;

  // Handle Segment objects
  if ('start' in obj && 'word' in obj) {
    // This is a Segment - get kanji from the word (recursive)
    return getKanji((obj as Segment).word);
  }

  // Line 108-109: kanji-text method - return text directly
  if ('bestKana' in obj && 'text' in obj && !testWord(obj.text, 'kana')) {
    return obj.text;
  }

  // Line 153-155: kana-text method - call best-kanji-conj
  if ('bestKanji' in obj && 'seq' in obj) {
    const bk = await bestKanjiConj(obj as KanaText);
    if (bk !== ':null') {
      return bk;
    }
    return null;
  }

  // Default: check if text contains kanji
  if ('text' in obj && !testWord(obj.text, 'kana')) {
    return obj.text;
  }

  return null;
}

export function trueText(obj: Word | CompoundText | null): string {
  if (!obj) return '';
  if ('source' in obj) {
    return trueText(obj.source);
  }
  return ('text' in obj) ? obj.text : '';
}

export async function trueKana(obj: Word | CompoundText): Promise<string> {
  if ('source' in obj) {
    return trueKana(obj.source);
  }
  return getKana(obj);
}

export async function trueKanji(obj: Word | CompoundText): Promise<string | null> {
  if ('source' in obj) {
    return trueKanji(obj.source);
  }
  return getKanji(obj);
}

// Line 654-667: defgeneric word-conj-data
export async function wordConjData(word: AnyWord): Promise<ConjData[]> {
  if (!word) return [];
  if ('words' in word && Array.isArray(word.words)) {
    // Compound text - get conj data from last word
    return wordConjData(word.words[word.words.length - 1]);
  }
  // ProxyText - delegate to source (Lisp generic method pattern)
  if ('source' in word && word.source) {
    return wordConjData(word.source);
  }
  if ('seq' in word) {
    // Handle both single seq (simple words) and seq array (compounds)
    const seq = Array.isArray(word.seq) ? word.seq[0] : word.seq;
    const conjugations = 'conjugations' in word ? word.conjugations : undefined;
    return getConjData(seq, conjugations ?? undefined, trueText(word));
  }
  return [];
}

// Line 669-672: defgeneric score-base (only defined for compound-text in Lisp)
export function scoreBase(word: AnyWord): Word {
  // In Lisp, score-base is only defined for compound-text
  // Calling it on non-compound words would throw an error
  if (!('primary' in word && 'words' in word)) {
    throw new Error('scoreBase called on non-compound word');
  }
  if ('scoreBase' in word && word.scoreBase) {
    return word.scoreBase;
  }
  return word.primary;
}

// Line 160: defmethod common
export async function getCommon(obj: Entry | { seq: number } | any): Promise<number | null> {
  if ('source' in obj) {
    return getCommon(obj.source);
  }

  const sql = getConnection();
  const seq = obj.seq;

  const result = await sql`
    SELECT MAX(common) as max_common FROM (
      SELECT common FROM kanji_text WHERE seq = ${seq}
      UNION
      SELECT common FROM kana_text WHERE seq = ${seq}
    ) tmp
  `;

  return result[0]?.max_common ?? null;
}

// Line 486: defparameter *max-word-length*
export const MAX_WORD_LENGTH = 50;

// Line 329-340: defcache :no-conj-data
const noConjDataCache = defineCache<Set<number>>(':no-conj-data', async () => {
  const sql = getConnection();
  const rows = await sql<{ seq: number }[]>`
    SELECT entry.seq FROM entry
    LEFT JOIN conjugation c ON entry.seq = c.seq
    WHERE c.seq IS NULL
  `;
  return new Set(rows.map(r => r.seq));
});

// Line 339-340: defun no-conj-data
async function noConjData(seq: number): Promise<boolean> {
  const cache = await noConjDataCache();
  return cache.has(seq);
}

// Line 342-369: defun get-conj-data
export async function getConjData(
  seq: number,
  fromConjIds?: number[] | number | ':root',
  texts?: string | string[]
): Promise<ConjData[]> {
  if (fromConjIds === ':root' || await noConjData(seq)) {
    return [];
  }

  const sql = getConnection();
  const cache = getCalcScoreCache();
  const textList = texts ? (Array.isArray(texts) ? texts : [texts]) : null;

  const getConjugations = async () => {
    if (fromConjIds === null || fromConjIds === undefined) {
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY id`;
    } else if (Array.isArray(fromConjIds)) {
      const validIds = fromConjIds.filter((id): id is number => id !== null && id !== undefined);
      if (validIds.length === 0) {
        return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY id`;
      }
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} AND id IN ${sql(validIds)} ORDER BY id`;
    } else {
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} AND "from" = ${fromConjIds} ORDER BY id`;
    }
  };

  const shouldCacheConj = fromConjIds === null || fromConjIds === undefined;
  const conjs = shouldCacheConj
    ? await cache.getOrFetch('conj', seq, getConjugations) as Conjugation[]
    : await getConjugations();
  const results: ConjData[] = [];

  if (conjs.length === 0) {
    return results;
  }

  // Batch fetch all source readings and props for all conjugations
  const conjIds = conjs.map(c => c.id);

  // Fetch all source readings in one query
  const fetchSources = async (ids: number[], filter?: string[]) => {
    if (filter && filter.length > 0) {
      return sql<(ConjSourceReading & { conjId: number })[]>`
        SELECT text, source_text, conj_id FROM conj_source_reading
        WHERE conj_id IN ${sql(ids)} AND text IN ${sql(filter)}
      `;
    }
    return sql<(ConjSourceReading & { conjId: number })[]>`
      SELECT text, source_text, conj_id FROM conj_source_reading
      WHERE conj_id IN ${sql(ids)}
    `;
  };

  let allSrcMapRows: (ConjSourceReading & { conjId: number })[];
  if (textList && textList.length > 0) {
    const cached = cache.getConjSourcesFiltered(conjIds, textList);
    if (cached && cached.length > 0) {
      allSrcMapRows = cached as (ConjSourceReading & { conjId: number })[];
    } else {
      const fetchedSources = await cache.getOrFetch('conjSource', { conjIds, texts: textList }, () => fetchSources(conjIds, textList));
      allSrcMapRows = fetchedSources as (ConjSourceReading & { conjId: number })[];
      cache.setConjSources(conjIds, allSrcMapRows, textList);
    }
  } else {
    const cached = conjIds.flatMap(id => cache.getConjSources(id) || []);
    if (cached.length > 0) {
      allSrcMapRows = cached as (ConjSourceReading & { conjId: number })[];
    } else {
      const fetchedSources = await cache.getOrFetch('conjSource', { conjIds }, () => fetchSources(conjIds));
      allSrcMapRows = fetchedSources as (ConjSourceReading & { conjId: number })[];
      cache.setConjSources(conjIds, allSrcMapRows);
    }
  }

  // Fetch all props in one query
  const fetchProps = async (ids: number[]) => {
    return sql<(ConjProp & { conjId: number })[]>`
      SELECT * FROM conj_prop WHERE conj_id IN ${sql(ids)}
    `;
  };

  let allProps = conjIds.flatMap(id => cache.getConjProps(id) || []);
  if (allProps.length === 0) {
    allProps = await cache.getOrFetch('conjProps', conjIds, async () => fetchProps(conjIds)) as (ConjProp & { conjId: number })[];
  }

  // Group source readings by conj_id
  const srcMapByConjId = new Map<number, [string, string][]>();
  for (const row of allSrcMapRows) {
    if (!srcMapByConjId.has(row.conjId)) {
      srcMapByConjId.set(row.conjId, []);
    }
    srcMapByConjId.get(row.conjId)!.push([row.text, row.sourceText]);
  }

  // Group props by conj_id
  const propsByConjId = new Map<number, ConjProp[]>();
  for (const prop of allProps) {
    if (!propsByConjId.has(prop.conjId)) {
      propsByConjId.set(prop.conjId, []);
    }
    propsByConjId.get(prop.conjId)!.push(prop);
  }

  // Build results
  for (const conj of conjs) {
    const srcMap = srcMapByConjId.get(conj.id) || [];

    if (!textList || srcMap.length > 0) {
      const props = propsByConjId.get(conj.id) || [];

      for (const prop of props) {
        results.push({
          seq: conj.seq,
          from: conj.from,
          via: conj.via,
          prop,
          srcMap
        });
      }
    }
  }

  return results;
}

// Line 371-378: defun get-original-text-once
export async function getOriginalTextOnce(conjDatas: ConjData | ConjData[], texts: string | string[]): Promise<string[]> {
  const textList = Array.isArray(texts) ? texts : [texts];
  const conjDataList = Array.isArray(conjDatas) ? conjDatas : [conjDatas];

  const results: string[] = [];
  for (const conjData of conjDataList) {
    for (const [txt, srcTxt] of conjData.srcMap) {
      if (textList.includes(txt)) {
        results.push(srcTxt);
      }
    }
  }
  return results;
}

// Line 380-392: defun get-original-text*
export async function getOriginalText(conjDatas: ConjData | ConjData[], texts: string | string[]): Promise<[string, number][]> {
  const textList = Array.isArray(texts) ? texts : [texts];
  const conjDataList = Array.isArray(conjDatas) ? conjDatas : [conjDatas];

  const results: [string, number][] = [];
  for (const conjData of conjDataList) {
    const srcTexts: string[] = [];
    for (const [txt, srcTxt] of conjData.srcMap) {
      if (textList.includes(txt)) {
        srcTexts.push(srcTxt);
      }
    }

    if (conjData.via === null) {
      for (const txt of srcTexts) {
        results.push([txt, conjData.from]);
      }
    } else {
      const newCd = await getConjData(conjData.via, conjData.from);
      const nestedResults = await getOriginalText(newCd, srcTexts);
      results.push(...nestedResults);
    }
  }
  return results;
}

// Line 404-415: defprepared query-parents-kanji
async function queryParentsKanji(seq: number, text: string): Promise<Array<[number, number]>> {
  const sql = getConnection();
  const results = await sql<Array<{ id: number; conjId: number }>>`
    SELECT kt.id, conj.id as conj_id
    FROM kanji_text kt, conj_source_reading csr, conjugation conj
    WHERE conj.seq = ${seq}
      AND conj.id = csr.conj_id
      AND csr.text = ${text}
      AND kt.seq = CASE WHEN conj.via IS NOT NULL THEN conj.via ELSE conj.from END
      AND kt.text = csr.source_text
  `;
  return results.map(r => [r.id, r.conjId]);
}

// Line 417-428: defprepared query-parents-kana
async function queryParentsKana(seq: number, text: string): Promise<Array<[number, number]>> {
  const sql = getConnection();
  const results = await sql<Array<{ id: number; conjId: number }>>`
    SELECT kt.id, conj.id as conj_id
    FROM kana_text kt, conj_source_reading csr, conjugation conj
    WHERE conj.seq = ${seq}
      AND conj.id = csr.conj_id
      AND csr.text = ${text}
      AND kt.seq = CASE WHEN conj.via IS NOT NULL THEN conj.via ELSE conj.from END
      AND kt.text = csr.source_text
  `;
  return results.map(r => [r.id, r.conjId]);
}

// Line 430-455: defun best-kana-conj
export async function bestKanaConj(obj: KanjiText): Promise<string | ':null'> {
  const wc = obj.conjugations;

  // If not conjugated or is root, return best-kana directly
  if ((!wc || wc === ':root') && obj.bestKana && obj.bestKana !== ':null') {
    return obj.bestKana;
  }

  const parents = await queryParentsKanji(obj.seq, obj.text);
  if (parents.length === 0) return ':null';

  const sql = getConnection();

  // Batch fetch all parent kanji_text rows
  const parentIds = parents.map(([pid]) => pid);
  const parentKtRows = await sql<KanjiText[]>`
    SELECT * FROM kanji_text WHERE id = ANY(${parentIds})
  `;
  const parentKtMap = new Map(parentKtRows.map(kt => [kt.id, kt]));

  // Batch fetch all conj_source_reading records for all conjugation IDs
  const conjIds = parents.map(([, cid]) => cid);
  const allReadings = await sql<Array<{ conjId: number; text: string; sourceText: string }>>`
    SELECT conj_id as conj_id, text, source_text as source_text
    FROM conj_source_reading
    WHERE conj_id = ANY(${conjIds})
  `;

  // Build a map: conjId -> sourceText -> reading texts
  const readingsMap = new Map<number, Map<string, string[]>>();
  for (const r of allReadings) {
    if (!readingsMap.has(r.conjId)) {
      readingsMap.set(r.conjId, new Map());
    }
    const sourceMap = readingsMap.get(r.conjId)!;
    if (!sourceMap.has(r.sourceText)) {
      sourceMap.set(r.sourceText, []);
    }
    sourceMap.get(r.sourceText)!.push(r.text);
  }

  for (const [pid, cid] of parents) {
    // Get parent kanji-text from cache
    const parentKt = parentKtMap.get(pid);
    if (!parentKt) continue;

    // Recursively get best-kana from parent
    const parentBk = await bestKanaConj(parentKt);

    // Skip if parent has no best-kana or if conjugation filter doesn't match
    if (parentBk === ':null') continue;
    if (wc && wc !== ':root' && Array.isArray(wc) && !wc.includes(cid)) continue;

    // Get readings for this conjugation from parent's best-kana (from cache)
    const readings = readingsMap.get(cid)?.get(parentBk) || [];

    if (readings.length === 0) continue;

    if (readings.length === 1) {
      return readings[0];
    }

    // Multiple readings - try to match using kanji-cross-match
    const km = kanjiCrossMatch(parentKt.text, parentBk, obj.text);
    if (km) {
      const exactMatch = readings.find(r => r === km);
      if (exactMatch) {
        return exactMatch;
      }

      // Try regex matching, preferring similar length
      const regex = kanjiRegex(obj.text);
      const lenKm = km.length;
      const sortedReadings = [...readings].sort((a, b) => Math.abs(a.length - lenKm) - Math.abs(b.length - lenKm));

      for (const rd of sortedReadings) {
        if (regex.test(rd)) {
          return rd;
        }
      }
    }

    // Fallback to first reading
    return readings[0];
  }

  return ':null';
}

// Line 457-476: defun best-kanji-conj
export async function bestKanjiConj(obj: KanaText): Promise<string | ':null'> {
  const wc = obj.conjugations;

  // If not conjugated or is root, return best-kanji directly
  if ((!wc || wc === ':root') && obj.bestKanji && obj.bestKanji !== ':null') {
    return obj.bestKanji;
  }

  // If entry has no kanji, return :null
  const sql = getConnection();
  const entryRows = await sql<Entry[]>`SELECT * FROM entry WHERE seq = ${obj.seq}`;
  if (obj.nokanji || (entryRows.length > 0 && entryRows[0].nKanji === 0)) {
    return ':null';
  }

  const parents = await queryParentsKana(obj.seq, obj.text);
  if (parents.length === 0) return ':null';

  // Batch fetch all parent kana_text rows
  const parentIds = parents.map(([pid]) => pid);
  const parentKanaRows = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE id = ANY(${parentIds})
  `;
  const parentKanaMap = new Map(parentKanaRows.map(kt => [kt.id, kt]));

  // Batch fetch all conj_source_reading records for all conjugation IDs
  const conjIds = parents.map(([, cid]) => cid);
  const allReadings = await sql<Array<{ conjId: number; text: string; sourceText: string }>>`
    SELECT conj_id as conj_id, text, source_text as source_text
    FROM conj_source_reading
    WHERE conj_id = ANY(${conjIds})
  `;

  // Build a map: conjId -> sourceText -> reading texts
  const readingsMap = new Map<number, Map<string, string[]>>();
  for (const r of allReadings) {
    if (!readingsMap.has(r.conjId)) {
      readingsMap.set(r.conjId, new Map());
    }
    const sourceMap = readingsMap.get(r.conjId)!;
    if (!sourceMap.has(r.sourceText)) {
      sourceMap.set(r.sourceText, []);
    }
    sourceMap.get(r.sourceText)!.push(r.text);
  }

  for (const [pid, cid] of parents) {
    // Get parent kana-text from cache
    const parentKanaText = parentKanaMap.get(pid);
    if (!parentKanaText) continue;

    // Recursively get best-kanji from parent
    const parentBk = await bestKanjiConj(parentKanaText);

    // Skip if parent has no best-kanji or if conjugation filter doesn't match
    if (parentBk === ':null') continue;
    if (wc && wc !== ':root' && Array.isArray(wc) && !wc.includes(cid)) continue;

    // Get readings for this conjugation from parent's best-kanji (from cache)
    const readings = readingsMap.get(cid)?.get(parentBk) || [];

    // Find matching reading using kanji-match
    for (const reading of readings) {
      if (kanjiMatch(reading, obj.text)) {
        return reading;
      }
    }
  }

  return ':null';
}

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

// Line 501-518: defun find-substring-words
export async function findSubstringWords(
  str: string,
  sticky: number[] = []
): Promise<Map<string, Array<{ table: string; row: any }>>> {
  const endTimer = startTimer('findSubstringWords');
  const sql = getConnection();
  const substringHash = new Map<string, Array<{ table: string; row: any }>>();
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
export async function adjoinWord(
  word1: any,
  word2: any,
  options: {
    text?: string;
    kana?: string;
    scoreMod?: number | ((score: number) => number);
    scoreBase?: any;
  } = {}
): Promise<CompoundText> {
  const text = options.text || (getText(word1) + getText(word2));
  const kana = options.kana || (await getKana(word1) + await getKana(word2));
  const scoreMod = options.scoreMod ?? 0;

  // Helper to extract seq from a word (handles compounds recursively)
  const getSeqs = (word: any): number[] => {
    if ('seq' in word) {
      // If word.seq is already an array (compound), flatten it
      if (Array.isArray(word.seq)) {
        return word.seq;
      }
      // Simple word with single seq
      return [word.seq];
    }
    return [];
  };

  // Check if word1 is already a compound
  if ('words' in word1 && Array.isArray(word1.words)) {
    // Extend existing compound
    word1.text = text;
    word1.kana = kana;
    word1.words.push(word2);
    word1.scoreMod = Array.isArray(word1.scoreMod)
      ? [scoreMod, ...word1.scoreMod]
      : [scoreMod, word1.scoreMod];
    // Update seq array to include word2's seq
    word1.seq = [...word1.seq, ...getSeqs(word2)];
    return word1;
  }

  // Create new compound
  // Lisp: (defmethod seq ((obj compound-text)) (mapcar #'seq (words obj)))
  return {
    text,
    kana,
    primary: word1,
    words: [word1, word2],
    seq: [...getSeqs(word1), ...getSeqs(word2)],
    scoreMod,
    scoreBase: options.scoreBase
  };
}

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

// dict-split.lisp Line 77-81: defun get-split
// Now imported from dict-split.js - fully implemented with async support

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

// Line 777-983: defun calc-score (FAITHFUL ASYNC PORT)
export async function calcScore(
  reading: any,
  options: {
    final?: boolean;
    useLength?: number;
    scoreMod?: number | number[] | ((s: number) => number);
    kanjiBreak?: number[];
  } = {}
): Promise<[number, any]> {
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
  if ('words' in reading && 'primary' in reading) {
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
  if ('valueString' in reading) {
    ctrMode = true;
  }

  // Line 794-806: Initialize variables
  let score = 1;
  let propScore = 0;
  // For counter-text, use getText() to get full "1倍" not just "倍"
  const text = ctrMode && 'getText' in reading && typeof reading.getText === 'function'
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
    : (('source' in reading && reading.source) ? reading.source.seq : reading.seq);
  let ord = (typeof reading.getOrd === 'function')
    ? reading.getOrd()
    : (('source' in reading && reading.source) ? (reading.source.ord || 0) : (reading.ord || 0));

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
  const wc = ('source' in reading && reading.source) ? reading.source.conjugations : reading.conjugations;
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
    : await cache.getOrFetch('nonArchPosi', seqSet, async (sql) => {
        return await getNonArchPosi(seqSet);
      });
  endGetNonArchPosi();

  // Line 828-830: Initialize common
  // Use getCommon() method if available (CounterText, ProxyText have their own delegation logic)
  // Otherwise delegate to source for ProxyText pattern
  const readingCommon = (typeof reading.getCommon === 'function')
    ? reading.getCommon()
    : (('source' in reading && reading.source) ? reading.source.common : reading.common);
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
  let splitInfo: any = null;

  // Line 855-858: Check skip-words
  if (seqSet.some(s => SKIP_WORDS.includes(s)) ||
      (!final && seq && FINAL_PRT.includes(seq)) ||
      (!rootP && skipByConjData(conjData))) {
    return [0, {}];
  }

  // Line 859-870: Handle conjugation common/ord inheritance
  if (conjData.length > 0 && !(ord === 0 && commonP)) {
    // For ProxyText, use source text (like Lisp's proxy-text get-original-text method)
    const textForLookup = ('source' in reading && reading.source) ? reading.source.text : text;
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
       (!entry || !entry.primaryNokanji || ('nokanji' in reading && reading.nokanji))) ||
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

    if (Array.isArray(split) && split.some(s => s === ':score')) {
      // Line 943-945: Simple score modifier
      score += scoreModSplitNum;
      splitInfo = scoreModSplitNum;
    } else if (Array.isArray(split) && split.some(s => s === ':pscore')) {
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

  const info = {
    posi,
    seqSet: ctrMode ? seqSet : [seq, ...conjOf],
    conj: conjData,
    common: commonP ? commonOf : null,
    scoreInfo: [propScore, kanjiBreak, useLengthBonus, splitInfo],
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

// Constants
// Line 1069: defparameter *score-cutoff*
export const SCORE_CUTOFF = 5;

// Line 1163: defparameter *gap-penalty*
export const GAP_PENALTY = -500;

// Line 1020: defparameter *identical-word-score-cutoff*
export const IDENTICAL_WORD_SCORE_CUTOFF = 1 / 2;

// Line 1349: defparameter *segment-score-cutoff*
export const SEGMENT_SCORE_CUTOFF = 2 / 3;

// Line 1165-1167: defun gap-penalty
export function gapPenalty(start: number, end: number): number {
  return (end - start) * GAP_PENALTY;
}

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
    const commonA = a.info?.common;
    const commonB = b.info?.common;
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
  if ('description' in seg && 'connector' in seg && 'score' in seg) {
    return (seg as Synergy).score;
  }
  if ('segments' in seg && Array.isArray(seg.segments)) {
    // SegmentList
    const firstSeg = seg.segments[0];
    return firstSeg ? firstSeg.score ?? 0 : 0;
  }
  // Segment
  return (seg as Segment).score ?? 0;
}

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
    suffixMapTemp?: Map<number, import('./dict-grammar.js').ParsedSuffix[]>;
    suffixNextEnd?: number;
  } = {}
): Promise<any[]> {
  const endTimer = startTimer('findWordFull');

  const simpleWords = await findWord(word);
  const results: any[] = [...simpleWords];

  // Add suffix matches (returns CompoundText[])
  // Line 1089-1092: Pass suffix-map-temp and suffix-next-end (dynamic variables in Lisp)
  const suffixMatches = await findWordSuffix(word, {
    matches: simpleWords,
    suffixMapTemp: options.suffixMapTemp,
    suffixNextEnd: options.suffixNextEnd
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
  return all.filter((w): w is KanjiText | KanaText =>
    'seq' in w && 'text' in w && !('words' in w) && !('source' in w) && !('valueString' in w)
  );
}

// Line 1071-1109: defun join-substring-words*
async function joinSubstringWords(str: string): Promise<[Array<[number, number, Segment[]]>, number[]]> {
  const endTimer = startTimer('joinSubstringWords');

  const sticky = findStickyPositions(str);
  const substringHash = await findSubstringWords(str, sticky);

  // Set substring hash context for find-word to use
  return await withSubstringHash(substringHash, async () => {
    const katakanaGroups = consecutiveCharGroups('katakana', str);
    const numberGroups = consecutiveCharGroups('number', str);
    const endGetSuffixMap = startTimer('joinSubstringWords_getSuffixMap');
    const suffixMap = await getSuffixMap(str);
    endGetSuffixMap();

    let kanjiBreak: number[] = [];
    const ends: number[] = [];
    const result: Array<[number, number, Segment[]]> = [];

  for (let start = 0; start < str.length; start++) {
    if (sticky.includes(start)) continue;

    const katakanaGroupEnd = katakanaGroups.find(g => g[0] === start)?.[1];
    const numberGroupEnd = numberGroups.find(g => g[0] === start)?.[1];

    for (let end = start + 1; end <= Math.min(str.length, start + MAX_WORD_LENGTH); end++) {
      if (sticky.includes(end)) continue;

      const part = str.slice(start, end);

      // Set up temporary variables for findWordFull
      // Line 1089-1092: Bind *suffix-map-temp* and *suffix-next-end* (Lisp dynamic variables)
      const endFindWord = startTimer('joinSubstringWords_findWord');
      const words = await findWordFull(part, {
        asHiragana: katakanaGroupEnd === end,
        counter: numberGroupEnd && numberGroupEnd <= end ?
          Math.min(numberGroupEnd - start, 20) : undefined,
        suffixMapTemp: suffixMap,
        suffixNextEnd: end
      });
      endFindWord();

      if (words.length > 0) {
        const segments = words.map(word => ({
          start,
          end,
          word
        }));

        if (start === 0 || ends.includes(start)) {
          if (FORCE_KANJI_BREAK.includes(part)) {
            for (let i = start + 1; i < end; i++) {
              kanjiBreak.push(i);
            }
          } else {
            const positions = sequentialKanjiPositions(part, start);
            kanjiBreak.push(...positions);
          }
        }

        if (!ends.includes(end)) {
          ends.push(end);
        }

        result.push([start, end, segments]);
      }
    }
  }

    endTimer();
    return [result, [...new Set(kanjiBreak)]];
  }); // Close withSubstringHash
}

function collectSeqsForPrefetch(
  word: AnyWord | null | undefined,
  target: Set<number>,
  seen: WeakSet<object> = new WeakSet()
): void {
  if (!word || typeof word !== 'object') return;
  const obj = word as object;
  if (seen.has(obj)) return;
  seen.add(obj);

  const addSeqValue = (value: unknown) => {
    if (value === null || value === undefined) return;
    if (Array.isArray(value)) {
      for (const item of value) {
        if (typeof item === 'number' && Number.isFinite(item)) {
          target.add(item);
        }
      }
    } else if (typeof value === 'number' && Number.isFinite(value)) {
      target.add(value);
    }
  };

  if (typeof (word as any).getSeq === 'function') {
    addSeqValue((word as any).getSeq());
  }

  if ('seq' in word) {
    addSeqValue((word as any).seq);
  }

  if ('primary' in word && word.primary) {
    collectSeqsForPrefetch(word.primary as AnyWord, target, seen);
  }

  if ('words' in word && Array.isArray((word as any).words)) {
    for (const child of (word as any).words) {
      collectSeqsForPrefetch(child as AnyWord, target, seen);
    }
  }

  if ('source' in word && (word as any).source) {
    collectSeqsForPrefetch((word as any).source as AnyWord, target, seen);
  }
}

// Line 1111-1127: defun join-substring-words
export async function joinSubstringWords2(str: string): Promise<SegmentList[]> {
  const [result, kanjiBreak] = await joinSubstringWords(str);
  const seqsForPrefetch = new Set<number>();
  for (const [, , segments] of result) {
    for (const segment of segments) {
      collectSeqsForPrefetch(segment.word as AnyWord, seqsForPrefetch);
    }
  }

  if (seqsForPrefetch.size > 0) {
    const cache = getCalcScoreCache();
    await cache.prefetchSeqs([...seqsForPrefetch]);
  }

  const endsWithLw = str.endsWith('ー');
  const segmentLists: SegmentList[] = [];

  for (const [start, end, segments] of result) {
    const kb = [start, end].filter(n => kanjiBreak.includes(n)).map(n => n - start);

    const scoredSegments: Segment[] = [];
    for (const segment of segments) {
      await genScore(segment, {
        final: segment.end === str.length || (endsWithLw && segment.end === str.length - 1),
        kanjiBreak: kb
      });


      if ((segment.score ?? 0) >= SCORE_CUTOFF) {
        scoredSegments.push(segment);
      }
    }

    if (scoredSegments.length > 0) {
      segmentLists.push({
        segments: cullSegments(scoredSegments),
        start,
        end,
        matches: segments.length
      });
    }
  }

  return segmentLists;
}

// Line 1169-1171: defun get-seg-initial
function getSegInitial(seg: Segment | SegmentList): any[] {
  const splits = applySegfilters(null, seg as SegmentList);
  return splits.map((split: any) => split[1]);
}

// Line 1173-1176: defun get-seg-splits
async function getSegSplits(segLeft: Segment | SegmentList, segRight: Segment | SegmentList): Promise<Array<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]>> {
  const splits = applySegfilters(segLeft as SegmentList | null, segRight as SegmentList);
  const results: Array<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]> = [];

  for (const split of splits) {
    const [left, right] = split;
    if (left === null) continue; // Skip splits with null left segment
    const penalties = await getPenalties(left, right);
    const synergies = await getSynergies(left, right);

    results.push(penalties, ...synergies);
  }

  return results;
}

// Line 1178-1186: defun expand-segment-list
async function expandSegmentList(segmentList: SegmentList): Promise<void> {
  const expanded: Segment[] = [...segmentList.segments];

  for (const segment of segmentList.segments) {
    const segsplit = await getSegsplit(segment);
    if (segsplit) {
      expanded.push(segsplit);
      segmentList.matches++;
    }
  }

  // Sort by score descending
  expanded.sort((a, b) => (b.score ?? 0) - (a.score ?? 0));
  segmentList.segments = expanded;
}

// Line 1188-1231: defun find-best-path
export async function findBestPath(
  segmentLists: SegmentList[],
  strLength: number,
  options: { limit?: number } = {}
): Promise<Array<[any[], number]>> {
  const endTimer = startTimer('findBestPath');
  const limit = options.limit ?? 5;
  const top = new TopArray(limit);

  top.registerItem(gapPenalty(0, strLength), []);

  // Expand all segment lists and initialize their tops
  for (const segmentList of segmentLists) {
    await expandSegmentList(segmentList);
    segmentList.top = new TopArray(limit);
  }

  // Process segment lists
  for (let i = 0; i < segmentLists.length; i++) {
    const seg1 = segmentLists[i];
    const gapLeft = gapPenalty(0, seg1.start);
    const gapRight = gapPenalty(seg1.end, strLength);

    // Initial segments
    const initialSegs = getSegInitial(seg1);
    for (const seg of initialSegs) {
      const score1 = getSegmentScore(seg);
      seg1.top!.registerItem(gapLeft + score1, [seg]);
      top.registerItem(gapLeft + score1 + gapRight, [seg]);
    }

    // Connect to following segments
    for (let j = i + 1; j < segmentLists.length; j++) {
      const seg2 = segmentLists[j];

      if (seg2.start >= seg1.end) {
        const score2 = getSegmentScore(seg2);
        const gapLeft2 = gapPenalty(seg1.end, seg2.start);
        const gapRight2 = gapPenalty(seg2.end, strLength);

        for (const tai of seg1.top!.getArray()) {
          const payload = tai.payload as any[];
          const segLeft = payload[0];
          const score3 = getSegmentScore(segLeft);
          const scoreTail = tai.score - score3;

          const splits = await getSegSplits(segLeft, seg2);
          for (const split of splits) {
            const splitArray = Array.isArray(split) ? split : [split];
            // Line 1241: (reduce #'+ split :key #'get-segment-score)
            const splitScore = splitArray.reduce((sum, s) => sum + getSegmentScore(s), 0);
            const accum = gapLeft2 + Math.max(splitScore, score3 + 1, score2 + 1) + scoreTail;
            const path = [...splitArray, ...payload.slice(1)];

            seg2.top!.registerItem(accum, path);
            top.registerItem(accum + gapRight2, path);
          }
        }
      }
    }
  }

  // Clear segment-list tops
  for (const segment of segmentLists) {
    segment.top = null;
  }

  // Return results
  endTimer();
  return top.getArray().map(tai => [tai.payload.reverse(), tai.score]);
}

// Line 1380-1386: defun word-info-from-text
export async function wordInfoFromText(text: string): Promise<WordInfo> {
  return await withDb(async (sql) => {
    const readings = await findWordFull(text, { counter: ':auto' });
    const segments: Segment[] = await Promise.all(readings.map(r =>
      genScore({
        start: 0,
        end: text.length,
        word: r,
        text
      })
    ));

    const segmentList: SegmentList = {
      segments,
      start: 0,
      end: text.length,
      matches: segments.length
    };

    return await wordInfoFromSegmentList(segmentList);
  });
}

// Line 1325-1347: defun word-info-from-segment
export async function wordInfoFromSegment(segment: Segment): Promise<WordInfo> {
  const word = segment.word;
  const wordType = getWordType(word);

  const data: Partial<WordInfo> = {
    type: wordType,
    text: getText(segment),
    kana: await getKana(word),
    seq: getSeq(word),
    conjugations: 'conjugations' in word ? word.conjugations : undefined,
    trueText: trueText(word),
    score: segment.score ?? 0,
    start: segment.start,
    end: segment.end
  };

  // Handle compound words
  if ('words' in word && Array.isArray(word.words)) {
    const primarySeq = getSeq(word.primary);
    data.components = await Promise.all(word.words.map(async (wrd: any) => new WordInfo({
      type: getWordType(wrd),
      text: getText(wrd),
      kana: await getKana(wrd),
      seq: getSeq(wrd),
      conjugations: wrd.conjugations,
      trueText: trueText(wrd),
      primary: getSeq(wrd) === primarySeq
    })));
  }

  // Handle counter words
  if ('valueString' in word && 'ordinalp' in word) {
    data.counter = [word.valueString(), word.ordinalp];
  }

  return new WordInfo(data as any);
}

// Line 1351-1378: defun word-info-from-segment-list
export async function wordInfoFromSegmentList(segmentList: SegmentList): Promise<WordInfo> {
  const segments = segmentList.segments;
  const wiList = await Promise.all(segments.map(wordInfoFromSegment));

  if (wiList.length === 0) {
    return new WordInfo({
      type: 'gap',
      text: '',
      kana: ''
    });
  }

  const wi1 = wiList[0];
  const maxScore = wi1.score;

  // Filter by score cutoff
  const filteredWiList = wiList.filter(wi =>
    wi.score >= maxScore * SEGMENT_SCORE_CUTOFF
  );

  const matches = segmentList.matches;

  if (filteredWiList.length === 1) {
    wi1.skipped = matches - 1;
    return wi1;
  }

  // Multiple alternatives
  const kanaList = filteredWiList.map(wi => wi.kana);
  const seqList = filteredWiList.map(wi => wi.seq).filter((s): s is number | number[] => s !== null && s !== undefined);

  return new WordInfo({
    type: wi1.type,
    text: wi1.text,
    kana: [...new Set(kanaList.flat())],
    seq: seqList.length > 0 ? seqList.flat() : null,
    components: filteredWiList,
    alternative: true,
    score: wi1.score,
    start: segmentList.start,
    end: segmentList.end,
    skipped: matches - filteredWiList.length
  });
}

// Line 1388-1405: defun fill-segment-path
export async function fillSegmentPath(str: string, path: (SegmentList | any)[]): Promise<WordInfo[]> {
  const endTimer = startTimer('fillSegmentPath');

  const result: WordInfo[] = [];
  let idx = 0;

  for (const segmentList of path) {
    if ('segments' in segmentList && 'start' in segmentList) {
      // It's a SegmentList
      const sl = segmentList as SegmentList;

      if (sl.start > idx) {
        // Add gap
        const substr = str.slice(idx, sl.start);
        result.push(new WordInfo({
          type: 'gap',
          text: substr,
          kana: substr,
          start: idx,
          end: sl.start
        }));
      }

      result.push(await wordInfoFromSegmentList(sl));
      idx = sl.end;
    }
  }

  // Final gap if needed
  if (idx < str.length) {
    const substr = str.slice(idx);
    result.push(new WordInfo({
      type: 'gap',
      text: substr,
      kana: substr,
      start: idx,
      end: str.length
    }));
  }

  endTimer();
  // Line 1405: Lisp uses (nreverse result) before processing
  // In Lisp, push adds to front, so result is built in reverse order, then nreverse fixes it.
  // In TypeScript, push adds to back, so result is already in correct order - no reversal needed.
  return processWordInfo(result);
}

// Line 1407-1413: defun word-info-rec-find
function wordInfoRecFind(wiList: WordInfo[], testFn: (wi: WordInfo) => boolean): Array<[WordInfo, WordInfo | undefined]> {
  const results: Array<[WordInfo, WordInfo | undefined]> = [];

  for (let i = 0; i < wiList.length; i++) {
    const wi = wiList[i];
    const wiNext = i < wiList.length - 1 ? wiList[i + 1] : undefined;

    if (testFn(wi)) {
      results.push([wi, wiNext]);
    }

    if (wi.components) {
      const nested = wordInfoRecFind(wi.components, testFn);
      for (const [wf, wfNext] of nested) {
        results.push([wf, wfNext || wiNext]);
      }
    }
  }

  return results;
}

// Line 1415-1440: defun process-word-info
export function processWordInfo(wiList: WordInfo[]): WordInfo[] {
  // Handle 何 (nani/nan) reading selection
  for (let i = 0; i < wiList.length; i++) {
    const wi = wiList[i];
    const wiNext = i < wiList.length - 1 ? wiList[i + 1] : null;

    if (wiNext && wi.text === '何') {
      const kn = Array.isArray(wiNext.kana) ? wiNext.kana : [wiNext.kana];
      let nani = false;
      let nan = false;

      for (const kana of kn) {
        if (kana.length > 0) {
          const firstChar = kana[0];
          const fcClass = getCharClass(firstChar);

          const nanClasses = [
            'ba', 'bi', 'bu', 'be', 'bo',
            'pa', 'pi', 'pu', 'pe', 'po',
            'da', 'dji', 'dzu', 'de', 'do',
            'za', 'ji', 'zu', 'ze', 'zo',
            'ta', 'chi', 'tsu', 'te', 'to',
            'na', 'nu', 'ne', 'no',
            'ra', 'ri', 'ru', 're', 'ro'
          ];

          if (nanClasses.includes(fcClass)) {
            nan = true;
          } else {
            nani = true;
          }
        }
      }

      const naniKana = nan && nani ? 'なに' :
                      nan ? 'なん' :
                      nani ? 'なに' : null;

      if (naniKana) {
        wi.kana = naniKana;
      }
    }
  }

  return wiList;
}

// Line 1442-1446: defun word-info-reading
export async function wordInfoReading(wordInfo: WordInfo | any): Promise<KanjiText | KanaText | null> {
  // Handle both WordInfo objects and plain JSON objects
  const type = wordInfo instanceof WordInfo
    ? wordInfo.type
    : (typeof wordInfo.type === 'string' ? wordInfo.type.toLowerCase() : wordInfo.type);

  const table = type === 'kanji' ? 'kanji_text' : 'kana_text';

  // Handle both trueText (WordInfo) and truetext (JSON)
  const trueTextVal = wordInfo instanceof WordInfo
    ? wordInfo.trueText
    : (wordInfo.trueText || wordInfo.truetext);

  if (!table || !trueTextVal) {
    return null;
  }

  const sql = getConnection();
  const results = await sql<(KanjiText | KanaText)[]>`
    SELECT * FROM ${sql(table)} WHERE text = ${trueTextVal} LIMIT 1
  `;

  return results[0] || null;
}

// Line 1448-1451: defun dict-segment
export async function dictSegment(
  str: string,
  options: { limit?: number } = {}
): Promise<Array<[WordInfo[], number]>> {
  const limit = options.limit ?? 5;
  const segmentLists = await joinSubstringWords2(str);
  const paths = await findBestPath(segmentLists, str.length, { limit });

  return Promise.all(paths.map(async ([path, score]) => [await fillSegmentPath(str, path), score]));
}

// Line 1453-1454: defun simple-segment
export async function simpleSegment(str: string, options: { limit?: number } = {}): Promise<WordInfo[]> {
  const results = await dictSegment(str, options);
  return results[0]?.[0] || [];
}

// =============================================================================
// JSON Output Functions (Lines 1456-1934 in dict.lisp)
// =============================================================================

// Types for JSON output
export interface SenseJson {
  pos: string;
  gloss: string;
  field?: string;
  info?: string;
}

export interface ConjPropJson {
  pos: string;
  type: string;
  fml?: boolean;
  neg?: boolean;
}

export interface ConjInfoJson {
  prop: ConjPropJson[];
  reading?: string;
  gloss?: SenseJson[];
  readok?: boolean;
  via?: ConjInfoJson[];
}

export interface WordInfoGlossJson {
  reading: string;
  text: string;
  kana: string | string[];
  score?: number;
  compound?: string[];
  components?: WordInfoGlossJson[];
  counter?: { value: string; ordinal: boolean };
  seq?: number;
  gloss?: SenseJson[];
  suffix?: string;
  conj?: ConjInfoJson[];
  alternative?: WordInfoGlossJson[];
}

interface SenseRaw {
  ord: number;
  gloss: string;
  props: Array<[string, string[]]>;
}

// Line 1456-1482: defun get-senses-raw
export async function getSensesRaw(seq: number): Promise<SenseRaw[]> {
  const db = getConnection();

  // Get glosses aggregated by sense
  const glosses = await db<{ord: number; gloss: string}[]>`
    SELECT sense.ord, string_agg(gloss.text, '; ' ORDER BY gloss.ord) AS gloss
    FROM sense
    LEFT JOIN gloss ON gloss.sense_id = sense.id
    WHERE sense.seq = ${seq}
    GROUP BY sense.id
    ORDER BY sense.ord
  `;

  // Get sense properties (pos, s_inf, stagk, stagr, field)
  const tags = ['pos', 's_inf', 'stagk', 'stagr', 'field'];
  const props = await db<{ord: number; tag: string; text: string}[]>`
    SELECT sense.ord, sense_prop.tag, sense_prop.text
    FROM sense, sense_prop
    WHERE sense.seq = ${seq}
      AND sense_prop.sense_id = sense.id
      AND sense_prop.tag IN ${db(tags)}
    ORDER BY sense.ord, sense_prop.tag, sense_prop.ord
  `;

  // Build sense list with empty props
  const senseList: SenseRaw[] = glosses.map(({ord, gloss}) => ({
    ord,
    gloss: gloss || '',
    props: []
  }));

  // Group props by sense and tag
  let cursord: number | null = null;
  let curtag: string | null = null;
  let bag: string[] = [];
  let curprop: SenseRaw | null = null;

  for (const {ord, tag, text} of props) {
    if (ord !== cursord || tag !== curtag) {
      // Flush previous bag
      // Note: In Lisp, push adds to front of list, so bag is built in reverse order.
      // In TypeScript, push adds to end of array, so bag is already in correct order.
      // Lisp reverses the bag to get correct order, but we don't need to.
      if (curprop && curtag) {
        curprop.props.push([curtag, bag]);
      }
      cursord = ord;
      curtag = tag;
      bag = [];
      curprop = senseList.find(s => s.ord === ord) || null;
    }
    bag.push(text);
  }

  // Flush final bag
  // Note: In Lisp, the final bag is NOT reversed (unlike intermediate flushes).
  // Since Lisp's push builds the bag in reverse order, the final bag ends up reversed.
  // In TypeScript, push builds in forward order, so we need to explicitly reverse to match Lisp.
  if (curprop && curtag) {
    curprop.props.push([curtag, [...bag].reverse()]);
  }

  return senseList;
}

// Line 1484-1490: defun get-senses
export async function getSenses(seq: number): Promise<Array<[string, string, Array<[string, string[]]>]>> {
  const senses = await getSensesRaw(seq);
  return senses.map(({gloss, props}) => {
    const posArray = props.find(([tag]) => tag === 'pos')?.[1] || [];
    const posStr = `[${posArray.join(',')}]`;
    return [posStr, gloss, props];
  });
}

// Line 1492-1502: defun get-senses-str
export async function getSensesStr(seq: number): Promise<string> {
  const senses = await getSenses(seq);
  const lines: string[] = [];
  let rpos = '';

  for (let i = 0; i < senses.length; i++) {
    const [pos, gloss, props] = senses[i];
    const emptypos = pos === '[]';

    if (!emptypos) rpos = pos;

    const inf = props.find(([tag]) => tag === 's_inf')?.[1];
    const rinf = inf ? inf.join('; ') : null;

    const field = props.find(([tag]) => tag === 'field')?.[1];
    const rfield = field ? field.join(',') : null;

    const parts: string[] = [`${i + 1}. ${rpos}`];
    if (rfield) parts.push(`{${rfield}}`);
    if (rinf) parts.push(`《${rinf}》`);
    parts.push(gloss);

    lines.push(parts.join(' '));
  }

  return lines.join('\n');
}

// Line 1559-1574: defun short-sense-str
async function shortSenseStr(seq: number, withPos?: string): Promise<string | null> {
  const db = getConnection();

  if (withPos) {
    const result = await db<{ gloss_text: string }[]>`
      SELECT (
        SELECT string_agg(gloss.text, '; ' ORDER BY gloss.ord)
        FROM gloss
        WHERE gloss.sense_id = sense.id
      ) as gloss_text
      FROM sense
      INNER JOIN sense_prop pos ON pos.sense_id = sense.id
        AND pos.tag = 'pos'
        AND pos.text = ${withPos}
      WHERE sense.seq = ${seq}
      GROUP BY sense.id
      ORDER BY sense.ord
      LIMIT 1
    `;
    return result[0]?.gloss_text || null;
  } else {
    const result = await db<{ gloss_text: string }[]>`
      SELECT (
        SELECT string_agg(gloss.text, '; ' ORDER BY gloss.ord)
        FROM gloss
        WHERE gloss.sense_id = sense.id
      ) as gloss_text
      FROM sense
      WHERE sense.seq = ${seq}
      GROUP BY sense.id
      ORDER BY sense.ord
      LIMIT 1
    `;
    return result[0]?.gloss_text || null;
  }
}

// Line 1592-1596: defun entry-info-short
async function entryInfoShort(seq: number, withPos?: string): Promise<string> {
  const senseStr = await shortSenseStr(seq, withPos);
  const readingPart = await readingStrSeq(seq);
  return `${readingPart} : ${senseStr || ''}`;
}

// Line 1601-1607: defun select-conjs
async function selectConjs(seq: number, conjIds?: number[] | 'root'): Promise<Conjugation[]> {
  const db = getConnection();

  if (conjIds) {
    if (conjIds === 'root') {
      return [];
    }
    // Filter out undefined values from the array
    const validConjIds = conjIds.filter((id): id is number => id !== undefined);

    // If no valid IDs remain, fall through to default behavior
    if (validConjIds.length === 0) {
      // Don't query with empty array, fall through to default
    } else {
      return await db<Conjugation[]>`
        SELECT * FROM conjugation WHERE seq = ${seq} AND id = ANY(${validConjIds}) ORDER BY id
      `;
    }
  }

  // Fetch all conjugations in a single query, with via IS NULL rows first
  const allConjs = await db<Conjugation[]>`
    SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY (via IS NULL) DESC, id
  `;

  // If any have via IS NULL, return only those; otherwise return all
  if (allConjs.length > 0 && allConjs[0].via === null) {
    // Find the last index where via IS NULL
    let lastNullIdx = 0;
    for (let i = 1; i < allConjs.length; i++) {
      if (allConjs[i].via === null) {
        lastNullIdx = i;
      } else {
        break;
      }
    }
    return allConjs.slice(0, lastNullIdx + 1);
  }

  return allConjs;
}

// Line 1609-1614: defun conj-type-order
function conjTypeOrder(conjType: number): number {
  // Swaps Continuative (10) and Imperative (13) so former is shown first
  if (conjType === 10) return 13;
  if (conjType === 13) return 10;
  return conjType;
}

// Line 1616-1622: defun is-rareru
function isRareru(text: string | string[]): boolean {
  const endings = ['られる', 'られます', 'られない', 'られません'];

  if (Array.isArray(text)) {
    return text.some(t => endings.some(end => t.endsWith(end)));
  }

  return endings.some(end => text.endsWith(end));
}

// Line 1624-1634: defun filter-props
function filterProps(props: ConjProp[], text?: string | string[] | null): ConjProp[] {
  // A hack to remove Passive conjugation on れる forms
  return props.filter(prop => {
    if (!text) return true;

    const isPassive = prop.conjType === 6; // passive
    const isIchidan = ['v1', 'v1s', 'vk'].includes(prop.pos);

    if (isPassive && isIchidan && !isRareru(text)) {
      return false;
    }

    return true;
  });
}

// Line 1636-1644: defun select-conjs-and-props
async function selectConjsAndProps(
  seq: number,
  conjIds?: number[] | 'root',
  text?: string | string[] | null
): Promise<Array<[Conjugation, ConjProp[], [number, number]]>> {
  const db = getConnection();
  const conjs = await selectConjs(seq, conjIds);

  if (conjs.length === 0) {
    return [];
  }

  // Batch load all props for all conjugations in one query
  const conjIdList = conjs.map(c => c.id);
  const allProps = await db<ConjProp[]>`
    SELECT * FROM conj_prop WHERE conj_id IN ${db(conjIdList)}
  `;

  // Group props by conj_id
  const propsByConjId = new Map<number, ConjProp[]>();
  for (const prop of allProps) {
    const list = propsByConjId.get(prop.conjId);
    if (list) {
      list.push(prop);
    } else {
      propsByConjId.set(prop.conjId, [prop]);
    }
  }

  const results: Array<[Conjugation, ConjProp[], [number, number]]> = [];

  for (const conj of conjs) {
    const props = propsByConjId.get(conj.id) || [];

    const minVal = Math.min(...props.map((p: ConjProp) => conjTypeOrder(p.conjType)));
    const viaVal = conj.via === null ? 0 : 1;
    const fprops = filterProps(props, text);

    results.push([conj, fprops, [viaVal, minVal]]);
  }

  // Sort by third element (tuple comparison)
  results.sort((a, b) => {
    const [viaA, minA] = a[2];
    const [viaB, minB] = b[2];
    if (viaA !== viaB) return viaA - viaB;
    return minA - minB;
  });

  return results;
}

// Line 277-283: defun conj-info-short
function conjInfoShort(obj: ConjProp): string {
  const pos = obj.pos;
  const type = getConjDescription(obj.conjType);

  // Lisp: ~@[~[ Affirmative~; Negative~]~] - only print if non-nil, choose based on value
  let neg = '';
  if (obj.neg !== null && obj.neg !== undefined) {
    neg = obj.neg ? ' Negative' : ' Affirmative';
  }

  // Lisp: ~@[~[ Plain~; Formal~]~] - only print if non-nil, choose based on value
  let fml = '';
  if (obj.fml !== null && obj.fml !== undefined) {
    fml = obj.fml ? ' Formal' : ' Plain';
  }

  return `[${pos}] ${type}${neg}${fml}`;
}

// Line 1646-1660: defun print-conj-info
async function printConjInfo(
  seq: number,
  conjugations?: number[] | 'root',
  out: string[] = []
): Promise<string> {
  const viaUsed = new Set<number>();
  const results = await selectConjsAndProps(seq, conjugations);

  for (const [conj, props] of results) {
    const via = conj.via;

    if (via !== null && viaUsed.has(via)) {
      continue;
    }

    let first = true;
    for (const conjProp of props) {
      out.push(`\n${first ? '[' : ' '} Conjugation: ${conjInfoShort(conjProp)}`);
      first = false;
    }

    if (via === null) {
      out.push(`\n  ${await entryInfoShort(conj.from)}`);
    } else {
      out.push('\n --(via)--');
      await printConjInfo(via, undefined, out);
      viaUsed.add(via);
    }

    out.push(' ]');
  }

  return out.join('');
}

// Line 1743-1778: defun word-info-str
export async function wordInfoStr(wordInfo: WordInfo): Promise<string> {
  const lines: string[] = [];

  async function inner(wi: WordInfo, suffix?: boolean, marker?: boolean): Promise<void> {
    if (marker) {
      lines.push(' * ');
    }

    lines.push(wordInfoReadingStr(wi));

    if (wi.components && wi.components.length > 0) {
      // Compound word
      const componentTexts = wi.components.map(c => c.text).join(' + ');
      lines.push(` Compound word: ${componentTexts}`);

      for (const comp of wi.components) {
        lines.push('\n');
        await inner(comp, !comp.primary, true);
      }
    } else if (wi.counter) {
      // Counter
      const [value] = wi.counter;
      lines.push('\n');
      lines.push(value);

      if (wi.seq && typeof wi.seq === 'number') {
        lines.push('\n');
        lines.push(await getSensesStr(wi.seq));
      }
    } else {
      // Regular word
      const seq = wi.seq;
      const conjs = wi.conjugations;
      let desc: string | null = null;

      if (suffix && typeof seq === 'number') {
        desc = getSuffixDescription(seq);
      }

      if (desc) {
        lines.push(`  [suffix]: ${desc} `);
      } else if (!conjs || conjs === ':root') {
        lines.push('\n');
        lines.push(typeof seq === 'number' ? await getSensesStr(seq) : '???');
      }

      if (typeof seq === 'number' && conjs !== ':root') {
        const conjInfo = await printConjInfo(seq, Array.isArray(conjs) ? conjs : undefined);
        lines.push(conjInfo);
      }
    }
  }

  if (wordInfo.alternative && wordInfo.components) {
    // Alternative readings
    for (let i = 0; i < wordInfo.components.length; i++) {
      if (i > 0) lines.push('\n');
      lines.push(`<${i + 1}>. `);
      await inner(wordInfo.components[i]);
    }
  } else {
    await inner(wordInfo);
  }

  return lines.join('');
}

// Helper to get word type for readings
function wordType(reading: Reading): 'kanji' | 'kana' {
  // Check the actual text content, not the nokanji flag
  // nokanji=false means the word HAS a kanji form available, but reading.text may still be kana
  if ('text' in reading) {
    return testWord(reading.text, 'kana') ? 'kana' : 'kanji';
  }
  return 'kana';
}

// Line 1505-1511: defun match-kana-kanji
function matchKanaKanji(
  kanaReading: KanaText,
  kanjiReading: KanjiText,
  restricted: Array<[string, string]>
): boolean {
  if (!kanaReading.text) return false;
  // Line 1506: (cond ((nokanji kana-reading) nil)
  if (kanaReading.nokanji) return false;

  const kanaText = kanaReading.text;
  const restr = restricted
    .filter(([rt]) => rt === kanaText)
    .map(([, kt]) => kt);

  return restr.length > 0
    ? restr.includes(kanjiReading.text)
    : true;
}

// Line 1513-1529: defun match-sense-restrictions
async function matchSenseRestrictions(
  seq: number,
  props: Array<[string, string[]]>,
  reading: Reading
): Promise<boolean> {
  const db = getConnection();

  const stagk = props.find(([tag]) => tag === 'stagk')?.[1];
  const stagr = props.find(([tag]) => tag === 'stagr')?.[1];

  if (!stagk && !stagr) return true;

  const readingText = 'text' in reading ? reading.text : '';

  if (stagk?.includes(readingText) || stagr?.includes(readingText)) {
    return true;
  }

  const wtype = wordType(reading);

  // For kana readings with stagr restriction, normalize to hiragana for comparison
  if (wtype === 'kana' && stagr) {
    const normalizedReading = asHiragana(readingText);
    if (stagr.includes(normalizedReading)) {
      return true;
    }
  }

  if (!stagr && wtype === 'kanji') return false;
  if (!stagk && wtype === 'kana') return false;

  // Query restricted readings
  const restricted = await db<{reading: string; text: string}[]>`
    SELECT reading, text FROM restricted_readings WHERE seq = ${seq}
  `;
  const restrictedPairs = restricted.map(r => [r.reading, r.text] as [string, string]);

  if (wtype === 'kanji') {
    // For kanji readings, query kana_text entries that match the stagr restriction
    // and check if any of them are compatible with this kanji reading
    const rkana = await db<KanaText[]>`
      SELECT * FROM kana_text WHERE seq = ${seq} AND text IN ${db(stagr || [])}
    `;
    return rkana.some(rk => matchKanaKanji(rk, reading as KanjiText, restrictedPairs));
  } else if (wtype === 'kana') {
    const rkanji = await db<KanjiText[]>`
      SELECT * FROM kanji_text WHERE seq = ${seq} AND text IN ${db(stagk || [])}
    `;
    return rkanji.some(rk => matchKanaKanji(reading as KanaText, rk, restrictedPairs));
  }

  return false;
}

// Line 1532-1533: defun split-pos
function splitPos(posStr: string): string[] {
  if (posStr.length < 2) return [];
  return posStr.slice(1, -1).split(',');
}

// Line 1535-1557: defun get-senses-json
export async function getSensesJson(
  seq: number,
  options: {
    posList?: string[];
    reading?: Reading;
    readingGetter?: () => Reading | null | Promise<Reading | null>;
  } = {}
): Promise<SenseJson[]> {
  const { posList, reading: initialReading, readingGetter } = options;
  const senses = await getSenses(seq);
  const result: SenseJson[] = [];
  let rpos = '';
  let lpos: string[] = [];
  let reading: Reading | null = initialReading || null;
  let readp = false;

  for (const [pos, gloss, props] of senses) {
    const emptypos = pos === '[]';

    if (!emptypos) {
      rpos = pos;
      lpos = splitPos(pos);
    }

    // Filter by POS if specified
    if (posList && !lpos.some(p => posList.includes(p))) {
      continue;
    }

    // Check sense restrictions
    if (initialReading || readingGetter) {
      const hasRestrictions = props.some(([tag]) => tag === 'stagk' || tag === 'stagr');

      if (hasRestrictions) {
        if (!reading && readingGetter && !readp) {
          readp = true;
          reading = await readingGetter();
        }

        if (reading && !(await matchSenseRestrictions(seq, props, reading))) {
          continue;
        }
      }
    }

    const inf = props.find(([tag]) => tag === 's_inf')?.[1];
    const rinf = inf ? inf.join('; ') : null;

    const field = props.find(([tag]) => tag === 'field')?.[1];
    const rfield = field ? `{${field.join(',')}}` : null;

    const js: SenseJson = { pos: rpos, gloss };
    if (rfield) js.field = rfield;
    if (rinf) js.info = rinf;

    result.push(js);
  }

  return result;
}

// Line 1576-1590: defun reading-str*
function readingStrBase(kanji: string | null, kana: string | string[]): string {
  const kanaStr = Array.isArray(kana) ? kana[0] : kana;
  return kanji ? `${kanji} 【${kanaStr}】` : kanaStr;
}

// Line 1581-1584: defun reading-str-seq
async function readingStrSeq(seq: number): Promise<string> {
  const db = getConnection();

  const kanjiResult = await db<{text: string}[]>`
    SELECT text FROM kanji_text WHERE seq = ${seq} AND ord = 0 LIMIT 1
  `;
  const kanjiText = kanjiResult[0]?.text || null;

  const kanaResult = await db<{text: string}[]>`
    SELECT text FROM kana_text WHERE seq = ${seq} AND ord = 0 LIMIT 1
  `;
  const kanaText = kanaResult[0]?.text || '';

  return readingStrBase(kanjiText, kanaText);
}

// Line 1586-1590: defgeneric reading-str
export async function readingStr(obj: SimpleText | number | WordInfo): Promise<string> {
  if (typeof obj === 'number') {
    return readingStrSeq(obj);
  } else if (obj instanceof WordInfo) {
    return wordInfoReadingStr(obj);
  } else {
    // SimpleText (KanjiText or KanaText)
    // Use getKanji/getKana to properly handle bestKanjiConj for KanaText
    const kanji = await getKanji(obj as KanjiText | KanaText);
    const kana = await getKana(obj as KanjiText | KanaText);
    return readingStrBase(kanji, typeof kana === 'string' ? kana : '');
  }
}

// Line 1725-1735: defun word-info-reading-str
function wordInfoReadingStr(wordInfo: WordInfo): string {
  const isKanji = wordInfo.type === 'kanji';
  const hasCounter = wordInfo.counter && wordInfo.seq;

  if (isKanji || hasCounter) {
    return readingStrBase(wordInfo.text, wordInfo.kana);
  } else {
    return readingStrBase(null, wordInfo.text);
  }
}

// Line 1701-1723: defun simplify-reading-list
export function simplifyReadingList(readingList: string[]): string[] {
  const assoc: Array<[string, number, number[]]> = [];

  for (const reading of readingList) {
    const can: string[] = [];
    const spos: number[] = [];
    let off = 0;

    for (let i = 0; i < reading.length; i++) {
      const char = reading[i];
      if (char === ' ') {
        spos.push(i - off);
        off++;
      } else {
        can.push(char);
      }
    }

    const canStr = can.join('');
    const existing = assoc.find(([c]) => c === canStr);

    if (existing) {
      existing[1]++;
      existing[2].push(...spos);
    } else {
      assoc.push([canStr, 1, spos]);
    }
  }

  // Line 1715: Lisp uses (nreverse assoc) before mapping
  // In Lisp, push adds to front, so assoc is built in reverse order
  // But we need to preserve the INPUT order, not reverse it
  // The nreverse in Lisp restores the original order that was reversed by push
  // In TypeScript, push maintains order, so NO reverse needed
  return assoc.map(([can, cnt, spos]) => {
    const sspos = [...new Set(spos)].sort((a, b) => a - b);
    const chars: string[] = [];
    let sposIdx = 0;

    for (let i = 0; i < can.length; i++) {
      if (sspos[sposIdx] === i) {
        const allMatch = spos.filter(p => p === sspos[sposIdx]).length === cnt;
        chars.push(allMatch ? ' ' : '·');
        sposIdx++;
      }
      chars.push(can[i]);
    }

    return chars.join('');
  });
}

// Line 1725-1729: defun map-word-info-kana
function mapWordInfoKana(fn: (s: string) => string, wordInfo: WordInfo, separator = '/'): string {
  const wkana = wordInfo.kana;

  if (Array.isArray(wkana)) {
    return simplifyReadingList(wkana.map(fn)).join(separator);
  } else {
    return fn(wkana);
  }
}

// Line 285-295: defun conj-prop-json
// Conjugation prop to JSON helper
function conjPropToJson(prop: ConjProp): ConjPropJson {
  const result: ConjPropJson = {
    pos: prop.pos,
    type: getConjDescription(prop.conjType) // Line 288: (get-conj-description (conj-type obj))
  };

  // Line 291-294: Only add neg/fml if not null/false
  if (prop.fml) result.fml = true;
  if (prop.neg) result.neg = true;

  return result;
}

// Line 1662-1699: defun conj-info-json
async function conjInfoJson(
  seq: number,
  options: {
    conjugations?: number[] | ':root';
    text?: string | string[];
    hasGloss?: boolean;
  } = {}
): Promise<ConjInfoJson[]> {
  const { conjugations, text, hasGloss } = options;
  const db = getConnection();

  // Get conjugations
  let conjs: Conjugation[];
  if (conjugations && conjugations !== ':root') {
    // Filter out any undefined/null values that might come from ProxyText delegation
    const validConjs = conjugations.filter(c => c != null);
    conjs = await db<Conjugation[]>`
      SELECT * FROM conjugation WHERE seq = ${seq} AND id IN ${db(validConjs)} ORDER BY id
    `;
  } else if (conjugations === ':root') {
    // :root is a sentinel value meaning "don't show conjugation details"
    // Used for auxiliary verbs in compounds (ください, ておく, ていく, etc.)
    // Lisp's (unless (eql conj-ids :root) ...) returns NIL when :root
    conjs = [];
  } else {
    const nullVia = await db<Conjugation[]>`
      SELECT * FROM conjugation WHERE seq = ${seq} AND via IS NULL ORDER BY id
    `;
    conjs = nullVia.length > 0 ? nullVia : await db<Conjugation[]>`
      SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY id
    `;
  }

  // Build array with conjugations, props, and sort keys (like selectConjsAndProps)
  const conjsWithProps: Array<[Conjugation, ConjProp[], [number, number]]> = [];

  for (const conj of conjs) {
    const props = await db<ConjProp[]>`
      SELECT * FROM conj_prop WHERE conj_id = ${conj.id}
    `;

    const minVal = Math.min(...props.map((p: ConjProp) => conjTypeOrder(p.conjType)));
    const viaVal = conj.via === null ? 0 : 1;

    conjsWithProps.push([conj, props, [viaVal, minVal]]);
  }

  // Sort by via first, then by minimum conjTypeOrder (same as selectConjsAndProps)
  conjsWithProps.sort((a, b) => {
    const [viaA, minA] = a[2];
    const [viaB, minB] = b[2];
    if (viaA !== viaB) return viaA - viaB;
    return minA - minB;
  });

  const result: ConjInfoJson[] = [];
  const viaUsed: number[] = [];

  // Filter out direct conjugations (via=null) when indirect ones (via!=null) exist from the same base
  // Group by 'from' value to detect conflicts
  const fromGroups = new Map<number, Array<[Conjugation, ConjProp[], [number, number]]>>();
  for (const entry of conjsWithProps) {
    const [conj] = entry;
    const from = conj.from;
    if (!fromGroups.has(from)) {
      fromGroups.set(from, []);
    }
    fromGroups.get(from)!.push(entry);
  }

  // For each group, if there are both direct (via=null) and indirect (via!=null) entries,
  // filter out the direct ones
  const filteredConjsWithProps: typeof conjsWithProps = [];
  for (const group of fromGroups.values()) {
    const hasIndirect = group.some(([conj]) => conj.via !== null);
    if (hasIndirect) {
      // Keep only indirect conjugations
      filteredConjsWithProps.push(...group.filter(([conj]) => conj.via !== null));
    } else {
      // Keep all (they're all direct)
      filteredConjsWithProps.push(...group);
    }
  }

  for (const [conj, props] of filteredConjsWithProps) {
    const via = conj.via;

    if (via !== null && viaUsed.includes(via)) {
      continue;
    }

    const js: ConjInfoJson = {
      prop: props.map(conjPropToJson)
    };

    // Get original text from conj_source_reading (Lisp: get-original-text-once)
    const origText = await getOriginalTextOnce(
      await getConjData(seq, [conj.id]),
      text || []
    );

    if (via === null) {
      const fromSeq = conj.from;

      // Get original reading if we have original text (Lisp: find-words-seqs)
      let origReading: any = null;
      if (origText.length > 0) {
        const words = await findWordsSeqs(origText[0], fromSeq);
        origReading = words.length > 0 ? words[0] : null;
      }

      // Skip this conjugation if has-gloss is true and no orig-reading found
      // (Lisp line 1675-1676: (when (and has-gloss (not orig-reading)) (return-from outer nil)))
      if (hasGloss && !origReading) {
        continue;
      }

      js.reading = await readingStr(origReading || fromSeq);

      const conjPos = props.map(p => p.pos);
      js.gloss = await getSensesJson(fromSeq, {
        posList: conjPos,
        readingGetter: () => origReading  // Pass origReading for sense restriction filtering
      });
      js.readok = !!origReading;
    } else {
      const viaConjs = await conjInfoJson(via, { text: origText, hasGloss });
      if (viaConjs.length > 0) {
        js.via = viaConjs;
        js.readok = viaConjs[0].readok;
        viaUsed.push(via);
      } else {
        // If via conjugations are empty, skip this conjugation entirely
        continue;
      }
    }

    result.push(js);
  }

  // Always filter to only readok entries if any exist (Lisp line 1647-1649)
  // Lisp: (or fcij cij) where fcij = (remove-if-not (lambda (c) (jsown:val c "readok")) cij)
  const filtered = result.filter(c => c.readok);
  return filtered.length > 0 ? filtered : result;
}

// Helper to read WordInfo properties from either WordInfo object or JSON
function readWordInfoProp<T>(obj: WordInfo | any, prop: string, transform?: (val: any) => T): T {
  if (obj instanceof WordInfo) {
    return obj[prop as keyof WordInfo] as T;
  }
  // Plain JSON object
  let val = obj[prop];
  // Handle special conversions from JSON
  if (prop === 'type' && typeof val === 'string') {
    val = val.toLowerCase() as 'kanji' | 'kana' | 'gap';
  }
  if (prop === 'conjugations' && val === 'ROOT') {
    val = ':root';
  }
  return transform ? transform(val) : val;
}

// Line 1780-1830: defun word-info-gloss-json
export async function wordInfoGlossJson(
  wordInfo: WordInfo | any,
  options: { rootOnly?: boolean; suffix?: boolean } = {}
): Promise<WordInfoGlossJson> {
  const { rootOnly, suffix } = options;

  // Read properties (works for both WordInfo objects and JSON)
  const alternative = readWordInfoProp(wordInfo, 'alternative');
  const components = readWordInfoProp(wordInfo, 'components');
  const text = readWordInfoProp(wordInfo, 'text');
  const kana = readWordInfoProp(wordInfo, 'kana');
  const score = readWordInfoProp(wordInfo, 'score');
  const type = readWordInfoProp(wordInfo, 'type') as 'kanji' | 'kana' | 'gap';
  const seq = readWordInfoProp(wordInfo, 'seq');
  const counter = readWordInfoProp(wordInfo, 'counter');
  const conjugations = readWordInfoProp(wordInfo, 'conjugations');
  const trueText = readWordInfoProp(wordInfo, 'trueText') || readWordInfoProp(wordInfo, 'truetext');
  const primary = readWordInfoProp(wordInfo, 'primary');

  // Handle alternative case
  if (alternative) {
    if (components && Array.isArray(components) && components.length > 0) {
      const altComponents = await Promise.all(
        // Alternatives should not inherit suffix flag (Lisp line 1829)
        components.map((wi: any) => wordInfoGlossJson(wi, {}))
      );
      return { alternative: altComponents } as any;
    }
    // Alternative without components - shouldn't happen, but handle gracefully
    throw new Error(`BUG: Alternative WordInfo without components: ${JSON.stringify({ text, kana, seq })}`);
  }

  // Create reading string (simplified for JSON objects)
  const isKanji = type === 'kanji';
  const hasCounter = counter && seq;
  const reading = (isKanji || hasCounter)
    ? readingStrBase(text as string, kana as string | string[])
    : readingStrBase(null, text as string);

  const js: WordInfoGlossJson = {
    reading,
    text: text as string,
    kana: kana as string | string[]
  };

  if (score !== null && score !== undefined) {
    js.score = score as number;
  }

  // Handle components (compound words)
  if (components && Array.isArray(components)) {
    const glossComponents = await Promise.all(
      components.map((wi: any, idx: number) => {
        const wiPrimary = readWordInfoProp(wi, 'primary');
        // Pass suffix: true for non-primary components (matches Lisp line 1795)
        return wordInfoGlossJson(wi, { suffix: !wiPrimary });
      })
    );
    js.compound = components.map((wi: any) => readWordInfoProp(wi, 'text') as string);
    js.components = glossComponents;
    return js;
  }

  // Handle counter
  if (counter && Array.isArray(counter)) {
    const [value, ordinal] = counter;
    // Match Lisp jsown behavior: false (nil) → [], true (t) → true
    js.counter = { value, ordinal: ordinal ? true : [] as any };

    if (seq) {
      const seqNum = Array.isArray(seq) ? seq[0] : seq;
      js.seq = seqNum;

      const gloss = await getSensesJson(seqNum, { posList: ['ctr'] });
      if (gloss.length > 0) {
        js.gloss = gloss;
      }
    }

    return js;
  }

  // Handle regular word
  const conjs = conjugations;

  if (seq) {
    js.seq = Array.isArray(seq) ? seq[0] : seq;
  }

  if (rootOnly && js.seq) {
    // Lisp: (get-senses-json seq :reading-getter (lambda () (word-info-reading word-info)))
    js.gloss = await getSensesJson(js.seq, {
      readingGetter: () => wordInfoReading(wordInfo)
    });
    return js;
  }

  // Check for suffix description (only for non-primary components)
  // This is part of a cond-like structure with mutually exclusive branches
  let suffixAdded = false;
  if (suffix && js.seq) {
    const suffixDesc = await getSuffixDescription(js.seq);
    if (suffixDesc) {
      js.suffix = suffixDesc;
      suffixAdded = true;
      // Note: Unlike rootOnly, suffix case does NOT return early
      // It continues to add conj field below
    }
  }

  // Add gloss for root or no conjugation (but not if suffix was added)
  if (!suffixAdded && js.seq && (!conjs || conjs === ':root')) {
    // Lisp: (get-senses-json seq :reading-getter (lambda () (word-info-reading word-info)))
    const gloss = await getSensesJson(js.seq, {
      readingGetter: () => wordInfoReading(wordInfo)
    });
    if (gloss.length > 0) {
      js.gloss = gloss;
    }
  }

  // Add conjugation info
  // Note: Lisp always calls conj-info-json if seq exists, regardless of conjs value
  if (js.seq) {
    const conjInfo = await conjInfoJson(js.seq, {
      conjugations: (conjs as number[] | ':root') || undefined,
      text: (trueText as string | string[]) || undefined,
      hasGloss: !!js.gloss
    });

    // Always add conj field, even if empty (to match Lisp behavior)
    js.conj = conjInfo;
  }

  return js;
}
