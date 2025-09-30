// ichiran/dict-grammar - Grammar rules and conjugations
// Port of dict-grammar.lisp (1168 lines)

import { getConnection, withDb, defineCache } from './conn.js';
import { testWord, asHiragana } from './characters.js';
import {
  CONJ_ADVERBIAL, CONJ_ADJECTIVE_STEM, CONJ_NEGATIVE_STEM, CONJ_ADJECTIVE_LITERARY,
  WEAK_CONJ_FORMS, testConjProp, skipByConjData, SEMI_FINAL_PRT
} from './dict-errata.js';
import type {
  KanaText, KanjiText, ProxyText, CompoundText, ConjData, ConjProp,
  Segment, SegmentList, AnyWord
} from './types.js';
import { getKana, getText } from './dict.js';
import type { CounterText } from './dict-counters.js';

// ============================================================================
// TYPES & INTERFACES
// ============================================================================

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
// CACHE INFRASTRUCTURE (Lines 4-10)
// ============================================================================

// Line 5-6: def-conn-var *suffix-cache* and *suffix-class*
// These are lazily initialized mutable caches (like Lisp's def-conn-var)
let suffixCache: Map<string, SuffixCacheEntry> | null = null;
let suffixClass: Map<number, string> | null = null;

// Line 326: defparameter *suffix-list*
const suffixList = new Map<string, SuffixFunction>();

// Line 327: defparameter *suffix-unique-only*
const suffixUniqueOnly = new Map<string, ((matches: any[]) => boolean | Promise<boolean>) | true>();

// Line 720: defparameter *synergy-list*
const synergyList: SynergyFunction[] = [];

// Line 961: defparameter *penalty-list*
const penaltyList: PenaltyFunction[] = [];

// Line 1021: defparameter *segfilter-list*
const segfilterList: SegfilterFunction[] = [];

// Line 162: defvar *init-suffixes-lock*
let initSuffixesLock = false;

// Line 108-157: hash-from-list *suffix-description*
export const SUFFIX_DESCRIPTION: Record<string | number, string> = {
  ':chau': 'indicates completion (to finish ...)',
  ':ha': 'topic marker particle',
  ':tai': 'want to... / would like to...',
  ':iru': 'indicates continuing action (to be ...ing)',
  ':oru': 'indicates continuing action (to be ...ing) (humble)',
  ':aru': 'indicates completion / finished action',
  ':kuru': 'indicates action that had been continuing up till now / came to be ',
  ':oku': 'to do in advance / to leave in the current state expecting a later change',
  ':kureru': '(asking) to do something for one',
  ':morau': '(asking) to get somebody to do something',
  ':itadaku': '(asking) to get somebody to do something (polite)',
  ':iku': 'is becoming / action starting now and continuing',
  ':suru': 'makes a verb from a noun',
  ':itasu': 'makes a verb from a noun (humble)',
  ':sareru': 'makes a verb from a noun (honorific or passive)',
  ':saseru': 'let/make someone/something do ...',
  ':rou': 'probably / it seems that... / I guess ...',
  ':ii': "it's ok if ... / is it ok if ...?",
  ':mo': 'even if ...',
  ':sugiru': 'to be too (much) ...',
  ':nikui': 'difficult to...',
  ':sa': '-ness (degree or condition of adjective)',
  ':tsutsu': 'while ... / in the process of ...',
  ':tsutsuaru': 'to be doing ... / to be in the process of doing ...',
  ':uru': 'can ... / to be able to ...',
  ':sou': 'looking like ... / seeming ...',
  ':nai': 'negative suffix',
  ':ra': 'pluralizing suffix (not polite)',
  ':kudasai': 'please do ...',
  ':yagaru': 'indicates disdain or contempt',
  ':naru': 'to become ...',
  ':desu': 'formal copula',
  ':desho': "it seems/perhaps/don't you think?",
  ':tosuru': 'to try to .../to be about to...',
  ':garu': 'to feel .../have a ... impression of someone',
  ':me': 'somewhat/-ish',
  ':gai': 'worth it to ...',
  ':tasou': 'seem to want to... (tai+sou)',
  // For splitsegs
  2826528: 'polite prefix', // お
  2028980: 'at / in / by', // で
  2028970: 'or / questioning particle', // か
  2028990: 'to / at / in', // に
  2029010: 'indicates direct object of action', // を
  1469800: "indicates possessive (...'s)", // の
  2086960: 'quoting particle', // と
  1002980: 'from / because', // から
};

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Line 8-10: defun init-suffix-hashtables
function initSuffixHashtables(): void {
  suffixCache = new Map<string, SuffixCacheEntry>();
  suffixClass = new Map<number, string>();
}

// Line 159-160: defun get-suffix-description
export function getSuffixDescription(seq: number): string | null {
  const cls = suffixClass?.get(seq);
  return SUFFIX_DESCRIPTION[cls || seq] || null;
}

// Line 164-166: defun init-suffixes-running-p
function initSuffixesRunningP(): boolean {
  return !suffixCache || initSuffixesLock;
}

// Line 12-17: defun get-kana-forms-conj-data-filter
async function getKanaFormsConjDataFilter(conjData: ConjData[]): Promise<number[]> {
  if (skipByConjData(conjData)) return [];

  const result: number[] = [];
  for (const cd of conjData) {
    const prop = cd.prop;
    if (!testConjProp(prop, WEAK_CONJ_FORMS)) {
      result.push(prop.conjId);
    }
  }
  return result;
}

// Line 19-32: defun get-kana-forms*
async function getKanaForms_(seq: number): Promise<KanaText[]> {
  const sql = getConnection();

  // Union of direct kana-text and conjugated forms
  const results = await sql<KanaText[]>`
    SELECT kt.* FROM kana_text kt WHERE kt.seq = ${seq}
    UNION
    SELECT kt.* FROM kana_text kt
    LEFT JOIN conjugation conj ON conj.seq = kt.seq
    WHERE conj.from = ${seq}
  `;

  // Collect all (seq, from) pairs for conjugated forms
  const conjPairs: Array<{ ktSeq: number; from: number }> = [];
  for (const kt of results) {
    if (kt.seq !== seq) {
      conjPairs.push({ ktSeq: kt.seq, from: seq });
    }
  }

  // Batch query all conjugation data at once
  let conjDataMap = new Map<number, ConjData[]>();
  if (conjPairs.length > 0) {
    const seqs = conjPairs.map(p => p.ktSeq);
    const froms = conjPairs.map(p => p.from);

    const allConjs = await sql<any[]>`
      SELECT c.*, cp.*
      FROM conjugation c
      LEFT JOIN conj_prop cp ON cp.conj_id = c.id
      WHERE c.seq = ANY(${seqs}) AND c.from = ANY(${froms})
    `;

    // Group by seq
    for (const row of allConjs) {
      const conjData: ConjData = {
        seq: row.seq,
        from: row.from,
        via: row.via,
        prop: {
          id: row.id,
          conjId: row.conjId,
          conjType: row.conjType,
          pos: row.pos,
          neg: row.neg,
          fml: row.fml,
        },
        srcMap: [],
      };

      if (!conjDataMap.has(row.seq)) {
        conjDataMap.set(row.seq, []);
      }
      conjDataMap.get(row.seq)!.push(conjData);
    }
  }

  const output: KanaText[] = [];

  for (const kt of results) {
    if (kt.seq === seq) {
      // Root form
      kt.conjugations = ':root';
      output.push(kt);
    } else {
      // Conjugated form - use batched data
      const conjData = conjDataMap.get(kt.seq) || [];
      const conjIds = await getKanaFormsConjDataFilter(conjData);
      if (conjIds.length > 0) {
        kt.conjugations = conjIds;
        output.push(kt);
      }
    }
  }

  return output;
}

// Helper to get conjugation data (simplified version of dict.ts getConjData)
async function getConjDataHelper(seq: number, from: number): Promise<ConjData[]> {
  const sql = getConnection();

  const conjs = await sql<any[]>`
    SELECT c.*, cp.*
    FROM conjugation c
    LEFT JOIN conj_prop cp ON cp.conj_id = c.id
    WHERE c.seq = ${seq} AND c.from = ${from}
  `;

  return conjs.map(row => ({
    seq: row.seq,
    from: row.from,
    via: row.via,
    prop: {
      id: row.id,
      conjId: row.conjId,  // Fixed: camelCase due to postgres.camel transform
      conjType: row.conjType,  // Fixed: camelCase due to postgres.camel transform
      pos: row.pos,
      neg: row.neg,
      fml: row.fml,
    },
    srcMap: [],
  }));
}

// Line 34-36: defun get-kana-forms
async function getKanaForms(seq: number): Promise<KanaText[]> {
  const result = await getKanaForms_(seq);
  if (result.length === 0) {
    console.warn(`No kana forms found for: ${seq}`);
  }
  return result;
}

// Line 38-42: defun get-kana-form
async function getKanaForm(seq: number, text: string, conj?: ':root' | number[]): Promise<KanaText | null> {
  const sql = getConnection();

  const results = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE text = ${text} AND seq = ${seq} LIMIT 1
  `;

  if (results.length > 0) {
    const res = results[0];
    if (conj !== undefined) {
      res.conjugations = conj;
    }
    return res;
  }

  return null;
}

// Line 44-51: defun find-word-with-conj-prop
async function findWordWithConjProp(
  wordstr: string,
  filterFn: (cdata: ConjData) => boolean,
  options: { allowRoot?: boolean } = {}
): Promise<AnyWord[]> {
  const allWords = await findWordFull(wordstr);
  // Line 38-48: Process all words including compounds (matches Lisp behavior)
  const result: AnyWord[] = [];

  for (const word of allWords) {
    const conjData = await wordConjData(word as any);
    const conjDataFiltered = conjData.filter(filterFn);
    const conjIds = conjDataFiltered.map(cdata => cdata.prop.conjId);

    if (conjDataFiltered.length > 0 || (conjData.length === 0 && options.allowRoot)) {
      // Clone the word to avoid mutating shared objects
      const wordClone = { ...word, conjugations: conjIds.length > 0 ? conjIds : null };
      result.push(wordClone);
    }
  }

  return result;
}

// Line 53-56: defun find-word-with-conj-type
async function findWordWithConjType(word: string, ...conjTypes: number[]): Promise<AnyWord[]> {
  return findWordWithConjProp(word, (cdata) => {
    return conjTypes.includes(cdata.prop.conjType);
  });
}

// Line 58-73: defun pair-words-by-conj
async function pairWordsByConj(...wordGroups: Array<KanjiText | KanaText>[]): Promise<Array<Array<KanjiText | KanaText | null>>> {
  const sql = getConnection();

  // Collect all conjugation IDs from all words
  const allConjIds = new Set<number>();
  for (const wordGroup of wordGroups) {
    for (const word of wordGroup) {
      const conjIds = word.conjugations;
      if (conjIds && conjIds !== ':root') {
        for (const conjId of conjIds) {
          allConjIds.add(conjId);
        }
      }
    }
  }

  // Batch query all conjugations at once
  const conjMap = new Map<number, { from: number; via: number }>();
  if (allConjIds.size > 0) {
    const ids = Array.from(allConjIds);
    const conjs = await sql<any[]>`
      SELECT id, "from", via FROM conjugation WHERE id = ANY(${ids})
    `;

    for (const conj of conjs) {
      conjMap.set(conj.id, {
        from: conj.from || 0,
        via: conj.via === null ? 0 : conj.via
      });
    }
  }

  // Key function: sort conjugation seqs (now uses cached data)
  const key = (word: KanjiText | KanaText): string => {
    const conjIds = word.conjugations;
    if (!conjIds || conjIds === ':root') return '';

    const pairs: Array<[number, number]> = [];
    for (const conjId of conjIds) {
      const conj = conjMap.get(conjId);
      if (conj) {
        pairs.push([conj.from, conj.via]);
      }
    }

    pairs.sort((a, b) => a[0] - b[0] || a[1] - b[1]);
    return JSON.stringify(pairs);
  };

  const bag = new Map<string, Array<KanjiText | KanaText | null>>();

  for (let idx = 0; idx < wordGroups.length; idx++) {
    const wg = wordGroups[idx];
    for (const word of wg) {
      const k = key(word);
      let arr = bag.get(k);
      if (!arr) {
        arr = new Array(wordGroups.length).fill(null);
        bag.set(k, arr);
      }
      arr[idx] = word;
    }
  }

  return Array.from(bag.values());
}

// Line 75-77: defun find-word-seq
export async function findWordSeq(word: string, ...seqs: number[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<any[]>`
    SELECT * FROM ${sql(table)} WHERE text = ${word} AND seq = ANY(${seqs})
  `;

  return results as Array<KanjiText | KanaText>;
}

// Line 79-87: defun find-word-conj-of
export async function findWordConjOf(word: string, ...seqs: number[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  // Direct matches
  const direct = await findWordSeq(word, ...seqs);

  // Conjugated forms
  const conjugated = await sql<any[]>`
    SELECT kt.* FROM ${sql(table)} kt
    INNER JOIN conjugation conj ON kt.seq = conj.seq
    WHERE conj.from = ANY(${seqs}) AND kt.text = ${word}
  `;

  // Union by id
  const seen = new Set<number>();
  const result: Array<KanjiText | KanaText> = [];

  for (const item of [...direct, ...conjugated]) {
    if (!seen.has(item.id)) {
      seen.add(item.id);
      result.push(item);
    }
  }

  return result;
}

// Line 89-95: defun find-word-with-pos
async function findWordWithPos(word: string, ...posi: string[]): Promise<Array<KanjiText | KanaText>> {
  const sql = getConnection();
  const table = testWord(word, 'kana') ? 'kana_text' : 'kanji_text';

  const results = await sql<any[]>`
    SELECT DISTINCT kt.* FROM ${sql(table)} kt
    INNER JOIN sense_prop sp ON sp.seq = kt.seq AND sp.tag = 'pos'
    WHERE kt.text = ${word} AND sp.text = ANY(${posi})
  `;

  return results as Array<KanjiText | KanaText>;
}

// Line 97-100: defun or-as-hiragana
async function orAsHiragana<T>(fn: (...args: any[]) => Promise<T[]>, word: string, ...args: any[]): Promise<T[]> {
  let result = await fn(word, ...args);
  if (result.length === 0) {
    result = await fn(asHiragana(word), ...args);
  }
  return result;
}

// Line 102-106: defun find-word-with-suffix
async function findWordWithSuffix(wordstr: string, ...suffixClasses: string[]): Promise<Array<KanjiText | KanaText>> {
  const words = await findWordFull(wordstr);
  const result: Array<KanjiText | KanaText> = [];

  for (const word of words) {
    const seq = word.seq;
    if (Array.isArray(seq)) {
      const lastSeq = seq[seq.length - 1];
      const sufClass = suffixClass?.get(lastSeq);
      if (sufClass && suffixClasses.includes(sufClass)) {
        result.push(word);
      }
    }
  }

  return result;
}

// Placeholder imports from dict.ts - these will be imported properly
// Returns heterogeneous array - callers must filter
async function findWordFull(word: string): Promise<any[]> {
  // This is imported from dict.ts - stub for now
  const { findWordFull: fwf } = await import('./dict.js');
  return fwf(word);
}

async function wordConjData(word: KanjiText | KanaText): Promise<ConjData[]> {
  // This is imported from dict.ts - stub for now
  const { wordConjData: wcd } = await import('./dict.js');
  return wcd(word);
}

// ============================================================================
// SUFFIX INITIALIZATION (Lines 168-316)
// ============================================================================

// Line 168-316: defun init-suffixes-thread
async function initSuffixesThread(): Promise<void> {
  if (!suffixCache || !suffixClass) {
    throw new Error('Suffix caches not initialized');
  }

  const cache = suffixCache;
  const classMap = suffixClass;

  // Line 171-182: update-suffix-cache
  function updateSuffixCache(text: string, newVal: SuffixValue, options: { join?: boolean } = {}): void {
    const old = cache.get(text);

    if (!old) {
      cache.set(text, newVal);
    } else if (options.join) {
      if (Array.isArray(old) && Array.isArray(old[0])) {
        // Already an array of values
        cache.set(text, [newVal, ...(old as SuffixValue[])]);
      } else {
        // Single value, convert to array
        cache.set(text, [newVal, old as SuffixValue]);
      }
    } else {
      // Overwrite
      cache.set(text, newVal);
    }
  }

  // Line 183-185: load-kf
  function loadKf(key: string, kf: KanaText | null, options: { class?: string; text?: string; join?: boolean } = {}): void {
    const text = options.text || (kf ? kf.text : '');
    updateSuffixCache(text, [key, kf], { join: options.join });
    if (kf) {
      classMap.set(kf.seq, options.class || key);
    }
  }

  // Line 186-188: load-conjs
  async function loadConjs(key: string, seq: number, cls?: string, join?: boolean): Promise<void> {
    const forms = await getKanaForms(seq);
    for (const kf of forms) {
      loadKf(key, kf, { class: cls, join });
    }
  }

  // Line 189-190: load-abbr
  function loadAbbr(key: string, text: string, options: { join?: boolean } = {}): void {
    updateSuffixCache(text, [key, null], { join: options.join });
  }

  // Line 192-315: Load all suffixes
  await loadConjs(':chau', 2013800); // ちゃう
  await loadConjs(':chau', 2210750); // ちまう
  loadKf(':chau', await getKanaForm(2028920, 'は'), { class: ':ha', text: 'ちゃ' });
  loadKf(':chau', await getKanaForm(2028920, 'は'), { class: ':ha', text: 'じゃ' });

  await loadConjs(':tai', 2017560);

  // Because suffix た is used up by いる this combination cannot occur, so add separately
  loadKf(':tai', await getKanaForm(900000, 'たそう'), { class: ':tasou' });

  await loadConjs(':ren-', 2772730, ':nikui');

  await loadConjs(':te', 1577985, ':oru'); // おる
  await loadConjs(':te', 1296400, ':aru'); // ある

  // Line 208-213: Special handling for いる (る)
  const iruForms = await getKanaForms(1577980);
  for (const kf of iruForms) {
    const tkf = kf.text;
    cache.set(tkf, [tkf.length > 1 ? ':teiru+' : ':teiru', kf]);
    classMap.set(kf.seq, ':iru');
    if (tkf.length > 1) {
      cache.set(tkf.substring(1), [':teiru', kf]);
    }
  }

  await loadConjs(':te', 1547720, ':kuru'); // くる
  await loadConjs(':te', 1421850, ':oku'); // おく
  await loadConjs(':to', 2108590, ':oku'); // とく
  await loadConjs(':te', 1305380, ':chau'); // しまう
  await loadConjs(':te+space', 1269130, ':kureru'); // くれる
  await loadConjs(':te+space', 1535910, ':morau'); // もらう
  await loadConjs(':te+space', 1587290, ':itadaku'); // いただく

  // Line 226-234: Special handling for いく / く
  const ikuForms = await getKanaForms(1578850);
  for (const kf of ikuForms) {
    const tkf = kf.text;
    const tkfShort = tkf.substring(1);
    const val: SuffixValue = [':te', kf];

    if (tkf.charCodeAt(0) === 0x3044) { // HIRAGANA_LETTER_I
      cache.set(tkf, val);
      classMap.set(kf.seq, ':iku');
      if (!cache.has(tkfShort)) {
        cache.set(tkfShort, val);
      }
    }
  }

  loadKf(':teii', await getKanaForm(2820690, 'いい'), { class: ':ii' });
  loadKf(':teii', await getKanaForm(900001, 'もいい'), { class: ':ii', text: 'もいい' });
  loadKf(':te', await getKanaForm(2028940, 'も'), { class: ':mo' });

  loadKf(':kudasai', await getKanaForm(1184270, 'ください', ':root'));

  await loadConjs(':suru', 1157170); // する
  await loadConjs(':suru', 1421900, ':itasu'); // いたす
  await loadConjs(':suru', 2269820, ':sareru'); // される
  await loadConjs(':suru', 1005160, ':saseru'); // させる

  await loadConjs(':sou', 1006610); // そう
  await loadConjs(':sou+', 2141080); // そうにない

  loadKf(':rou', await getKanaForm(1928670, 'だろう'), { text: 'ろう' });

  await loadConjs(':sugiru', 1195970); // すぎる

  loadKf(':sa', await getKanaForm(2029120, 'さ'));

  loadKf(':ren', await getKanaForm(1008120, 'つつ'), { class: ':tsutsu' });
  await loadConjs(':ren', 2027910, ':tsutsuaru');

  loadKf(':ren', await getKanaForm(1454500, 'うる'), { class: ':uru' });
  const nakuWords = await findWordConjOf('なく', 1529520);
  if (nakuWords.length > 0) {
    // nakuWords can be KanaText or KanjiText, but we only use KanaText
    const nakuKana = nakuWords[0] as KanaText;
    loadKf(':neg', nakuKana, { class: ':nai' });
  }

  await loadConjs(':adv', 1375610, ':naru'); // なる
  await loadConjs(':teren', 1012740, ':yagaru');

  loadKf(':ra', await getKanaForm(2067770, 'ら'));
  await loadConjs(':rashii', 1013240); // らしい

  loadKf(':desu', await getKanaForm(1628500, 'です'));
  loadKf(':desho', await getKanaForm(1008420, 'でしょう'));
  loadKf(':desho', await getKanaForm(1008420, 'でしょ'));

  await loadConjs(':tosuru', 2136890); // とする

  loadKf(':kurai', await getKanaForm(1154340, 'くらい'));
  loadKf(':kurai', await getKanaForm(1154340, 'ぐらい'));

  await loadConjs(':garu', 1631750); // がる

  loadKf(':ren', await getKanaForm(2016470, 'がち'), { class: ':gachi' });
  loadKf(':iadj', await getKanaForm(2006580, 'げ'));
  loadKf(':iadj', await getKanaForm(1604890, 'め'), { class: ':me' });

  loadKf(':ren-', await getKanaForm(2606690, 'がい'), { class: ':gai' });

  // Abbreviations
  loadAbbr(':nai', 'ねえ');
  loadAbbr(':nai', 'ねぇ');
  loadAbbr(':nai', 'ねー');
  loadAbbr(':nai-x', 'ず');
  loadAbbr(':nai-x', 'ざる');
  loadAbbr(':nai-x', 'ぬ');
  loadAbbr(':nai-n', 'ん');

  loadAbbr(':nakereba', 'なきゃ');
  loadAbbr(':nakereba', 'なくちゃ');

  // loadAbbr(':eba', 'や'); // Conflicts with noun + や
  loadAbbr(':teba', 'ちゃ', { join: true }); // つ
  loadAbbr(':reba', 'りゃ'); // る
  loadAbbr(':keba', 'きゃ'); // く
  loadAbbr(':geba', 'ぎゃ'); // ぐ
  loadAbbr(':neba', 'にゃ'); // ぬ
  loadAbbr(':beba', 'びゃ'); // ぶ
  loadAbbr(':meba', 'みゃ'); // む
  loadAbbr(':seba', 'しゃ'); // す

  loadAbbr(':shimashou', 'しましょ');
  loadAbbr(':dewanai', 'じゃない');
  loadAbbr(':ii', 'ええ');
}

// Line 318-324: defun init-suffixes
export async function initSuffixes(options: { blocking?: boolean; reset?: boolean } = {}): Promise<boolean> {
  if (options.reset || !suffixCache) {
    initSuffixHashtables();

    if (initSuffixesLock && !options.blocking) {
      // Already initializing
      return true;
    }

    initSuffixesLock = true;

    try {
      await initSuffixesThread();
    } finally {
      initSuffixesLock = false;
    }
  }

  return initSuffixesRunningP();
}

// ============================================================================
// SUFFIX DEFINITIONS (Lines 329-660)
// ============================================================================

// Helper: Remove stem characters from end of string
// Line 358, 360: (destem k stem-length)
function destem(str: string, stemLength: number): string {
  if (stemLength === 0) return str;
  return str.substring(0, str.length - stemLength);
}

// Import adjoinWord from dict.ts
async function adjoinWord(
  word1: any,
  word2: any,
  options: {
    text?: string;
    kana?: string;
    scoreMod?: number | ((score: number) => number);
    scoreBase?: any;
  } = {}
): Promise<CompoundText> {
  const { adjoinWord: aw } = await import('./dict.js');

  // Pass scoreMod directly - it's already in the correct format
  // (either a number or a constantly-wrapped function)
  return aw(word1, word2, options);
}

// Line 329-333: defmacro defsuffix
// TypeScript: Register suffix function
function defsuffix(keyword: string, fn: SuffixFunction): void {
  suffixList.set(keyword, fn);
}

// Line 337-365: defmacro def-simple-suffix
// TypeScript: Higher-order function that creates and registers a suffix
function defSimpleSuffix(
  name: string,
  keyword: string,
  options: {
    stem?: number;
    score?: ScoreValue;
    connector?: string;
  },
  getPrimaryWords: (root: string, suffix: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }) => Promise<Array<AnyWord | [KanjiText | KanaText, any]>>
): void {
  const stem = options.stem ?? 0;
  const score = options.score ?? 0;
  const connector = options.connector ?? '';

  const suffixFn: SuffixFunction = async (root: string, suffix: string, kf: KanaText | null) => {
    // Line 345: *suffix-map-temp* rebinding pattern
    // When stem === 0, preserve current value; otherwise set to null (disable recursive suffix detection)
    const oldSuffixMapTemp = suffixMapTempVar;
    suffixMapTempVar = stem === 0 ? suffixMapTempVar : null;

    try {
      // Patch holder (mutable reference pattern from Lisp)
      let patchValue: [string, string] | null = null;
      const patch = {
        set: (p: [string, string]) => { patchValue = p; },
        get: () => patchValue
      };

      // Get primary words
      const primaryWords = await getPrimaryWords(root, suffix, patch);

    // Line 348-364: Map over primary words to create compound texts
    const results: CompoundText[] = [];
    for (const pw of primaryWords) {
      let word: AnyWord;
      let scoreBase: any = undefined;

      // Check if pw is [word, scoreBase] tuple
      if (Array.isArray(pw)) {
        scoreBase = pw[1];
        word = pw[0];
      } else {
        word = pw;
      }

      // Skip proxy texts or other non-standard words
      if (!('seq' in word || ('words' in word && Array.isArray(word.words)))) {
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

      const compound = await adjoinWord(word, kf, {
        text: root + suffix,
        kana,
        scoreMod,
        scoreBase,
      });

      results.push(compound);
    }

    return results;
    } finally {
      suffixMapTempVar = oldSuffixMapTemp;
    }
  };

  defsuffix(keyword, suffixFn);
}

// Dynamic variables for suffix rebinding (Line 335, 345, 552, 703)
// These are set temporarily during findWordSuffix execution
let suffixMapTempVar: Map<number, ParsedSuffix[]> | null = null;
let suffixNextEndVar: number | undefined = undefined;

// Line 367-369: def-simple-suffix suffix-tai
defSimpleSuffix('suffix-tai', ':tai', { connector: '', score: 5 }, async (root) => {
  if (root === 'い') return [];
  return await findWordWithConjType(root, 13);
});

// Line 371-373: def-simple-suffix suffix-ren
defSimpleSuffix('suffix-ren', ':ren', { connector: '', score: 5 }, async (root) => {
  // generic ren'youkei suffix
  return await findWordWithConjType(root, 13);
});

// Line 375-376: def-simple-suffix suffix-ren-
defSimpleSuffix('suffix-ren-', ':ren-', { connector: '', score: 0 }, async (root) => {
  return await findWordWithConjType(root, 13);
});

// Line 378-379: def-simple-suffix suffix-neg
defSimpleSuffix('suffix-neg', ':neg', { connector: '', score: 5 }, async (root) => {
  return await findWordWithConjType(root, 13, CONJ_NEGATIVE_STEM);
});

// Line 381-384: defun te-check
async function teCheck(root: string): Promise<AnyWord[]> {
  if (root === 'で') return [];
  const lastChar = root[root.length - 1];
  if (lastChar !== 'て' && lastChar !== 'で') return [];
  return await findWordWithConjType(root, 3);
}

// Line 386-387: def-simple-suffix suffix-te
defSimpleSuffix('suffix-te', ':te', { connector: '', score: 0 }, async (root) => {
  return await teCheck(root);
});

// Line 389-390: defun teiru-check
async function teiruCheck(root: string): Promise<AnyWord[]> {
  if (root === 'いて') return [];
  return await teCheck(root);
}

// Line 392-393: def-simple-suffix suffix-teiru
defSimpleSuffix('suffix-teiru', ':teiru', { connector: '', score: 3 }, async (root) => {
  return await teiruCheck(root);
});

// Line 395-396: def-simple-suffix suffix-teiru+
defSimpleSuffix('suffix-teiru+', ':teiru+', { connector: '', score: 6 }, async (root) => {
  return await teiruCheck(root);
});

// Line 398-399: def-simple-suffix suffix-te+space
defSimpleSuffix('suffix-te+space', ':te+space', { connector: ' ', score: 3 }, async (root) => {
  return await teCheck(root);
});

// Line 401-402: def-simple-suffix suffix-kudasai
defSimpleSuffix('suffix-kudasai', ':kudasai', { connector: ' ', score: () => 360 }, async (root) => {
  return await teCheck(root);
});

// Line 404-409: def-simple-suffix suffix-te-ren (teren)
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

// Line 411-413: def-simple-suffix suffix-teii
defSimpleSuffix('suffix-teii', ':teii', { connector: ' ', score: 1 }, async (root) => {
  const lastChar = root[root.length - 1];
  if (lastChar !== 'て' && lastChar !== 'で') return [];
  return await findWordWithConjType(root, 3);
});

// Line 415-420: def-simple-suffix suffix-chau
defSimpleSuffix('suffix-chau', ':chau', { stem: 1, connector: '', score: 5 }, async (root, suffix) => {
  const firstChar = suffix[0];
  let te: string | null = null;

  if (firstChar === 'じ') te = 'で';  // HIRAGANA_LETTER_ZI
  else if (firstChar === 'ち') te = 'て';  // HIRAGANA_LETTER_TI

  if (!te) return [];
  return await findWordWithConjType(root + te, 3);
});

// Line 422-427: def-simple-suffix suffix-to
defSimpleSuffix('suffix-to', ':to', { stem: 1, connector: '', score: 0 }, async (root, suffix) => {
  const firstChar = suffix[0];
  let te: string | null = null;

  if (firstChar === 'と') te = 'て';  // HIRAGANA_LETTER_TO
  else if (firstChar === 'ど') te = 'で';  // HIRAGANA_LETTER_DO

  if (!te) return [];
  return await findWordWithConjType(root + te, 3);
});

// Line 429-430: def-simple-suffix suffix-suru
defSimpleSuffix('suffix-suru', ':suru', { connector: ' ', score: 5 }, async (root) => {
  return await findWordWithPos(root, 'vs');
});

// Line 432-433: defun apply-patch
function applyPatch(root: string, patch: [string, string]): string {
  return root.substring(0, root.length - patch[1].length) + patch[0];
}

// Line 435-443: defmacro suffix-sou-base
async function suffixSouBase(root: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }): Promise<Array<KanjiText | KanaText | CompoundText>> {
  if (root.endsWith('なさ')) {
    patch.set(['い', 'さ']);
    const patchedRoot = applyPatch(root, ['い', 'さ']);
    // *suffix-map-temp* nil: disable recursive suffix detection
    const oldSuffixMapTemp = suffixMapTempVar;
    suffixMapTempVar = null;
    try {
      return await findWordWithConjProp(patchedRoot, (cdata) => {
        return cdata.prop.neg !== false;
      }) as Array<KanjiText | KanaText | CompoundText>;
    } finally {
      suffixMapTempVar = oldSuffixMapTemp;
    }
  } else if (!['な', 'よ', 'よさ', 'に', 'き'].includes(root)) {
    return await findWordWithConjType(root, 13, CONJ_ADJECTIVE_STEM, CONJ_ADVERBIAL) as Array<KanjiText | KanaText | CompoundText>;
  }
  return [];
}

// Line 445-452: def-simple-suffix suffix-sou
defSimpleSuffix('suffix-sou', ':sou', {
  connector: '',
  score: (root) => {
    if (root === 'から') return 40;
    if (root === 'い') return 0;
    if (root === '出来') return 100;
    return 60;
  }
}, async (root, suffix, patch) => {
  return await suffixSouBase(root, patch);
});

// Line 454-456: def-simple-suffix suffix-sou+
defSimpleSuffix('suffix-sou+', ':sou+', { connector: '', score: 1 }, async (root, suffix, patch) => {
  return await suffixSouBase(root, patch);
});

// Line 458-459: def-simple-suffix suffix-rou
defSimpleSuffix('suffix-rou', ':rou', { connector: '', score: 1 }, async (root) => {
  return await findWordWithConjType(root, 2);
});

// Line 461-462: def-simple-suffix suffix-adv
defSimpleSuffix('suffix-adv', ':adv', { connector: '', score: 1 }, async (root) => {
  return await findWordWithConjType(root, CONJ_ADVERBIAL);
});

// Line 464-476: def-simple-suffix suffix-sugiru
defSimpleSuffix('suffix-sugiru', ':sugiru', { stem: 1, connector: '', score: 5 }, async (root, suffix, patch) => {
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

// Line 478-481: def-simple-suffix suffix-sa
defSimpleSuffix('suffix-sa', ':sa', { connector: '', score: 2 }, async (root) => {
  const results1 = await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
  const results2 = await findWordWithPos(root, 'adj-na');
  return [...results1, ...results2];
});

// Line 483-487: pushnew :sa unique-only filter
suffixUniqueOnly.set(':sa', async (matches: any[]) => {
  const sql = getConnection();
  const seqs = matches.map(m => m.seq).filter((s): s is number => s != null);
  if (seqs.length === 0) return false;

  const result = await sql<{ seq: number }[]>`
    SELECT seq FROM entry WHERE seq = ANY(${seqs}) AND root_p = true
  `;
  return result.length > 0;
});

// Line 489-490: def-simple-suffix suffix-iadj
defSimpleSuffix('suffix-iadj', ':iadj', { connector: '', score: 1 }, async (root) => {
  return await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
});

// Line 492-499: def-simple-suffix suffix-garu
defSimpleSuffix('suffix-garu', ':garu', { connector: '', score: 0 }, async (root, suffix, patch) => {
  if (['な', 'い', 'よ'].includes(root)) return [];

  const result1 = await findWordWithConjType(root, CONJ_ADJECTIVE_STEM);
  if (result1.length > 0) return result1;

  if (root.endsWith('そ')) {
    patch.set(['う', '']);
    const patchedRoot = applyPatch(root, ['う', '']);
    // *suffix-map-temp* nil: disable recursive suffix detection
    const oldSuffixMapTemp = suffixMapTempVar;
    suffixMapTempVar = null;
    try {
      return await findWordWithSuffix(patchedRoot, ':sou');
    } finally {
      suffixMapTempVar = oldSuffixMapTemp;
    }
  }

  return [];
});

// Line 501-504: def-simple-suffix suffix-ra
defSimpleSuffix('suffix-ra', ':ra', { connector: '', score: 1 }, async (root) => {
  if (root.endsWith('ら')) return [];

  const result1 = await orAsHiragana(findWordWithPos, root, 'pn');
  if (result1.length > 0) return result1;

  return await findWordSeq(root, 1580640);
});

// Line 506: pushnew :ra unique-only
suffixUniqueOnly.set(':ra', true);

// Line 508-511: def-simple-suffix suffix-rashii
defSimpleSuffix('suffix-rashii', ':rashii', { connector: '', score: 3 }, async (root) => {
  const words1 = await findWordWithConjType(root, 2);
  const words2 = await findWordWithConjType(root + 'ら', 11);
  // Filter to only simple words for pairWordsByConj
  const simpleWords1 = words1.filter((w): w is KanjiText | KanaText => 'seq' in w && !('words' in w));
  const simpleWords2 = words2.filter((w): w is KanjiText | KanaText => 'seq' in w && !('words' in w));
  return (await pairWordsByConj(simpleWords1, simpleWords2)).flat().filter((w): w is KanjiText | KanaText => w !== null);
});

// Line 513-517: def-simple-suffix suffix-desu
defSimpleSuffix('suffix-desu', ':desu', { connector: ' ', score: () => 200 }, async (root) => {
  if (!root.endsWith('ない') && !root.endsWith('なかった')) return [];

  return await findWordWithConjProp(root, (cdata) => {
    return cdata.prop.neg !== false;
  });
});

// Line 519-527: pushnew :desu unique-only filter
suffixUniqueOnly.set(':desu', async (matches: any[]) => {
  const sql = getConnection();
  const seqs = matches.map(m => m.seq).filter((s): s is number => s != null);
  if (seqs.length === 0) return false;

  const result = await sql<{ seq: number }[]>`
    SELECT seq FROM conjugation WHERE seq = ANY(${seqs}) AND "from" = 2755350
  `;
  // じゃない (2755350)
  return result.length < matches.length;
});

// Line 529-532: def-simple-suffix suffix-desho
defSimpleSuffix('suffix-desho', ':desho', { connector: ' ', score: () => 300 }, async (root) => {
  if (!root.endsWith('ない')) return [];

  return await findWordWithConjProp(root, (cdata) => {
    return cdata.prop.neg !== false;
  });
});

// Line 534-535: def-simple-suffix suffix-tosuru
defSimpleSuffix('suffix-tosuru', ':tosuru', { connector: ' ', score: 3 }, async (root) => {
  return await findWordWithConjType(root, 9);
});

// Line 537-538: def-simple-suffix suffix-kurai
defSimpleSuffix('suffix-kurai', ':kurai', { connector: ' ', score: 3 }, async (root) => {
  return await findWordWithConjType(root, 2);
});

// Line 540-542: pushnew unique-only markers
suffixUniqueOnly.set(':mo', true);
suffixUniqueOnly.set(':nikui', true);
suffixUniqueOnly.set(':gai', true);

// ============================================================================
// ABBREVIATION SUFFIXES (Lines 544-660)
// ============================================================================

// Line 544-576: defmacro def-abbr-suffix
// TypeScript: Higher-order function for abbreviation suffixes
function defAbbrSuffix(
  name: string,
  keyword: string,
  stem: number,
  getPrimaryWords: (root: string, suffix: string, patch: { set: (p: [string, string]) => void; get: () => [string, string] | null }) => Promise<Array<KanjiText | KanaText | CompoundText>>
): void {
  const suffixFn: SuffixFunction = async (root: string, suffix: string, kf: KanaText | null) => {
    // Line 552: *suffix-map-temp* nil - disable recursive suffix detection
    // (kf is ignored for abbr suffixes)
    const oldSuffixMapTemp = suffixMapTempVar;
    suffixMapTempVar = null;

    try {
      let patchValue: [string, string] | null = null;
      const patch = {
        set: (p: [string, string]) => { patchValue = p; },
        get: () => patchValue
      };

      const primaryWords = await getPrimaryWords(root, suffix, patch);
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
      if ('words' in pw) {
        // compound-text: modify in place
        (pw as CompoundText).text = text;
        (pw as CompoundText).kana = kana;
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
    } finally {
      suffixMapTempVar = oldSuffixMapTemp;
    }
  };

  defsuffix(keyword, suffixFn);
}

// Line 579-586: def-abbr-suffix abbr-nee
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

// Line 588-597: def-abbr-suffix abbr-nx
defAbbrSuffix('abbr-nx', ':nai-x', 2, async (root, suffix, patch) => {
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

// Line 599-605: def-abbr-suffix abbr-n
defAbbrSuffix('abbr-n', ':nai-n', 2, async (root) => {
  return await findWordWithConjProp(
    root + 'ない',
    (cdata) => {
      // 居ない (1577980) 来ない (1547720) create problems so they are blocked
      return cdata.from !== 1577980 && cdata.from !== 1547720 && cdata.prop.neg !== false;
    }
  ) as Array<KanjiText | KanaText | CompoundText>;
});

// Line 607: pushnew :nai-n unique-only
suffixUniqueOnly.set(':nai-n', true);

// Line 609-610: def-abbr-suffix abbr-nakereba
defAbbrSuffix('abbr-nakereba', ':nakereba', 4, async (root) => {
  return await findWordFull(root + 'なければ');
});

// Line 612-613: def-abbr-suffix abbr-shimasho
defAbbrSuffix('abbr-shimasho', ':shimashou', 5, async (root) => {
  return await findWordFull(root + 'しましょう');
});

// Line 615-616: def-abbr-suffix abbr-dewanai
defAbbrSuffix('abbr-dewanai', ':dewanai', 4, async (root) => {
  return await findWordFull(root + 'ではない');
});

// Line 618: pushnew :dewanai unique-only
suffixUniqueOnly.set(':dewanai', true);

// Line 620-621: Commented out :eba (conflicts with noun + や)
// defAbbrSuffix('abbr-eba', ':eba', 2, async (root) => {
//   return await findWordFull(root + 'えば');
// });

// Line 623-645: All the ~eba abbreviations
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

// Line 647-655: pushnew unique-only markers for all ~eba forms
suffixUniqueOnly.set(':eba', true);
suffixUniqueOnly.set(':teba', true);
suffixUniqueOnly.set(':reba', true);
suffixUniqueOnly.set(':keba', true);
suffixUniqueOnly.set(':geba', true);
suffixUniqueOnly.set(':neba', true);
suffixUniqueOnly.set(':beba', true);
suffixUniqueOnly.set(':meba', true);
suffixUniqueOnly.set(':seba', true);

// Line 657-658: def-abbr-suffix abbr-ii
defAbbrSuffix('abbr-ii', ':ii', 2, async (root) => {
  return await findWordFull(root + 'いい');
});

// Line 660: pushnew :ii unique-only
suffixUniqueOnly.set(':ii', true);

// ============================================================================
// SUFFIX LOOKUP & MAIN FUNCTIONS (Lines 662-704)
// ============================================================================

// Line 662-666: defun parse-suffix-val
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

// Line 668-677: defun get-suffix-map
export async function getSuffixMap(str: string): Promise<Map<number, ParsedSuffix[]>> {
  await initSuffixes();
  if (!suffixCache) throw new Error('Suffix cache not initialized');

  const result = new Map<number, ParsedSuffix[]>();

  for (let start = 0; start < str.length; start++) {
    for (let end = start + 1; end <= str.length; end++) {
      const substr = str.substring(start, end);
      const val = suffixCache.get(substr);
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

// Line 679-684: defun get-suffixes
export function getSuffixes(word: string): ParsedSuffix[] {
  if (!suffixCache) return [];

  const results: ParsedSuffix[] = [];

  for (let start = word.length - 1; start >= 1; start--) {
    const substr = word.substring(start);
    const val = suffixCache.get(substr);
    results.push(...parseSuffixVal(substr, val));
  }

  return results;
}

// Line 686-690: defun match-unique
async function matchUnique(suffixClassKey: string, matches: any[]): Promise<boolean> {
  const uniq = suffixUniqueOnly.get(suffixClassKey);

  if (typeof uniq === 'function') {
    return await uniq(matches);
  }

  return uniq === true;
}

// Line 692-704: defun find-word-suffix
export async function findWordSuffix(
  word: string,
  options: { matches?: any[]; suffixMapTemp?: Map<number, ParsedSuffix[]>; suffixNextEnd?: number } = {}
): Promise<Array<KanjiText | KanaText | ProxyText | CompoundText>> {
  await initSuffixes();

  const matches = options.matches || [];
  let suffixes: ParsedSuffix[];

  // Line 693-695: Check if using temporary suffix map (for recursive calls)
  if (options.suffixMapTemp && options.suffixNextEnd !== undefined) {
    suffixes = options.suffixMapTemp.get(options.suffixNextEnd) || [];
  } else {
    suffixes = getSuffixes(word);
  }

  const results: Array<KanjiText | KanaText | ProxyText | CompoundText> = [];

  for (const [suffix, keyword, kf] of suffixes) {
    const suffixFn = suffixList.get(keyword);
    const suffixClassKey = kf ? (suffixClass?.get(kf.seq) || keyword) : keyword;
    const offset = word.length - suffix.length;

    // Line 700-702: Validate suffix
    if (!suffixFn || offset <= 0) {
      continue;
    }
    if (matches.length > 0 && await matchUnique(suffixClassKey, matches)) {
      continue;
    }

    // Line 703: Set suffix-next-end for recursive suffix detection
    const suffixNextEnd = options.suffixNextEnd ? options.suffixNextEnd - suffix.length : undefined;

    // Temporarily set dynamic variables for nested calls (Lisp's let rebinding)
    const oldSuffixMapTemp = suffixMapTempVar;
    const oldSuffixNextEnd = suffixNextEndVar;
    suffixMapTempVar = options.suffixMapTemp || null;
    suffixNextEndVar = suffixNextEnd;

    try {
      const rootPart = word.substring(0, offset);
      const compounds = await suffixFn(rootPart, suffix, kf);

      // Lisp uses nconc - no filtering, just concatenate all results
      results.push(...compounds);
    } finally {
      suffixMapTempVar = oldSuffixMapTemp;
      suffixNextEndVar = oldSuffixNextEnd;
    }
  }

  return results;
}

// ============================================================================
// SYNERGIES SYSTEM (Lines 706-957)
// ============================================================================

// Line 710-713: defstruct synergy (already defined in types at top)
// Synergy interface is exported above

// Line 712-713: defmethod get-segment-score
// In TypeScript, this is just accessing synergy.score

// Line 715-718: defun make-segment-list-from
function makeSegmentListFrom(oldSegmentList: SegmentList, segments: Segment[]): SegmentList {
  return {
    ...oldSegmentList,
    segments
  };
}

// Line 722-726: defmacro defsynergy
function defsynergy(name: string, fn: SynergyFunction): void {
  synergyList.push(fn);
}

// Line 728-743: defmacro def-generic-synergy
function defGenericSynergy(
  name: string,
  filterLeft: (segment: Segment) => boolean,
  filterRight: (segment: Segment) => boolean,
  options: {
    description: string;
    connector: string;
    score: number | ((l: SegmentList, r: SegmentList) => number);
  }
): void {
  const synergyFn: SynergyFunction = async (segmentListLeft: SegmentList, segmentListRight: SegmentList) => {
    const start = segmentListLeft.end;
    const end = segmentListRight.start;

    // Line 734: Only combine adjacent segments
    if (start !== end) return null;

    // Line 735-736: Filter segments
    const left = segmentListLeft.segments.filter(filterLeft);
    const right = segmentListRight.segments.filter(filterRight);

    // Line 737-743: Create synergy if both sides match
    if (left.length > 0 && right.length > 0) {
      const score = typeof options.score === 'function'
        ? options.score(segmentListLeft, segmentListRight)
        : options.score;

      return [[
        makeSegmentListFrom(segmentListRight, right),
        {
          start,
          end,
          description: options.description,
          connector: options.connector,
          score
        },
        makeSegmentListFrom(segmentListLeft, left)
      ]];
    }

    return null;
  };

  defsynergy(name, synergyFn);
}

// ============================================================================
// FILTER FUNCTIONS (Lines 745-797)
// ============================================================================

// Line 745-752: defun filter-is-noun
function filterIsNoun(segment: Segment): boolean {
  const info = segment.info || {};
  const kpcl = info.kpcl || [null, null, null, null];
  const [k, p, c, l] = kpcl;
  const posi = info.posi || [];

  // Check KPCL and POS
  if ((l || k || (p && c)) &&
      ['n', 'n-adv', 'n-t', 'adj-na', 'n-suf', 'pn'].some(pos => posi.includes(pos))) {
    return true;
  }

  // Check if counter-text with seq-set
  // Counters have 'valueString' method (from CounterText class)
  // Note: in Lisp, empty seq-set is nil (falsy), so we check length > 0
  if (segment.word && 'valueString' in segment.word && info.seqSet && info.seqSet.length > 0) {
    return true;
  }

  return false;
}

// Line 754-761: defmacro filter-is-pos
function filterIsPos(posList: string[], kpclTest: (k: any, p: any, c: any, l: any) => boolean): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info || {};
    const kpcl = info.kpcl || [null, null, null, null];
    const [k, p, c, l] = kpcl;
    const posi = info.posi || [];

    if (!kpclTest(k, p, c, l)) return false;
    return posList.some(pos => posi.includes(pos));
  };
}

// Line 763-766: defun filter-in-seq-set
function filterInSeqSet(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info || {};
    const seqSet = info.seqSet || [];
    return seqs.some(seq => seqSet.includes(seq));
  };
}

// Line 768-774: defun filter-in-seq-set-simple
function filterInSeqSetSimple(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !('seq' in word)) return false;
    const seq = word.seq;

    // Check that word is not compound (seq is not a list)
    if (Array.isArray(seq)) return false;

    const info = segment.info || {};
    const seqSet = info.seqSet || [];
    return seqs.some(s => seqSet.includes(s));
  };
}

// Line 776-780: defun filter-is-conjugation
function filterIsConjugation(conjType: number): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info || {};
    const conj = info.conj || [];
    return conj.some((cdata: ConjData) => cdata.prop.conjType === conjType);
  };
}

// Line 782-788: defun filter-is-compound-end
function filterIsCompoundEnd(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !('words' in word)) return false;

    // word is a CompoundText
    const words = (word as CompoundText).words;
    const lastWord = words[words.length - 1];
    const lastSeq = 'seq' in lastWord ? lastWord.seq : undefined;
    return lastSeq !== undefined && seqs.includes(lastSeq);
  };
}

// Line 790-796: defun filter-is-compound-end-text
function filterIsCompoundEndText(...texts: string[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !('words' in word)) return false;

    // word is a CompoundText
    const words = (word as CompoundText).words;
    const lastWord = words[words.length - 1];
    const lastText = getText(lastWord);
    return texts.includes(lastText);
  };
}

// Line 798-822: defparameter *noun-particles*
const NOUN_PARTICLES = [
  2028920, // は
  2028930, // が
  2028990, // に
  2028980, // で
  2029000, // へ
  1007340, // だけ
  1579080, // ごろ
  1525680, // まで
  2028940, // も
  1582300, // など
  2215430, // には
  1469800, // の
  1009990, // のみ
  2029010, // を
  1005120, // さえ
  2034520, // でさえ
  1005120, // すら
  1008490, // と
  1008530, // とか
  1008590, // として
  2028950, // とは
  2028960, // や
  1009600, // にとって
];

// ============================================================================
// SYNERGY DEFINITIONS (Lines 824-952)
// ============================================================================

// Line 824-829: def-generic-synergy synergy-noun-particle
defGenericSynergy('synergy-noun-particle',
  filterIsNoun,
  filterInSeqSet(...NOUN_PARTICLES),
  {
    description: 'noun+prt',
    score: (l, r) => 10 + 4 * (r.end - r.start),
    connector: ' '
  }
);

// Line 831-836: synergy-suru-verb (commented out in Lisp)
// defGenericSynergy('synergy-suru-verb',
//   filterIsPos(['vs'], (k, p, c, l) => k || l || (p && c)),
//   filterInSeqSet(1157170), // する
//   {
//     description: 'noun+suru',
//     score: 10,
//     connector: ''
//   }
// );

// Line 838-843: def-generic-synergy synergy-noun-da
defGenericSynergy('synergy-noun-da',
  filterIsNoun,
  filterInSeqSet(2089020), // だ
  {
    description: 'noun+da',
    score: 10,
    connector: ' '
  }
);

// Line 845-850: def-generic-synergy synergy-no-da
defGenericSynergy('synergy-no-da',
  filterInSeqSet(1469800, 2139720), // の, ん
  filterInSeqSet(2089020, 1007370, 1928670), // だ, です, だろう
  {
    description: 'no da/desu',
    score: 15,
    connector: ' '
  }
);

// Line 852-858: def-generic-synergy synergy-sou-nanda (TODO: remove hack)
defGenericSynergy('synergy-sou-nanda',
  filterInSeqSet(2137720),
  filterInSeqSet(2140410),
  {
    description: 'sou na n da',
    score: 50,
    connector: ' '
  }
);

// Line 860-865: def-generic-synergy synergy-no-adjectives
defGenericSynergy('synergy-no-adjectives',
  filterIsPos(['adj-no'], (k, p, c, l) => k || l || (p && c)),
  filterInSeqSet(1469800), // の
  {
    description: 'no-adjective',
    score: 15,
    connector: ' '
  }
);

// Line 867-872: def-generic-synergy synergy-na-adjectives
defGenericSynergy('synergy-na-adjectives',
  filterIsPos(['adj-na'], (k, p, c, l) => k || l || (p && c)),
  filterInSeqSet(2029110, 2028990), // な, に
  {
    description: 'na-adjective',
    score: 15,
    connector: ' '
  }
);

// Line 874-879: def-generic-synergy synergy-to-adverbs
defGenericSynergy('synergy-to-adverbs',
  filterIsPos(['adv-to'], (k, p, c, l) => k || l || p),
  filterInSeqSet(1008490), // と
  {
    description: 'to-adverb',
    score: (l, r) => 10 + 10 * (l.end - l.start),
    connector: ' '
  }
);

// Line 881-886: def-generic-synergy synergy-suffix-chu
defGenericSynergy('synergy-suffix-chu',
  filterIsNoun,
  filterInSeqSet(1620400, 2083570), // 中
  {
    description: 'suffix-chu',
    score: 12,
    connector: '-'
  }
);

// Line 888-893: def-generic-synergy synergy-suffix-tachi
defGenericSynergy('synergy-suffix-tachi',
  filterIsNoun,
  filterInSeqSet(1416220), // 達
  {
    description: 'suffix-tachi',
    score: 10,
    connector: '-'
  }
);

// Line 895-900: def-generic-synergy synergy-suffix-buri
defGenericSynergy('synergy-suffix-buri',
  filterIsNoun,
  filterInSeqSet(1361140), // 振り
  {
    description: 'suffix-buri',
    score: 40,
    connector: ''
  }
);

// Line 902-908: def-generic-synergy synergy-suffix-sei
defGenericSynergy('synergy-suffix-sei',
  filterIsNoun,
  filterInSeqSet(1375260), // 性
  {
    description: 'suffix-sei',
    score: 12,
    connector: ''
  }
);

// Line 910-915: def-generic-synergy synergy-o-prefix
defGenericSynergy('synergy-o-prefix',
  filterInSeqSet(1270190), // お
  filterIsPos(['n'], (k, p, c, l) => k || l),
  {
    description: 'o+noun',
    score: 10,
    connector: ''
  }
);

// Line 917-922: def-generic-synergy synergy-kanji-prefix
defGenericSynergy('synergy-kanji-prefix',
  filterInSeqSet(2242840, 1922780, 2423740), // 未, 不
  filterIsPos(['n'], (k, p, c, l) => k),
  {
    description: 'kanji prefix+noun',
    score: 15,
    connector: ''
  }
);

// Line 924-929: def-generic-synergy synergy-shicha-ikenai
defGenericSynergy('synergy-shicha-ikenai',
  filterIsCompoundEnd(2028920), // は
  filterInSeqSet(1000730, 1612750, 1409110, 2829697, 1587610), // いけない, いけません, だめ, いかん, いや
  {
    description: 'shicha ikenai',
    score: 50,
    connector: ' '
  }
);

// Line 931-939: def-generic-synergy synergy-shika-negative
defGenericSynergy('synergy-shika-negative',
  filterInSeqSet(1005460), // しか
  (segment: Segment) => {
    const info = segment.info || {};
    const conj = info.conj || [];
    return conj.some((cdata: ConjData) => cdata.prop.neg !== false);
  },
  {
    description: 'shika+neg',
    score: 50,
    connector: ' '
  }
);

// Line 941-946: def-generic-synergy synergy-no-toori
defGenericSynergy('synergy-no-toori',
  filterInSeqSet(1469800), // の
  filterInSeqSet(1432920), // 通り
  {
    description: 'no toori',
    score: 50,
    connector: ' '
  }
);

// Line 948-952: def-generic-synergy synergy-oki
defGenericSynergy('synergy-oki',
  filterIsPos(['ctr'], (k, p, c, l) => true),
  filterInSeqSet(2854117, 2084550), // 置き
  {
    score: 20,
    connector: ''
  } as any // description is optional
);

// Line 954-956: defun get-synergies
export async function getSynergies(segmentListLeft: SegmentList, segmentListRight: SegmentList): Promise<Array<[SegmentList, Synergy, SegmentList]>> {
  const results: Array<[SegmentList, Synergy, SegmentList]> = [];

  for (const fn of synergyList) {
    const result = await fn(segmentListLeft, segmentListRight);
    if (result) {
      results.push(...result);
    }
  }

  return results;
}

// ============================================================================
// PENALTIES SYSTEM (Lines 959-1014)
// Similar to synergies but reduces the score of two consequent segments
// ============================================================================

// Line 963-967: defmacro defpenalty
function defpenalty(name: string, fn: PenaltyFunction): void {
  if (!penaltyList.includes(fn)) {
    penaltyList.unshift(fn); // Add to beginning like Lisp's pushnew
  }
}

// Line 969-981: defmacro def-generic-penalty
function defGenericPenalty(
  name: string,
  filterLeft: (segment: SegmentList) => boolean,
  filterRight: (segment: SegmentList) => boolean,
  options: {
    description: string;
    connector?: string;
    score: number;
    serial?: boolean;
  }
): void {
  const { description, connector = ' ', score, serial = true } = options;

  const penaltyFn: PenaltyFunction = async (segmentListLeft, segmentListRight) => {
    const start = segmentListLeft.end;
    const end = segmentListRight.start;

    // Line 975: (when (and ,(if serial `(= ,start ,end) t) ...))
    if (serial && start !== end) {
      return null;
    }

    if (filterLeft(segmentListLeft) && filterRight(segmentListRight)) {
      return { start, end, description, connector, score };
    }

    return null;
  };

  defpenalty(name, penaltyFn);
}

// Line 983-991: defun filter-short-kana
function filterShortKana(len: number, options: { except?: string[] } = {}): (segment: SegmentList) => boolean {
  const { except } = options;

  return (segmentList: SegmentList) => {
    const seg = segmentList.segments[0];
    if (!seg) return false;

    // Line 988-990: (<= (- (segment-list-end) (segment-list-start)) len)
    const lengthOk = (segmentList.end - segmentList.start) <= len;
    if (!lengthOk) return false;

    // Line 990: (not (car (getf (segment-info seg) :kpcl)))
    const info = (seg as any).info || {};
    if (info.kpcl?.[0]) return false;

    // Line 991: (not (and except (member (get-text seg) except :test 'equal)))
    if (except && except.includes(getText(seg))) return false;

    return true;
  };
}

// Line 993-998: def-generic-penalty penalty-short
defGenericPenalty(
  'penalty-short',
  filterShortKana(1, {}),
  filterShortKana(1, { except: ['と'] }),
  {
    description: 'short',
    serial: false,
    score: -9,
  }
);

// Line 1000-1006: def-generic-penalty penalty-semi-final
defGenericPenalty(
  'penalty-semi-final',
  (sl: SegmentList) => {
    // Line 1002: (some (lambda (s) (funcall (apply 'filter-in-seq-set *semi-final-prt*) s)) ...)
    return sl.segments.some(s => filterInSeqSet(...SEMI_FINAL_PRT)(s));
  },
  (_: SegmentList) => true,
  {
    description: 'semi-final not final',
    score: -15,
  }
);

// Line 1008-1013: defun get-penalties
export async function getPenalties(segLeft: SegmentList, segRight: SegmentList): Promise<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]> {
  // Line 1009-1012: (loop for fn in *penalty-list* for penalty = (funcall fn seg-left seg-right) ...)
  for (const fn of penaltyList) {
    const penalty = await fn(segLeft, segRight);
    if (penalty) {
      // Line 1012: do (return (list seg-right penalty seg-left))
      return [segRight, penalty, segLeft];
    }
  }

  // Line 1013: finally (return (list seg-right seg-left))
  return [segRight, segLeft];
}

// ============================================================================
// SEGFILTERS SYSTEM (Lines 1016-1168)
// Used to ban certain combinations of subsequent words
// A function is called with arguments segment-list-left and segment-list-right
// and must return a list of possible segment-list-left/segment-list-right combinations
// segfilter must work even if segment-list-left is nil
// ============================================================================

// Line 1023-1027: defmacro defsegfilter
function defsegfilter(name: string, fn: SegfilterFunction): void {
  if (!segfilterList.includes(fn)) {
    segfilterList.push(fn);
  }
}

// Line 1029-1034: defun classify
function classify<T>(filter: (element: T) => boolean, list: T[]): [T[], T[]] {
  const yep: T[] = [];
  const nope: T[] = [];

  for (const element of list) {
    if (filter(element)) {
      yep.push(element);
    } else {
      nope.push(element);
    }
  }

  return [yep, nope];
}

// Line 1036-1066: defmacro def-segfilter-must-follow
// This segfilter is for when segments that satisfy filter-right MUST follow segments that satisfy filter-left
function defSegfilterMustFollow(
  name: string,
  filterLeft: (segment: Segment) => boolean,
  filterRight: (segment: Segment) => boolean,
  options: { allowFirst?: boolean } = {}
): void {
  const { allowFirst = false } = options;

  const segfilterFn: SegfilterFunction = (segmentListLeft, segmentListRight) => {
    // Line 1042-1043: (multiple-value-bind (,satisfies-right ,contradicts-right) (classify ...))
    const [satisfiesRight, contradictsRight] = classify(filterRight, segmentListRight.segments);

    // Line 1044-1046: ((or (not ,satisfies-right) (and ,allow-first (not ,segment-list-left))) ...)
    if (satisfiesRight.length === 0 || (allowFirst && !segmentListLeft)) {
      return [[segmentListLeft, segmentListRight]];
    }

    // Line 1047-1051: ((or (not ,segment-list-left) (/= ...)) ...)
    if (!segmentListLeft || segmentListLeft.end !== segmentListRight.start) {
      if (contradictsRight.length > 0) {
        return [[segmentListLeft, makeSegmentListFrom(segmentListRight, contradictsRight)]];
      }
      return [];
    }

    // Line 1052-1066: (t (multiple-value-bind (,satisfies-left ,contradicts-left) ...))
    const [satisfiesLeft, contradictsLeft] = classify(filterLeft, segmentListLeft.segments);

    if (contradictsLeft.length > 0) {
      const result: Array<[SegmentList | null, SegmentList]> = [];

      // Line 1056-1059: (let ((,result (when ,contradicts-right ...)))
      if (contradictsRight.length > 0) {
        result.push([segmentListLeft, makeSegmentListFrom(segmentListRight, contradictsRight)]);
      }

      // Line 1060-1064: (when ,satisfies-left (push ...))
      if (satisfiesLeft.length > 0) {
        result.push([
          makeSegmentListFrom(segmentListLeft, satisfiesLeft),
          makeSegmentListFrom(segmentListRight, satisfiesRight)
        ]);
      }

      return result;
    } else {
      // Line 1066: (list (list ,segment-list-left ,segment-list-right))
      return [[segmentListLeft, segmentListRight]];
    }
  };

  defsegfilter(name, segfilterFn);
}

// Line 1069-1072: defparameter *aux-verbs*
const AUX_VERBS = [
  1342560, // 初める/そめる
  // 2141080, // そうにない
];

// Line 1074-1076: def-segfilter-must-follow segfilter-aux-verb
defSegfilterMustFollow(
  'segfilter-aux-verb',
  filterIsConjugation(13),
  filterInSeqSet(...AUX_VERBS)
);

// Line 1078-1081: def-segfilter-must-follow segfilter-tsu-iru
defSegfilterMustFollow(
  'segfilter-tsu-iru',
  (segment: Segment) => !filterInSeqSet(2221640)(segment),
  filterInSeqSet(1577980),
  { allowFirst: true }
);

// Line 1083-1086: def-segfilter-must-follow segfilter-n
defSegfilterMustFollow(
  'segfilter-n',
  (segment: Segment) => !filterInSeqSetSimple(...NOUN_PARTICLES)(segment),
  filterInSeqSet(2139720, 2849370, 2849387), // ん んだ
  { allowFirst: true }
);

// Line 1088-1090: def-segfilter-must-follow segfilter-wokarasu
defSegfilterMustFollow(
  'segfilter-wokarasu',
  filterInSeqSet(2029010),
  filterInSeqSet(2087020)
);

// Line 1092-1094: def-segfilter-must-follow segfilter-badend
defSegfilterMustFollow(
  'segfilter-badend',
  (_: Segment) => false,
  filterIsCompoundEndText('ちゃい', 'いか', 'とか', 'とき', 'い')
);

// Line 1096-1102: def-segfilter-must-follow segfilter-sukiyoki
// Some of adj-ix words end with 好い which produces a confusing 好き conjugation; this should disable it
defSegfilterMustFollow(
  'segfilter-sukiyoki',
  (_: Segment) => false,
  (segment: Segment) => {
    return filterIsConjugation(CONJ_ADJECTIVE_LITERARY)(segment) &&
           getText(segment).endsWith('好き');
  }
);

// Line 1104-1107: Commented out segfilter-itsu
// (def-segfilter-must-follow segfilter-itsu (l r)
//   (complement (filter-is-compound-end-text "い"))
//   (filter-in-seq-set 2221640 1013250)
//   :allow-first t)

// Line 1109-1112: def-segfilter-must-follow segfilter-roku
defSegfilterMustFollow(
  'segfilter-roku',
  (segment: Segment) => !filterIsCompoundEndText('いろ')(segment),
  (segment: Segment) => getText(segment).startsWith('く'),
  { allowFirst: true }
);

// Line 1114-1117: def-segfilter-must-follow segfilter-sae
defSegfilterMustFollow(
  'segfilter-sae',
  (segment: Segment) => !filterIsCompoundEnd(2029120)(segment),
  (segment: Segment) => getText(segment).startsWith('え'),
  { allowFirst: true }
);

// Line 1119-1122: def-segfilter-must-follow segfilter-janai
defSegfilterMustFollow(
  'segfilter-janai',
  (segment: Segment) => !filterIsCompoundEnd(2028920)(segment),
  filterInSeqSet(1529520, 1296400, 2139720),
  { allowFirst: true }
);

// Line 1124-1127: def-segfilter-must-follow segfilter-nohayamete
defSegfilterMustFollow(
  'segfilter-nohayamete',
  (segment: Segment) => !filterInSeqSet(1469800)(segment),
  filterInSeqSet(1601080),
  { allowFirst: true }
);

// Line 1129-1133: def-segfilter-must-follow segfilter-toomou
// Split と before 思う 言う
defSegfilterMustFollow(
  'segfilter-toomou',
  (segment: Segment) => !filterInSeqSet(2837117)(segment), // 何だと
  filterInSeqSet(1589350, 1587040),
  { allowFirst: true }
);

// Line 1135-1138: def-segfilter-must-follow segfilter-totte
defSegfilterMustFollow(
  'segfilter-totte',
  (segment: Segment) => !filterInSeqSet(1008490)(segment),
  filterInSeqSet(2086960),
  { allowFirst: true }
);

// Line 1140-1145: def-segfilter-must-follow segfilter-dashi
defSegfilterMustFollow(
  'segfilter-dashi',
  (segment: Segment) => {
    const info = (segment as any).info || {};
    const seqSet: number[] = info.seqSet || [];
    // Line 1142-1143: (or (not (find 2089020 seq-set)) (find 2028980 seq-set))
    return !seqSet.includes(2089020) || seqSet.includes(2028980); // だ or で
  },
  filterInSeqSet(1157170, 2424740, 1305070), // する して
  { allowFirst: true }
);

// Line 1147-1151: def-segfilter-must-follow segfilter-dekiru
// 出 followed by 来る or 来てる
defSegfilterMustFollow(
  'segfilter-dekiru',
  (segment: Segment) => !filterInSeqSet(1896380, 2422860)(segment),
  filterInSeqSet(2830009, 1547720),
  { allowFirst: true }
);

// Line 1153-1156: defparameter *honorifics*
const HONORIFICS = [
  1247260, // 君
];

// Line 1157-1159: def-segfilter-must-follow segfilter-honorific
defSegfilterMustFollow(
  'segfilter-honorific',
  (segment: Segment) => !filterInSeqSetSimple(...NOUN_PARTICLES)(segment),
  filterInSeqSet(...HONORIFICS)
);

// Line 1162-1168: defun apply-segfilters
export function applySegfilters(segLeft: SegmentList | null, segRight: SegmentList): Array<[SegmentList | null, SegmentList]> {
  // Line 1163: (loop with splits = (list (list seg-left seg-right)) ...)
  let splits: Array<[SegmentList | null, SegmentList]> = [[segLeft, segRight]];

  // Line 1164-1167: for segfilter in *segfilter-list* do (setf splits ...)
  for (const segfilter of segfilterList) {
    const newSplits: Array<[SegmentList | null, SegmentList]> = [];
    for (const [left, right] of splits) {
      newSplits.push(...segfilter(left, right));
    }
    splits = newSplits;
  }

  // Line 1168: finally (return splits)
  return splits;
}
