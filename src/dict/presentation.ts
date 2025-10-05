// ichiran/dict/presentation - Presentation layer for dictionary output
// Extracted from dict.lisp lines 1243-1934
// This module handles:
// - WordInfo class and conversion functions
// - High-level segmentation functions (dictSegment, simpleSegment)
// - JSON output types and functions
// - String formatting for readings, senses, and conjugations

import { getConnection } from '../conn.js';
import { getCharClass, asHiragana, testWord } from '../characters.js';
import { getSuffixDescription } from '../grammar/suffixCache.js';
import { getConjDescription } from './conj-description.js';
import type {
  KanjiText,
  KanaText,
  SimpleText,
  Reading,
  ConjProp,
  Conjugation,
  Segment,
  SegmentList
} from '../types.js';
import { getKana, getKanji } from './readings.js';
import { findWordFull, findWordsSeqs } from './lookup.js';
import { getConjData, getOriginalTextOnce } from './conjugation.js';
import { genScore } from './scoring.js';
import {
  joinSubstringWords2,
  findBestPath,
  fillSegmentPath,
  wordInfoFromSegmentList
} from './segmentation.js';

// =============================================================================
// WordInfo Class (Lines 1243-1297 in dict.lisp)
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

// =============================================================================
// High-level Segmentation Functions (Lines 1380-1454 in dict.lisp)
// =============================================================================

// Line 1380-1386: defun word-info-from-text
export async function wordInfoFromText(text: string): Promise<WordInfo> {
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
}

// Line 1407-1413: defun word-info-rec-find
export function wordInfoRecFind(wiList: WordInfo[], testFn: (wi: WordInfo) => boolean): Array<[WordInfo, WordInfo | undefined]> {
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
// JSON Output Types (Lines 1456-1934 in dict.lisp)
// =============================================================================

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

// =============================================================================
// Sense and Gloss Functions (Lines 1456-1502 in dict.lisp)
// =============================================================================

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

// =============================================================================
// Conjugation Helper Functions (Lines 1601-1644 in dict.lisp)
// =============================================================================

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

// =============================================================================
// String Output Functions (Lines 1743-1778 in dict.lisp)
// =============================================================================

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

// =============================================================================
// Reading String Functions (Lines 1505-1735 in dict.lisp)
// =============================================================================

// Helper to get word type for readings
function wordType(reading: Reading): 'kanji' | 'kana' {
  // Check the actual text content, not the nokanji flag
  // nokanji=false means the word HAS a kanji form available, but reading.text may still be kana
  // Note: Reading (KanjiText | KanaText) always has text property
  return testWord(reading.text, 'kana') ? 'kana' : 'kanji';
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

  // Reading (KanjiText | KanaText) always has text property
  const readingText = reading.text;

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
export function mapWordInfoKana(fn: (s: string) => string, wordInfo: WordInfo, separator = '/'): string {
  const wkana = wordInfo.kana;

  if (Array.isArray(wkana)) {
    return simplifyReadingList(wkana.map(fn)).join(separator);
  } else {
    return fn(wkana);
  }
}

// =============================================================================
// JSON Conversion Functions (Lines 285-295, 1662-1830 in dict.lisp)
// =============================================================================

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
      components.map((wi: any, _idx: number) => {
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
