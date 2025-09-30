// ichiran/dict-counters - Counter word handling
// Port of dict-counters.lisp (765 lines)

import type { KanjiText, KanaText } from './types.js';
import { parseNumber, numberToKana, numberToKanji } from './numbers.js';
import { geminate, rendaku, testWord, countCharClass } from './characters.js';
import { getConnection, defineCache } from './conn.js';
import { KANA_HINT_SPACE } from './dict-split.js';

// ============================================================================
// Lines 3-7: counter-join generic function
// ============================================================================

// Base counter-join function (default implementation)
// Line 3: defgeneric counter-join
export function counterJoinBase(
  counter: CounterText,
  n: number,
  numberKana: string,
  counterKana: string
): string {
  // Line 6: (concatenate 'string number-kana counter-kana)
  return numberKana + counterKana;
}

// ============================================================================
// Lines 9-29: counter-text class
// ============================================================================

// Line 9: defclass counter-text
export class CounterText {
  text: string;
  kana: string;
  numberText: string;
  numberValue: number;
  source: (KanjiText | KanaText) | null;
  ordinalp: boolean;
  suffix: string | null;
  acceptsSuffixes: string[] | null;
  suffixDescriptions: string[];
  digitOpts: Array<[number | string, ...any[]]> | null;
  common: number | null;
  allowed: number[] | null;
  foreign: boolean;

  constructor(options: {
    text: string;
    kana: string;
    numberText: string;
    source?: (KanjiText | KanaText) | null;
    ordinalp?: boolean;
    suffix?: string | null;
    accepts?: string[] | null;
    suffixDescriptions?: string[];
    digitOpts?: Array<[number | string, ...any[]]> | null;
    common?: number | null;
    allowed?: number[] | null;
    foreign?: boolean;
  }) {
    this.text = options.text;
    this.kana = options.kana;
    this.numberText = options.numberText;
    this.source = options.source ?? null;
    this.ordinalp = options.ordinalp ?? false;
    this.suffix = options.suffix ?? null;
    this.acceptsSuffixes = options.accepts ?? null;
    this.suffixDescriptions = options.suffixDescriptions ?? [];
    this.digitOpts = options.digitOpts ?? null;
    this.common = options.common ?? null;
    this.allowed = options.allowed ?? null;
    this.foreign = options.foreign ?? false;

    // Line 52: initialize-instance :after
    this.numberValue = parseNumber(this.numberText);
  }

  // Line 31-36: verify method
  verify(unique: boolean): boolean {
    return (
      (!this.allowed || this.allowed.includes(this.numberValue)) &&
      unique
    );
  }

  // Line 58-59: text method
  getText(): string {
    return this.numberText + this.text;
  }

  // Line 61-62: get-kanji method
  getKanji(): string {
    return numberToKanji(this.numberValue) + this.text;
  }

  // Line 64-67: get-kana method (base)
  getKanaBase(): string {
    const n = this.numberValue;
    return this.counterJoin(
      n,
      numberToKana(n, { separator: KANA_HINT_SPACE }) as string,
      this.kana // Strings are immutable in JS, no need to copy
    );
  }

  // Line 69-71: get-kana :around method
  getKana(): string {
    const kana = this.getKanaBase();
    return this.suffix ? kana + this.suffix : kana;
  }

  // Line 73-74: word-type method
  wordType(): 'kanji' | 'kana' {
    return countCharClass(this.getText(), 'kanji-char') > 0 ? 'kanji' : 'kana';
  }

  // Line 76-77: common method
  getCommon(): number | null {
    if (this.common !== null && this.common !== undefined) return this.common;
    if (this.source) {
      const sourceCommon = (this.source as any).common;
      if (sourceCommon !== null && sourceCommon !== undefined) return sourceCommon;
    }
    return null;
  }

  // Line 79-80: seq method
  getSeq(): number | null {
    return this.source ? this.source.seq : null;
  }

  // Line 82-83: ord method
  getOrd(): number {
    return this.source ? this.source.ord : 0;
  }

  // Line 85-92: Various word methods
  wordConjugations(): null {
    return null;
  }

  wordConjData(): null {
    return null;
  }

  nokanji(): boolean {
    return this.source ? (this.source as any).nokanji ?? false : false;
  }

  rootP(): boolean {
    return true;
  }

  // Line 44-49: value-string method
  valueString(): string {
    const value = this.numberValue;
    const ordStr = this.ordinalp ? ordinalStr(value) : String(value);
    const descs = this.suffixDescriptions.length > 0
      ? ' ' + this.suffixDescriptions.slice().reverse().join(' ')
      : '';
    return `Value: ${ordStr}${descs}`;
  }

  // Line 101-201: counter-join method (complex phonetic rules)
  counterJoin(n: number, numberKana: string, counterKana: string): string {
    const digit = getDigit(n);
    const head = CHAR_CLASS_HASH.get(counterKana[0]);
    const digitOpts = this.digitOpts?.find(([d]) => d === digit);
    const off = this.digitOpts?.find(([d]) => d === ':off');

    // Line 106-123: digit-opts processing
    if (off || digitOpts) {
      let modCounter = false;
      for (const opt of (digitOpts ? digitOpts.slice(1) : [])) {
        if (typeof opt === 'string') {
          // Line 110-116: String options replace parts
          if (modCounter) {
            counterKana = opt;
          } else {
            const stem = digit < 10
              ? (DIGIT_TO_KANA as any)[digit]?.length ?? 0
              : (POWER_TO_KANA as any)[Math.round(Math.log10(digit))]?.length ?? 0;
            numberKana = numberKana.slice(0, numberKana.length - stem) + opt;
          }
        } else {
          // Line 117-122: Symbol options
          switch (opt) {
            case ':g':
              numberKana = geminate(numberKana);
              break;
            case ':r':
              counterKana = rendaku(counterKana);
              break;
            case ':h':
              counterKana = rendaku(counterKana, false, true); // handakuten
              break;
            case ':c':
              modCounter = true;
              break;
          }
        }
      }
      return counterJoinBase(this, n, numberKana, counterKana);
    }

    // Line 125-146: Foreign counter rules
    if (this.foreign) {
      switch (digit) {
        case 6:
          if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
            numberKana = geminate(numberKana);
          }
          break;
        case 8:
          if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'sa', 'shi', 'su', 'se', 'so',
                       'ta', 'chi', 'tsu', 'te', 'to', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
            numberKana = geminate(numberKana);
          }
          break;
        case 10:
          if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'sa', 'shi', 'su', 'se', 'so',
                       'ta', 'chi', 'tsu', 'te', 'to', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
            numberKana = geminate(numberKana);
          }
          break;
        case 100:
          if (head && ['ka', 'ki', 'ku', 'ke', 'ko'].includes(head)) {
            numberKana = geminate(numberKana);
          }
          break;
      }
      return counterJoinBase(this, n, numberKana, counterKana);
    }

    // Line 148-200: Standard counter phonetic rules
    switch (digit) {
      case 1:
        if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'sa', 'shi', 'su', 'se', 'so',
                     'ta', 'chi', 'tsu', 'te', 'to'].includes(head)) {
          numberKana = geminate(numberKana);
        }
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          numberKana = geminate(numberKana);
          counterKana = rendaku(counterKana, false, true); // handakuten
        }
        break;
      case 3:
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      // case 4: // Line 160-162: Commented out in Lisp
      case 6:
        if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
          numberKana = geminate(numberKana);
        }
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          numberKana = geminate(numberKana);
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      case 8:
        if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'sa', 'shi', 'su', 'se', 'so',
                     'ta', 'chi', 'tsu', 'te', 'to', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
          numberKana = geminate(numberKana);
        }
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          numberKana = geminate(numberKana);
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      case 10:
        if (head && ['ka', 'ki', 'ku', 'ke', 'ko', 'sa', 'shi', 'su', 'se', 'so',
                     'ta', 'chi', 'tsu', 'te', 'to', 'pa', 'pi', 'pu', 'pe', 'po'].includes(head)) {
          numberKana = geminate(numberKana);
        }
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          numberKana = geminate(numberKana);
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      case 100:
        if (head && ['ka', 'ki', 'ku', 'ke', 'ko'].includes(head)) {
          numberKana = geminate(numberKana);
        }
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          numberKana = geminate(numberKana);
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      case 1000:
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          counterKana = rendaku(counterKana, false, true);
        }
        break;
      case 10000:
        if (head && ['ha', 'hi', 'fu', 'he', 'ho'].includes(head)) {
          counterKana = rendaku(counterKana, false, true);
        }
        break;
    }

    return counterJoinBase(this, n, numberKana, counterKana);
  }

  // Line 54-56: print-object method (toString for debugging)
  toString(): string {
    return `CounterText(${this.numberText}[${this.numberValue}] ${this.text})`;
  }
}

// Line 38-42: ordinal-str helper function
function ordinalStr(n: number): string {
  const digit = n % 10;
  const teenp = 10 < (n % 100) && (n % 100) < 20;
  const suffix = teenp ? 'th' : (digit === 1 ? 'st' : digit === 2 ? 'nd' : digit === 3 ? 'rd' : 'th');
  return `${n}${suffix}`;
}

// Line 94-99: get-digit helper function
function getDigit(n: number): number {
  const digit = n % 10;
  if (digit === 0) {
    // Line 97-98: Check powers of 10
    for (const [p, pn] of [[10, 100], [100, 1000], [1000, 10000], [10000, 100000000]] as const) {
      if (pn && n % pn !== 0) {
        return p;
      }
    }
  }
  return digit;
}

// ============================================================================
// Lines 203-210: number-text class
// ============================================================================

// Line 203: defclass number-text
export class NumberText extends CounterText {
  constructor(options: any) {
    super({
      text: '',
      kana: '',
      numberText: options.numberText || '',
      ordinalp: false,
      ...options
    });
  }

  // Line 208-209: get-kana override
  getKanaBase(): string {
    return numberToKana(this.numberValue, { separator: KANA_HINT_SPACE }) as string;
  }
}

// Import CHAR_CLASS_HASH from characters.ts
import { CHAR_CLASS_HASH } from './characters.js';

// Import digit/power maps from numbers.ts (need to check if exported)
const DIGIT_TO_KANA: Record<number, string> = {
  1: 'いち', 2: 'に', 3: 'さん', 4: 'よん', 5: 'ご',
  6: 'ろく', 7: 'なな', 8: 'はち', 9: 'きゅう'
};

const POWER_TO_KANA: Record<number, string> = {
  1: 'じゅう',
  2: 'ひゃく',
  3: 'せん',
  4: 'まん'
};

// ============================================================================
// Lines 211-220: Counter configuration
// ============================================================================

// Line 211: defparameter *special-counters*
export const SPECIAL_COUNTERS = new Map<number, (readings: Array<KanjiText | KanaText>) => CounterArgs[]>();

// Line 213-215: defparameter *counter-suffixes*
export const COUNTER_SUFFIXES: Array<[string, string, string, string]> = [
  ['kan', '間', 'かん', '[duration]'],
  ['kango', '間後', 'かんご', '[after ...]'],
  ['chuu', '中', 'ちゅう', '[among/out of ...]']
];

// Line 217-218: defparameter *counter-accepts*
export const COUNTER_ACCEPTS: Array<[number, ...string[]]> = [
  [1194480, 'kan'],
  [1490430, 'kan'],
  [1333450, 'kan', 'kango']
];

// Line 219: defparameter *counter-foreign*
export const COUNTER_FOREIGN = [1120410];

// Type for counter arguments
type CounterArgs = [string | string[], typeof CounterText | any, ...any[]];

// ============================================================================
// Lines 283-330: Database query functions
// ============================================================================

// Line 283-289: get-counter-ids
async function getCounterIds(): Promise<number[]> {
  const sql = getConnection();
  const rows = await sql<{ seq: number }[]>`
    SELECT DISTINCT seq
    FROM sense_prop
    WHERE tag = 'pos' AND text = 'ctr'
    ORDER BY seq
  `;
  return rows.map(r => r.seq);
}

// Line 310-330: Counter ID configuration
const EXTRA_COUNTER_IDS = [
  1255430, // 月
  1606800  // 割
];

const SKIP_COUNTER_IDS = [
  2426510, // 一個当り
  2220370, // 歳 （とせ）
  2248360, // 入 （しお）
  2423450, // 差し
  2671670, // 幅 （の）
  2735690, // 種 （くさ）
  2838543, // 杯 （はた）
  // mahjong stuff - need some research on how to say these
  2249290, // 荘
  2833260, // 翻
  2833465, // 萬
  2833466, // 索
  2833467  // 筒
];

// Line 291-308: get-counter-stags
async function getCounterStags(seqs: number[]): Promise<[Map<number, string[]>, Map<number, string[]>]> {
  const sql = getConnection();
  const stagks = new Map<number, string[]>();
  const stagrs = new Map<number, string[]>();

  const queryTag = async (tag: string) => {
    return await sql<{ seq: number; text: string }[]>`
      SELECT sp.seq, sp.text
      FROM sense_prop sp, sense_prop sp1
      WHERE sp.seq = sp1.seq
        AND sp.sense_id = sp1.sense_id
        AND sp.tag = ${tag}
        AND sp1.tag = 'pos'
        AND sp1.text = 'ctr'
        AND sp.seq IN ${sql(seqs)}
    `;
  };

  // Query stagk
  for (const { seq, text } of await queryTag('stagk')) {
    if (!stagks.has(seq)) stagks.set(seq, []);
    stagks.get(seq)!.push(text);
  }

  // Query stagr
  for (const { seq, text } of await queryTag('stagr')) {
    if (!stagrs.has(seq)) stagrs.set(seq, []);
    stagrs.get(seq)!.push(text);
  }

  return [stagks, stagrs];
}

// Line 332-358: get-counter-readings
async function getCounterReadings(): Promise<Map<number, [(KanjiText | KanaText)[], (KanjiText | KanaText)[]]>> {
  const sql = getConnection();
  const hash = new Map<number, [(KanjiText | KanaText)[], (KanjiText | KanaText)[]]>();

  // Line 335-337: Combine counter IDs
  const baseIds = await getCounterIds();
  const allIds = [...baseIds, ...EXTRA_COUNTER_IDS];
  const counterIds = allIds.filter(id => !SKIP_COUNTER_IDS.includes(id));

  // Line 338: Get stags
  const [stagksMap, stagrsMap] = await getCounterStags(counterIds);

  // Line 339-340: Get readings from database
  const kanjiReadings = await sql<KanjiText[]>`
    SELECT * FROM kanji_text WHERE seq IN ${sql(counterIds)} ORDER BY seq, ord
  `;
  const kanaReadings = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE seq IN ${sql(counterIds)} ORDER BY seq, ord
  `;

  // Line 341-346: Process kanji readings
  for (const r of kanjiReadings) {
    const stagks = stagksMap.get(r.seq);
    if (!stagks || stagks.includes(r.text)) {
      if (!hash.has(r.seq)) {
        hash.set(r.seq, [[], []]);
      }
      hash.get(r.seq)![0].push(r);
    }
  }

  // Line 347-352: Process kana readings
  for (const r of kanaReadings) {
    const stagrs = stagrsMap.get(r.seq);
    if (!stagrs || stagrs.includes(r.text)) {
      if (!hash.has(r.seq)) {
        hash.set(r.seq, [[], []]);
      }
      hash.get(r.seq)![1].push(r);
    }
  }

  // Line 353-357: Sort by ord
  for (const [key, value] of hash) {
    hash.set(key, [
      value[0].sort((a, b) => a.ord - b.ord),
      value[1].sort((a, b) => a.ord - b.ord)
    ]);
  }

  return hash;
}

// ============================================================================
// Lines 221-271: Counter cache system
// ============================================================================

// Line 221: defcache :counters *counter-cache*
export const ensureCounterCache = defineCache('counters', async () => {
  const counterCache = new Map<string, Array<[typeof CounterText | any, any]>>();

  // Helper function to add args - Line 223-232
  function addArgs_(text: string, args: [typeof CounterText | any, any]) {
    if (!counterCache.has(text)) {
      counterCache.set(text, []);
    }
    counterCache.get(text)!.push(args);

    // Line 225-232: Add suffix variants
    const [cls, options] = args;
    const accepts = options.accepts as string[] | undefined;
    if (accepts) {
      for (const suf of accepts) {
        const suffixInfo = COUNTER_SUFFIXES.find(([name]) => name === suf);
        if (suffixInfo) {
          const [, sufText, sufKana, sufDesc] = suffixInfo;
          const newArgs: [any, any] = [cls, { ...options }];
          const newText = text + sufText;
          newArgs[1].text = newText;
          newArgs[1].suffix = (options.suffix || '') + sufKana;
          newArgs[1].suffixDescriptions = [...(options.suffixDescriptions || []), sufDesc];
          if (!counterCache.has(newText)) {
            counterCache.set(newText, []);
          }
          counterCache.get(newText)!.push(newArgs);
        }
      }
    }
  }

  // Line 233-240: addArgs wrapper for single/multiple texts
  function addArgs(text: string | string[], cls: typeof CounterText | any, options: any = {}) {
    if (Array.isArray(text)) {
      for (const txt of text) {
        const newOptions = { ...options, text: txt };
        if (typeof options.source === 'function') {
          newOptions.source = options.source(txt);
        }
        addArgs_(txt, [cls, newOptions]);
      }
    } else {
      addArgs_(text, [cls, options]);
    }
  }

  // Line 241: Add empty string for plain numbers
  addArgs('', NumberText, {});

  // Line 242-256: Process counter readings from database
  const readings = await getCounterReadings();
  for (const [seq, [kanji, kana]] of readings) {
    const special = SPECIAL_COUNTERS.get(seq);
    if (special) {
      // Line 246: Special counter handling
      const allReadings = [...kanji, ...kana];
      const argsList = special(allReadings);
      for (const args of argsList) {
        if (args.length >= 3) {
          addArgs(args[0], args[1], args[2]);
        }
      }
    } else {
      // Line 247-256: Standard counter handling
      const foreign = !kanji.length || COUNTER_FOREIGN.includes(seq);
      const textsToAdd = foreign
        ? [...kanji, ...kana.filter(x => testWord(x.text, 'katakana'))]
        : kanji;

      for (const kt of textsToAdd) {
        const text = kt.text;
        const ordinalp = text.length > 1 && text.endsWith('目');
        const accepts = COUNTER_ACCEPTS.find(([s]) => s === seq)?.slice(1) as string[] | undefined;
        addArgs(text, CounterText, {
          text,
          kana: kana[0]?.text || '',
          source: kt,
          ordinalp,
          accepts,
          foreign
        });
      }
    }
  }

  // Line 257-269: Add ordinal め suffix for all counters
  for (const [counter, argsList] of counterCache) {
    if (counter === '' || (counter.length > 1 && counter.endsWith('目'))) {
      continue;
    }
    const cord = counter + '目';
    if (counterCache.has(cord)) {
      continue;
    }
    for (const oldArgs of argsList) {
      const [cls, options] = oldArgs;
      if (!options.ordinalp) {
        const newOptions = { ...options };
        newOptions.text = cord;
        newOptions.suffix = (options.suffix || '') + 'め';
        newOptions.ordinalp = true;
        addArgs(cord, cls, newOptions);
      }
    }
  }

  return counterCache;
});

// ============================================================================
// Lines 273-281: findCounter main function
// ============================================================================

// Line 273: defun find-counter
export async function findCounter(
  number: string,
  counter: string,
  options: { unique?: boolean } = {}
): Promise<CounterText[]> {
  const unique = options.unique ?? true;
  const cache = await ensureCounterCache();
  const counterArgs = cache.get(counter);

  if (!counterArgs) {
    return [];
  }

  const results: CounterText[] = [];
  for (const [cls, opts] of counterArgs) {
    try {
      // Line 277-279: Create instance with number-text
      const instance = new cls({ ...opts, numberText: number });
      // Line 280: Verify and collect
      if (instance.verify(unique)) {
        results.push(instance);
      }
    } catch (e) {
      // Line 278: handler-case for not-a-number
      // Skip invalid numbers (parseNumber throws)
      continue;
    }
  }

  return results;
}

// ============================================================================
// Lines 360-381: defSpecialCounter macro and helpers
// ============================================================================

// Line 360: def-special-counter macro replacement
type SpecialCounterFn = (readings: Array<KanjiText | KanaText>) => CounterArgs[];

function defSpecialCounter(seq: number, fn: SpecialCounterFn) {
  SPECIAL_COUNTERS.set(seq, fn);
}

// Helper function: args() - creates standard counter args
// Line 365-371
function args(
  cls: typeof CounterText | any,
  text: string | string[],
  kana: string,
  options: any = {}
): CounterArgs {
  return [
    text,
    cls,
    {
      text: Array.isArray(text) ? text[0] : text,
      kana,
      source: (txt: string, readings: Array<KanjiText | KanaText>) => {
        return readings.find(r => r.text === txt) || null;
      },
      ...options
    }
  ];
}

// Helper function: args-suffix() - creates counter args with suffix structure
// Line 372-378
function argsSuffix(
  cls: typeof CounterText | any,
  text: [string, string],
  kana: [string, string],
  options: any = {}
): CounterArgs {
  const fullText = text[0] + text[1];
  return [
    fullText,
    cls,
    {
      text: fullText,
      kana: kana[0],
      suffix: kana[1],
      source: (txt: string, readings: Array<KanjiText | KanaText>) => {
        return readings.find(r => r.text === text[0]) || null;
      },
      ...options
    }
  ];
}

// Closure wrapper to capture readings in args/argsSuffix
function makeSpecialCounter(fn: (helpers: any) => CounterArgs[]): SpecialCounterFn {
  return (readings: Array<KanjiText | KanaText>) => {
    const helpers = {
      args: (cls: any, text: string | string[], kana: string, options: any = {}) => {
        const result = args(cls, text, kana, options);
        if (typeof result[2].source === 'function') {
          result[2].source = result[2].source(Array.isArray(text) ? text[0] : text, readings);
        }
        return result;
      },
      argsSuffix: (cls: any, text: [string, string], kana: [string, string], options: any = {}) => {
        const result = argsSuffix(cls, text, kana, options);
        if (typeof result[2].source === 'function') {
          result[2].source = result[2].source(text[0], readings);
        }
        return result;
      }
    };
    return fn(helpers);
  };
}

// ============================================================================
// Lines 391-765: Special counter class definitions and registrations
// ============================================================================

// Line 391-397: counter-halfhour class
class CounterHalfhour extends CounterText {
  // Line 393-394: value-string override
  valueString(): string {
    return `${this.numberValue}:30`;
  }
}

// Line 497-513: counter-tsu class
class CounterTsu extends CounterText {
  // Line 499-500: verify override
  verify(unique: boolean): boolean {
    return 1 <= this.numberValue && this.numberValue <= 9 && unique;
  }

  // Line 502-513: get-kana override
  getKanaBase(): string {
    switch (this.numberValue) {
      case 1: return 'ひとつ';
      case 2: return 'ふたつ';
      case 3: return 'みっつ';
      case 4: return 'よっつ';
      case 5: return 'いつつ';
      case 6: return 'むっつ';
      case 7: return 'ななつ';
      case 8: return 'やっつ';
      case 9: return 'ここのつ';
      default: return super.getKanaBase();
    }
  }
}

// Line 518-538: counter-hifumi class
class CounterHifumi extends CounterText {
  digitSet: number[];

  constructor(options: any) {
    super(options);
    this.digitSet = options.digitSet || [];
  }

  // Line 521-538: get-kana override
  getKanaBase(): string {
    const value = this.numberValue;
    if (this.digitSet.includes(value)) {
      let prefix = '';
      switch (value) {
        case 1: prefix = 'ひと'; break;
        case 2: prefix = 'ふた'; break;
        case 3: prefix = 'み'; break;
        case 4: prefix = 'よ'; break;
        case 5: prefix = 'いつ'; break;
        case 6: prefix = 'む'; break;
        case 7: prefix = 'なな'; break;
        case 8: prefix = 'や'; break;
        case 9: prefix = 'ここの'; break;
        case 10: prefix = 'と'; break;
      }
      return prefix + this.kana;
    }
    return super.getKanaBase();
  }
}

// Line 686-704: counter-days-kun class
class CounterDaysKun extends CounterText {
  constructor(options: any) {
    super({ ...options, allowed: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 20, 24, 30] });
  }

  // Line 689-704: get-kana override
  getKanaBase(): string {
    switch (this.numberValue) {
      case 1: return 'ついたち';
      case 2: return 'ふつか';
      case 3: return 'みっか';
      case 4: return 'よっか';
      case 5: return 'いつか';
      case 6: return 'むいか';
      case 7: return 'なのか';
      case 8: return 'ようか';
      case 9: return 'ここのか';
      case 10: return 'とうか';
      case 14: return 'じゅうよっか';
      case 20: return 'はつか';
      case 24: return 'にじゅうよっか';
      case 30: return 'みそか';
      default: return super.getKanaBase();
    }
  }
}

// Line 709-716: counter-days-on class
class CounterDaysOn extends CounterText {
  // Line 711-716: verify override
  verify(unique: boolean): boolean {
    const n = this.numberValue;
    return (n > 10 || n === 1) && n !== 20 && super.verify(unique);
  }
}

// Line 721-730: counter-months class
class CounterMonths extends CounterText {
  constructor(options: any) {
    super({
      ...options,
      allowed: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
      digitOpts: [[4, 'し'], [7, 'しち'], [9, 'く']]
    });
  }

  // Line 725-730: value-string override
  valueString(): string {
    const months = [
      'January', 'February', 'March', 'April', 'May', 'June',
      'July', 'August', 'September', 'October', 'November', 'December'
    ];
    return months[this.numberValue - 1];
  }
}

// Line 735-741: counter-people class
class CounterPeople extends CounterText {
  // Line 737-741: get-kana override
  getKanaBase(): string {
    switch (this.numberValue) {
      case 1: return 'ひとり';
      case 2: return 'ふたり';
      default: return super.getKanaBase();
    }
  }
}

// Line 746-749: counter-wari class
class CounterWari extends CounterText {
  // Line 748-749: value-string override
  valueString(): string {
    return `${this.numberValue * 10}%`;
  }
}

// Line 757-762: counter-age class
class CounterAge extends CounterText {
  // Line 759-762: get-kana override
  getKanaBase(): string {
    return this.numberValue === 20 ? 'はたち' : super.getKanaBase();
  }
}

// ============================================================================
// Lines 382-765: All special counter definitions
// ============================================================================

// Line 382-383
defSpecialCounter(1203020, makeSpecialCounter(({ args }) => [
  args(CounterText, '階', 'かい', { digitOpts: [[3, ':r']] })
]));

// Line 385-386
defSpecialCounter(2020680, makeSpecialCounter(({ args }) => [
  args(CounterText, '時', 'じ', { digitOpts: [[4, 'よ'], [7, 'しち'], [9, 'く']] })
]));

// Line 388-389
defSpecialCounter(1315920, makeSpecialCounter(({ args }) => [
  args(CounterText, '時間', 'じかん', { digitOpts: [[4, 'よ'], [9, 'く']] })
]));

// Line 396-397
defSpecialCounter(1658480, makeSpecialCounter(({ args }) => [
  args(CounterHalfhour, '時半', 'じはん', { digitOpts: [[4, 'よ'], [9, 'く']] })
]));

// Line 399-400
defSpecialCounter(1356740, makeSpecialCounter(({ args }) => [
  args(CounterText, '畳', 'じょう', { digitOpts: [[4, 'よ'], [7, 'しち']] })
]));

// Line 402-403
defSpecialCounter(2258110, makeSpecialCounter(({ args }) => [
  args(CounterText, '帖', 'じょう', { digitOpts: [[4, 'よ'], [7, 'しち']] })
]));

// Line 405-406
defSpecialCounter(1396490, makeSpecialCounter(({ args }) => [
  args(CounterText, '膳', 'ぜん', { digitOpts: [[4, 'よ'], [7, 'しち']] })
]));

// Line 408-410
defSpecialCounter(1427240, makeSpecialCounter(({ args, argsSuffix }) => [
  args(CounterText, '丁', 'ちょう', {}),
  argsSuffix(CounterText, ['丁', '目'], ['ちょう', 'め'], { ordinalp: true })
]));

// Line 412-413
defSpecialCounter(1427420, makeSpecialCounter(({ args }) => [
  args(CounterText, '丁目', 'ちょうめ', { ordinalp: true })
]));

// Line 415-416
defSpecialCounter(1514050, makeSpecialCounter(({ args }) => [
  args(CounterText, '舗', 'ほ', { digitOpts: [[4, ':h']] })
]));

// Line 418-419
defSpecialCounter(1522150, makeSpecialCounter(({ args }) => [
  args(CounterText, '本', 'ほん', { digitOpts: [[3, ':r']] })
]));

// Line 421-422
defSpecialCounter(1583370, makeSpecialCounter(({ args }) => [
  args(CounterText, ['匹', '疋'], 'ひき', { digitOpts: [[3, ':r']] })
]));

// Line 424-426
defSpecialCounter(1607310, makeSpecialCounter(({ args }) => [
  args(CounterText, '羽', 'わ', {
    digitOpts: [[3, ':c', 'ば'], [6, ':g', ':c', 'ぱ'], [10, ':g', ':c', 'ぱ'],
                 [100, ':g', ':c', 'ぱ'], [1000, ':c', 'ば'], [10000, ':c', 'ば']]
  })
]));

// Line 428-429
defSpecialCounter(1607320, makeSpecialCounter(({ args }) => [
  args(CounterText, '把', 'わ', { digitOpts: [[3, ':c', 'ば'], [7, 'しち'], [10, ':g', ':c', 'ぱ']] })
]));

// Line 431-432
defSpecialCounter(1633690, makeSpecialCounter(({ args }) => [
  args(CounterText, '段', 'だん', { digitOpts: [[7, 'しち']] })
]));

// Line 434-435
defSpecialCounter(1901390, makeSpecialCounter(({ args }) => [
  args(CounterText, '敗', 'はい', { digitOpts: [[4, ':h']] })
]));

// Line 437-438
defSpecialCounter(1919550, makeSpecialCounter(({ args }) => [
  args(CounterText, '泊', 'はく', { digitOpts: [[4, ':h']] })
]));

// Line 440-441
defSpecialCounter(1994890, makeSpecialCounter(({ args }) => [
  args(CounterText, '首', 'しゅ', { digitOpts: [[10]] })
]));

// Line 443-444
defSpecialCounter(1351270, makeSpecialCounter(({ args }) => [
  args(CounterText, '章', 'しょう', { digitOpts: [[10]] })
]));

// Line 446-447
defSpecialCounter(2019640, makeSpecialCounter(({ args }) => [
  args(CounterText, ['杯', '盃'], 'はい', { digitOpts: [[3, ':r']] })
]));

// Line 449-450
defSpecialCounter(2078550, makeSpecialCounter(({ args }) => [
  args(CounterText, '条', 'じょう', { digitOpts: [[7, 'しち']] })
]));

// Line 452-453
defSpecialCounter(2078590, makeSpecialCounter(({ args }) => [
  args(CounterText, '軒', 'けん', { digitOpts: [[3, ':r']] })
]));

// Line 455-456
defSpecialCounter(2081610, makeSpecialCounter(({ args }) => [
  args(CounterText, ['立て', 'たて', 'タテ'], 'たて', { digitOpts: [[':off']] })
]));

// Line 458-459
defSpecialCounter(2084840, makeSpecialCounter(({ args }) => [
  args(CounterText, '年', 'ねん', { digitOpts: [[4, 'よ'], [7, 'しち'], [9, 'く']], accepts: ['kan'] })
]));

// Line 461-462
defSpecialCounter(1468900, makeSpecialCounter(({ args }) => [
  args(CounterText, '年生', 'ねんせい', { digitOpts: [[4, 'よ'], [7, 'しち'], [9, 'く']] })
]));

// Line 464-465
defSpecialCounter(1502840, makeSpecialCounter(({ args }) => [
  args(CounterText, '分', 'ふん', { digitOpts: [[4, ':h']] })
]));

// Line 467-468
defSpecialCounter(2386360, makeSpecialCounter(({ args }) => [
  args(CounterText, '分間', 'ふんかん', { digitOpts: [[4, ':h']] })
]));

// Line 470-471
defSpecialCounter(1373990, makeSpecialCounter(({ args }) => [
  args(CounterText, '世紀', 'せいき', { digitOpts: [[10, 'じっ']] })
]));

// Line 473-474
defSpecialCounter(2836694, makeSpecialCounter(({ args }) => [
  args(CounterText, '傑', 'けつ', { digitOpts: [[10, 'じっ']] })
]));

// Line 476-477
defSpecialCounter(2208060, makeSpecialCounter(({ args }) => [
  args(CounterText, '遍', 'へん', { digitOpts: [[3, ':r']] })
]));

// Line 479-480
defSpecialCounter(1511870, makeSpecialCounter(({ args }) => [
  args(CounterText, ['編', '篇'], 'へん', { digitOpts: [[3, ':r']] })
]));

// Line 482-483
defSpecialCounter(2271620, makeSpecialCounter(({ args }) => [
  args(CounterText, '口', 'こう', {})
]));

// Line 485-486
defSpecialCounter(2412230, makeSpecialCounter(({ args }) => [
  args(CounterText, '足', 'そく', { digitOpts: [[3, ':r']] })
]));

// Line 488-489
defSpecialCounter(1175570, makeSpecialCounter(({ args }) => [
  args(CounterText, '円', 'えん', { digitOpts: [[4, 'よ']] })
]));

// Line 491-492
defSpecialCounter(1315130, makeSpecialCounter(({ args }) => [
  args(CounterText, '字', 'じ', { digitOpts: [[4, 'よ']] })
]));

// Line 494-495
defSpecialCounter(1487770, makeSpecialCounter(({ args }) => [
  args(CounterText, '筆', 'ひつ', { digitOpts: [[4, ':h']] })
]));

// Line 515-516
defSpecialCounter(2220330, makeSpecialCounter(({ args }) => [
  args(CounterTsu, 'つ', 'つ', {})
]));

// Line 540-541: Counter-hifumi definitions start here
defSpecialCounter(1208920, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '株', 'かぶ', { digitSet: [1, 2] })
]));

// Line 543-544
defSpecialCounter(1214060, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['竿', '棹'], 'さお', { digitSet: [1, 2, 3, 4, 5], digitOpts: [[4, 'よ'], [10]] })
]));

// Line 546-547
defSpecialCounter(1260670, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '本', 'もと', { digitSet: [1, 2, 3] })
]));

// Line 549-550
defSpecialCounter(1275640, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '口', 'くち', { digitSet: [1, 2, 3] })
]));

// Line 552-553
defSpecialCounter(1299680, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['皿', '盤'], 'さら', { digitSet: [1, 2, 3] })
]));

// Line 555-556
defSpecialCounter(1302680, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '山', 'やま', { digitSet: [1, 2, 3] })
]));

// Line 558-559
defSpecialCounter(1335810, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['重ね', '襲'], 'かさね', { digitSet: [1, 2, 3] })
]));

// Line 561-562
defSpecialCounter(1361130, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['振り', '風'], 'ふり', { digitSet: [1, 2], digitOpts: [[':off']] })
]));

// Line 564-565
defSpecialCounter(1366210, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['針', '鉤', '鈎'], 'はり', { digitSet: [1, 2], digitOpts: [[':off']] })
]));

// Line 567-568
defSpecialCounter(1379650, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['盛り', '盛'], 'もり', { digitSet: [1, 2] })
]));

// Line 570-571
defSpecialCounter(1383800, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['切り', '限り', '限'], 'きり', { digitSet: [1, 2, 3], digitOpts: [[4, 'よ'], [8]] })
]));

// Line 573-574
defSpecialCounter(1384840, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '切れ', 'きれ', { digitSet: [1, 2, 3], digitOpts: [[4, 'よ'], [8]] })
]));

// Line 576-577
defSpecialCounter(1385780, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '折', 'おり', { digitSet: [1, 2] })
]));

// Line 579-580
defSpecialCounter(1404450, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '束', 'たば', { digitSet: [1, 2] })
]));

// Line 582-583
defSpecialCounter(1426480, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '柱', 'はしら', { digitSet: [1, 2], digitOpts: [[':off']] })
]));

// Line 585-586
defSpecialCounter(1432920, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '通り', 'とおり', { digitSet: [1, 2], digitOpts: [[100, ':g']] })
]));

// Line 588-589
defSpecialCounter(1445150, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '度', 'たび', { digitSet: [1, 2], digitOpts: [[':off']], common: null })
]));

// Line 591-592
defSpecialCounter(1448350, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '棟', 'むね', { digitSet: [1, 2] })
]));

// Line 594-596
defSpecialCounter(1335730, makeSpecialCounter(({ args }) => {
  const digitSet = [1, 2, 3, 5, 7, 8, 9, 10];
  return [args(CounterHifumi, '重', 'え', { digitSet, allowed: digitSet })];
}));

// Line 598-599
defSpecialCounter(2108240, makeSpecialCounter(({ args }) => [
  args(CounterText, '重', 'じゅう', { digitOpts: [[4, 'し'], [7, 'しち'], [9, 'く']] })
]));

// Line 601-602
defSpecialCounter(1482110, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '晩', 'ばん', { digitSet: [1, 2, 3], digitOpts: [[4, 'よ']] })
]));

// Line 604-605
defSpecialCounter(1501110, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['腹', '肚'], 'はら', { digitSet: [1, 2], digitOpts: [[':off']] })
]));

// Line 607-610
defSpecialCounter(1397450, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '組', 'くみ', {
    digitSet: [1, 2, 3],
    allowed: [1, 2, 3],
    suffixDescriptions: ['(sets or pairs only)']
  }),
  args(CounterText, '組', 'くみ', { digitOpts: [[1]] })
]));

// Line 612-613
defSpecialCounter(1519300, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['房', '総'], 'ふさ', { digitSet: [1, 2], digitOpts: [[':off']] })
]));

// Line 615-616
defSpecialCounter(1552890, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '粒', 'つぶ', { digitSet: [1, 2, 3], digitOpts: [[6, ':g']] })
]));

// Line 618-619
defSpecialCounter(1564410, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '一刎', 'はね', { digitSet: [1, 2, 3], digitOpts: [[':off']] })
]));

// Line 621-623
defSpecialCounter(1585650, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['箱', '函', '匣', '筥', '筐', '凾'], 'はこ', {
    digitSet: [1, 2],
    digitOpts: [[4, 'よ'], [1000], [10000]]
  })
]));

// Line 625-626
defSpecialCounter(1602800, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['船', '舟'], 'ふね', { digitSet: [1, 2, 3], digitOpts: [[':off']] })
]));

// Line 628-629
defSpecialCounter(1853450, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['締め', '〆'], 'しめ', { digitSet: [1, 2] })
]));

// Line 631-632
defSpecialCounter(1215240, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '間', 'ま', { digitSet: [1, 2, 3, 4, 9], digitOpts: [[4, 'よ']] })
]));

// Line 634-635
defSpecialCounter(2243700, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '咫', 'あた', { digitSet: [1, 2, 3] })
]));

// Line 637-638
defSpecialCounter(2414730, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '梱', 'こり', { digitSet: [1, 2] })
]));

// Line 640-641
defSpecialCounter(1583470, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '品', 'しな', { digitSet: [1, 2, 3], digitOpts: [[4, 'よ']] })
]));

// Line 643-644
defSpecialCounter(1411070, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '袋', 'ふくろ', { digitSet: [1, 2, 3], digitOpts: [[4, 'よ'], [10, 'じっ', ':h']] })
]));

// Line 646-647
defSpecialCounter(2707020, makeSpecialCounter(({ args }) => [
  args(CounterText, '袋', 'たい', { digitOpts: [[10, 'じっ']] })
]));

// Line 649-650
defSpecialCounter(2800530, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['回り', '廻り'], 'まわり', { digitSet: [1, 2] })
]));

// Line 652-653
defSpecialCounter(1047880, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, 'ケース', 'ケース', { digitSet: [1, 2], foreign: true })
]));

// Line 655-656
defSpecialCounter(1214540, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '缶', 'かん', { digitSet: [1, 2] })
]));

// Line 658-659
defSpecialCounter(1575510, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, ['齣', 'コマ'], 'こま', { digitSet: [1, 2] })
]));

// Line 661-662
defSpecialCounter(1253800, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '桁', 'けた', { digitSet: [1, 2, 3] })
]));

// Line 664-665
defSpecialCounter(1241750, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '筋', 'すじ', { digitSet: [1, 2, 3] })
]));

// Line 667-668
defSpecialCounter(1515340, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '包み', 'つつみ', { digitSet: [1, 2, 3] })
]));

// Line 670-671
defSpecialCounter(2452360, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '片', 'ひら', { digitSet: [1, 2, 3] })
]));

// Line 673-674
defSpecialCounter(2844070, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '腰', 'こし', { digitSet: [1, 2, 3] })
]));

// Line 676-677
defSpecialCounter(2844196, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '緡', 'さし', { digitSet: [1, 2, 3] })
]));

// Line 679-680
defSpecialCounter(1175140, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '駅', 'えき', { digitSet: [1, 2] })
]));

// Line 682-683
defSpecialCounter(2855028, makeSpecialCounter(({ args }) => [
  args(CounterHifumi, '揃え', 'そろえ', { digitSet: [1, 2] })
]));

// Line 706-707: Days (kun reading)
defSpecialCounter(2083110, makeSpecialCounter(({ args }) => [
  args(CounterDaysKun, '日', 'か', { common: 0, accepts: ['kan'] })
]));

// Line 718-719: Days (on reading)
defSpecialCounter(2083100, makeSpecialCounter(({ args }) => [
  args(CounterDaysOn, '日', 'にち', {})
]));

// Line 732-733: Months
defSpecialCounter(1255430, makeSpecialCounter(({ args }) => [
  args(CounterMonths, '月', 'がつ', {})
]));

// Line 743-744: People
defSpecialCounter(2149890, makeSpecialCounter(({ args }) => [
  args(CounterPeople, '人', 'にん', { digitOpts: [[4, 'よ'], [7, 'しち']], accepts: ['chuu'] })
]));

// Line 751-752: Wari (percentage)
defSpecialCounter(1606800, makeSpecialCounter(({ args }) => [
  args(CounterWari, '割', 'わり', {})
]));

// Line 754-755: Wari discount
defSpecialCounter(1606950, makeSpecialCounter(({ args }) => [
  args(CounterWari, '割引', 'わりびき', {})
]));

// Line 764-765: Age
defSpecialCounter(1294940, makeSpecialCounter(({ args }) => [
  args(CounterAge, ['歳', '才'], 'さい', {})
]));