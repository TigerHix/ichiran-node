// ichiran/numbers - Number parsing and conversion
// Port of numbers.lisp

import { geminate, rendaku, join } from './characters.js';

// Line 3: defparameter *digit-kanji-default*
export const DIGIT_KANJI_DEFAULT = "〇一二三四五六七八九";
// Line 5: defparameter *digit-kanji-legal*
export const DIGIT_KANJI_LEGAL = "〇壱弐参四五六七八九拾";

// Line 7: defparameter *power-kanji*
export const POWER_KANJI = "一十百千万   億   兆   京";

type NumberClass = { type: 'jd' | 'p' | 'ad'; value: number };

// Line 9-16: defparameter *char-number-class*
const CHAR_NUMBER_CLASS_MAP: [string, NumberClass][] = [
  ["〇零", { type: 'jd', value: 0 }],
  ["一壱", { type: 'jd', value: 1 }],
  ["二弐", { type: 'jd', value: 2 }],
  ["三参", { type: 'jd', value: 3 }],
  ["四", { type: 'jd', value: 4 }],
  ["五", { type: 'jd', value: 5 }],
  ["六", { type: 'jd', value: 6 }],
  ["七", { type: 'jd', value: 7 }],
  ["八", { type: 'jd', value: 8 }],
  ["九", { type: 'jd', value: 9 }],
  ["十拾", { type: 'p', value: 1 }],
  ["百", { type: 'p', value: 2 }],
  ["千", { type: 'p', value: 3 }],
  ["万", { type: 'p', value: 4 }],
  ["億", { type: 'p', value: 8 }],
  ["兆", { type: 'p', value: 12 }],
  ["京", { type: 'p', value: 16 }],
  ["0０", { type: 'ad', value: 0 }],
  ["1１", { type: 'ad', value: 1 }],
  ["2２", { type: 'ad', value: 2 }],
  ["3３", { type: 'ad', value: 3 }],
  ["4４", { type: 'ad', value: 4 }],
  ["5５", { type: 'ad', value: 5 }],
  ["6６", { type: 'ad', value: 6 }],
  ["7７", { type: 'ad', value: 7 }],
  ["8８", { type: 'ad', value: 8 }],
  ["9９", { type: 'ad', value: 9 }]
];

// Line 18-23: defparameter *char-number-class-hash*
export const CHAR_NUMBER_CLASS_HASH = new Map<string, NumberClass>();
for (const [chars, numClass] of CHAR_NUMBER_CLASS_MAP) {
  for (const char of chars) {
    CHAR_NUMBER_CLASS_HASH.set(char, numClass);
  }
}

// Line 25-26: defparameter *digit-to-kana*
const DIGIT_TO_KANA: Record<number, string> = {
  0: "れい", 1: "いち", 2: "に", 3: "さん", 4: "よん",
  5: "ご", 6: "ろく", 7: "なな", 8: "はち", 9: "きゅう"
};

// Line 28-29: defparameter *power-to-kana*
const POWER_TO_KANA: Record<number, string> = {
  1: "じゅう", 2: "ひゃく", 3: "せん", 4: "まん",
  8: "おく", 12: "ちょう", 16: "けい"
};

// Line 32-50: defun number-to-kanji
export function numberToKanji(
  n: number,
  options: {
    digits?: string;
    powers?: string;
    "1sen"?: boolean;
  } = {}
): string {
  const digits = options.digits ?? DIGIT_KANJI_DEFAULT;
  const powers = options.powers ?? POWER_KANJI;
  const oneSen = options["1sen"] ?? false;

  if (!Number.isInteger(n) || n < 0) {
    throw new Error("Number must be a non-negative integer");
  }

  if (n === 0) return digits[0];

  let mp = 1;
  let mc = '';
  let p = 1;

  for (let i = 0; i < powers.length && p <= n; i++) {
    const c = powers[i];
    if (c !== ' ') {
      mp = p;
      mc = c;
    }
    p *= 10;
  }

  if (mp === 1) {
    return digits[n];
  }

  const qt = Math.floor(n / mp);
  const rem = n % mp;

  const qtStr = (qt === 1 && mp <= (oneSen ? 100 : 1000))
    ? ""
    : numberToKanji(qt, { ...options, "1sen": true });

  const remStr = rem === 0 ? "" : numberToKanji(rem, options);

  return qtStr + mc + remStr;
}

// Line 53-65: defun parse-number*
function parseNumberArray(na: NumberClass[], start = 0, end = na.length): number {
  let mp = 0;
  let mi: number | null = null;

  for (let i = start; i < end; i++) {
    const item = na[i];
    if (item.type === 'p' && item.value > mp) {
      mp = item.value;
      mi = i;
    }
  }

  if (mi === null) {
    return na.slice(start, end).reduce((acc, item) => acc * 10 + item.value, 0);
  }

  if (mi === start) {
    const pow = Math.pow(10, mp);
    const rest = start + 1 < end ? parseNumberArray(na, start + 1, end) : 0;
    return pow + rest;
  }

  const leftPart = parseNumberArray(na, start, mi);
  const rightPart = mi + 1 < end ? parseNumberArray(na, mi + 1, end) : 0;
  return leftPart * Math.pow(10, mp) + rightPart;
}

// Line 67-72: define-condition not-a-number
export class NotANumberError extends Error {
  constructor(public text: string, public reason: string) {
    super(`"${text}" is not a number: ${reason}`);
    this.name = 'NotANumberError';
  }
}

// Line 74-80: defun parse-number
export function parseNumber(str: string): number {
  const numArray: NumberClass[] = [];

  for (const char of str) {
    const numClass = CHAR_NUMBER_CLASS_HASH.get(char);
    if (!numClass) {
      throw new NotANumberError(str, `Invalid character: ${char}`);
    }
    numArray.push(numClass);
  }

  return parseNumberArray(numArray);
}

// Line 82-112: defgeneric num-sandhi (multiple methods)
function numSandhi(
  c1: string | null,
  v1: number | null,
  c2: string,
  v2: number,
  s1: string,
  s2: string
): string {
  let result1 = s1;
  let result2 = s2;

  if (c1 === 'jd' && v1 === 1 && c2 === 'p') {
    if ([3, 12, 16].includes(v2)) {
      result1 = geminate(result1);
    }
  } else if (c1 === 'jd' && v1 === 3 && c2 === 'p') {
    if ([2, 3].includes(v2)) {
      result2 = rendaku(result2);
    }
  } else if (c1 === 'jd' && v1 === 6 && c2 === 'p') {
    if (v2 === 2) {
      result1 = geminate(result1);
      result2 = rendaku(result2, false, true);
    } else if (v2 === 16) {
      result1 = geminate(result1);
    }
  } else if (c1 === 'jd' && v1 === 8 && c2 === 'p') {
    if (v2 === 2) {
      result1 = geminate(result1);
      result2 = rendaku(result2, false, true);
    } else if ([3, 12, 16].includes(v2)) {
      result1 = geminate(result1);
    }
  } else if (c1 === 'p' && v1 === 1 && c2 === 'p') {
    if ([12, 16].includes(v2)) {
      result1 = geminate(result1);
    }
  } else if (c1 === 'p' && v1 === 2 && c2 === 'p') {
    if (v2 === 16) {
      result1 = geminate(result1);
    }
  }

  return result1 + result2;
}

// Line 114-120: defun group-to-kana
function groupToKana(group: NumberClass[]): string {
  let result = "";
  let lastClass: string | null = null;
  let lastVal: number | null = null;

  for (const item of group) {
    const classType = item.type;
    const val = item.value;
    let kana = "";

    if (classType === 'jd') {
      kana = DIGIT_TO_KANA[val] ?? "";
    } else if (classType === 'p') {
      kana = POWER_TO_KANA[val] ?? "";
    }

    result = numSandhi(lastClass, lastVal, classType, val, result, kana);
    lastClass = classType;
    lastVal = val;
  }

  return result;
}

// Line 122-138: defun number-to-kana
export function numberToKana(
  n: number,
  options: {
    separator?: string | null;
    kanjiMethod?: (n: number) => string;
  } = {}
): string | string[] {
  const separator = options.separator === undefined ? ' ' : options.separator;
  const kanjiMethod = options.kanjiMethod ?? numberToKanji;

  const kanji = kanjiMethod(n);
  const groups: NumberClass[][] = [];
  let curGroup: NumberClass[] = [];
  let lastClass: string | null = null;
  let lastVal: number | null = null;

  for (const kanjiChar of kanji) {
    const numClass = CHAR_NUMBER_CLASS_HASH.get(kanjiChar);
    if (!numClass) continue;

    const classType = numClass.type;
    const val = numClass.value;

    if (!lastClass ||
        (classType === 'p' &&
         (lastClass === 'jd' || (lastClass === 'p' && val > (lastVal ?? 0))))) {
      curGroup.push(numClass);
    } else {
      groups.push([...curGroup]);
      curGroup = [numClass];
    }

    lastClass = classType;
    lastVal = val;
  }

  if (curGroup.length > 0) {
    groups.push(curGroup);
  }

  if (separator !== null) {
    return join(separator, groups, groupToKana);
  } else {
    return groups.map(groupToKana);
  }
}