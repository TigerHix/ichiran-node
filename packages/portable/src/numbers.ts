import { geminate, rendaku } from './characters.js';

export const DIGIT_KANJI_DEFAULT = '〇一二三四五六七八九';
export const DIGIT_KANJI_LEGAL = '〇壱弐参四五六七八九拾';
export const POWER_KANJI = '一十百千万   億   兆   京';

type NumberClass = Readonly<{ type: 'jd' | 'p' | 'ad'; value: number }>;

const NUMBER_CLASSES: readonly (readonly [string, NumberClass])[] = [
  ['〇零', { type: 'jd', value: 0 }], ['一壱', { type: 'jd', value: 1 }],
  ['二弐', { type: 'jd', value: 2 }], ['三参', { type: 'jd', value: 3 }],
  ['四', { type: 'jd', value: 4 }], ['五', { type: 'jd', value: 5 }],
  ['六', { type: 'jd', value: 6 }], ['七', { type: 'jd', value: 7 }],
  ['八', { type: 'jd', value: 8 }], ['九', { type: 'jd', value: 9 }],
  ['十拾', { type: 'p', value: 1 }], ['百', { type: 'p', value: 2 }],
  ['千', { type: 'p', value: 3 }], ['万', { type: 'p', value: 4 }],
  ['億', { type: 'p', value: 8 }], ['兆', { type: 'p', value: 12 }],
  ['京', { type: 'p', value: 16 }],
  ['0０', { type: 'ad', value: 0 }], ['1１', { type: 'ad', value: 1 }],
  ['2２', { type: 'ad', value: 2 }], ['3３', { type: 'ad', value: 3 }],
  ['4４', { type: 'ad', value: 4 }], ['5５', { type: 'ad', value: 5 }],
  ['6６', { type: 'ad', value: 6 }], ['7７', { type: 'ad', value: 7 }],
  ['8８', { type: 'ad', value: 8 }], ['9９', { type: 'ad', value: 9 }]
];

const CHARACTER_NUMBER_CLASS = new Map<string, NumberClass>();
for (const [characters, numberClass] of NUMBER_CLASSES) {
  for (const character of characters) CHARACTER_NUMBER_CLASS.set(character, numberClass);
}

const DIGIT_KANA: Readonly<Record<number, string>> = {
  0: 'れい', 1: 'いち', 2: 'に', 3: 'さん', 4: 'よん',
  5: 'ご', 6: 'ろく', 7: 'なな', 8: 'はち', 9: 'きゅう'
};
const POWER_KANA: Readonly<Record<number, string>> = {
  1: 'じゅう', 2: 'ひゃく', 3: 'せん', 4: 'まん',
  8: 'おく', 12: 'ちょう', 16: 'けい'
};

export class NotANumberError extends Error {
  constructor(readonly text: string, readonly reason: string) {
    super(`"${text}" is not a number: ${reason}`);
    this.name = 'NotANumberError';
  }
}

export function numberToKanji(
  number: number,
  options: {
    readonly digits?: string;
    readonly powers?: string;
    readonly '1sen'?: boolean;
  } = {}
): string {
  const digits = options.digits ?? DIGIT_KANJI_DEFAULT;
  const powers = options.powers ?? POWER_KANJI;
  if (!Number.isInteger(number) || number < 0) {
    throw new Error('Number must be a non-negative integer');
  }
  if (number === 0) return digits[0]!;

  let magnitude = 1;
  let magnitudeCharacter = '';
  for (let index = 0, power = 1; index < powers.length && power <= number; index++, power *= 10) {
    const character = powers[index]!;
    if (character !== ' ') {
      magnitude = power;
      magnitudeCharacter = character;
    }
  }
  if (magnitude === 1) return digits[number]!;

  const quotient = Math.floor(number / magnitude);
  const remainder = number % magnitude;
  const quotientText = quotient === 1 && magnitude <= (options['1sen'] ? 100 : 1000)
    ? ''
    : numberToKanji(quotient, { ...options, '1sen': true });
  const remainderText = remainder === 0 ? '' : numberToKanji(remainder, options);
  return quotientText + magnitudeCharacter + remainderText;
}

function parseNumberClasses(classes: readonly NumberClass[], start: number, end: number): number {
  let greatestPower = 0;
  let greatestIndex = -1;
  for (let index = start; index < end; index++) {
    const item = classes[index]!;
    if (item.type === 'p' && item.value > greatestPower) {
      greatestPower = item.value;
      greatestIndex = index;
    }
  }
  if (greatestIndex < 0) {
    let number = 0;
    for (let index = start; index < end; index++) number = number * 10 + classes[index]!.value;
    return number;
  }
  if (greatestIndex === start) {
    return 10 ** greatestPower
      + (start + 1 < end ? parseNumberClasses(classes, start + 1, end) : 0);
  }
  return parseNumberClasses(classes, start, greatestIndex) * 10 ** greatestPower
    + (greatestIndex + 1 < end ? parseNumberClasses(classes, greatestIndex + 1, end) : 0);
}

export function parseNumber(input: string): number {
  const classes: NumberClass[] = [];
  for (const character of input) {
    const numberClass = CHARACTER_NUMBER_CLASS.get(character);
    if (!numberClass) throw new NotANumberError(input, `Invalid character: ${character}`);
    classes.push(numberClass);
  }
  return parseNumberClasses(classes, 0, classes.length);
}

function joinNumberKana(
  previousType: NumberClass['type'] | null,
  previousValue: number | null,
  type: NumberClass['type'],
  value: number,
  prefix: string,
  suffix: string
): string {
  if (previousType === 'jd' && previousValue === 1 && type === 'p') {
    if (value === 3 || value === 12 || value === 16) prefix = geminate(prefix);
  } else if (previousType === 'jd' && previousValue === 3 && type === 'p') {
    if (value === 2 || value === 3) suffix = rendaku(suffix);
  } else if (previousType === 'jd' && previousValue === 6 && type === 'p') {
    if (value === 2) {
      prefix = geminate(prefix);
      suffix = rendaku(suffix, false, true);
    } else if (value === 16) prefix = geminate(prefix);
  } else if (previousType === 'jd' && previousValue === 8 && type === 'p') {
    if (value === 2) {
      prefix = geminate(prefix);
      suffix = rendaku(suffix, false, true);
    } else if (value === 3 || value === 12 || value === 16) prefix = geminate(prefix);
  } else if (previousType === 'p' && previousValue === 1 && type === 'p') {
    if (value === 12 || value === 16) prefix = geminate(prefix);
  } else if (previousType === 'p' && previousValue === 2 && type === 'p' && value === 16) {
    prefix = geminate(prefix);
  }
  return prefix + suffix;
}

function groupToKana(group: readonly NumberClass[]): string {
  let output = '';
  let previousType: NumberClass['type'] | null = null;
  let previousValue: number | null = null;
  for (const item of group) {
    const kana = item.type === 'jd' ? DIGIT_KANA[item.value]
      : item.type === 'p' ? POWER_KANA[item.value]
      : '';
    output = joinNumberKana(previousType, previousValue, item.type, item.value, output, kana ?? '');
    previousType = item.type;
    previousValue = item.value;
  }
  return output;
}

export function numberToKana(
  number: number,
  options: {
    readonly separator?: string | null;
    readonly kanjiMethod?: (number: number) => string;
  } = {}
): string | string[] {
  const separator = options.separator === undefined ? ' ' : options.separator;
  const kanji = (options.kanjiMethod ?? numberToKanji)(number);
  const groups: NumberClass[][] = [];
  let group: NumberClass[] = [];
  let previousType: NumberClass['type'] | null = null;
  let previousValue: number | null = null;

  for (const character of kanji) {
    const item = CHARACTER_NUMBER_CLASS.get(character);
    if (!item) continue;
    if (!previousType || (
      item.type === 'p'
      && (previousType === 'jd' || (previousType === 'p' && item.value > (previousValue ?? 0)))
    )) {
      group.push(item);
    } else {
      groups.push(group);
      group = [item];
    }
    previousType = item.type;
    previousValue = item.value;
  }
  if (group.length > 0) groups.push(group);
  const readings = groups.map(groupToKana);
  return separator === null ? readings : readings.join(separator);
}
