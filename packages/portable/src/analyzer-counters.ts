import type { AnalyzerSupportCounterVariant } from './analyzer-support.js';
import { countCharClass, geminate, getCharClass, rendaku } from './characters.js';
import { numberToKana, numberToKanji, parseNumber } from './numbers.js';

export interface PortableCounterValue {
  readonly text: string;
  readonly reading: string;
  readonly number: number;
  readonly route: 'kana' | 'kanji';
  readonly value: string;
  readonly ordinal: boolean;
}

function digitOf(value: number): number {
  const digit = value % 10;
  if (digit !== 0) return digit;
  for (const pair of [[10, 100], [100, 1000], [1000, 10000], [10000, 100000000]] as const) {
    if (value % pair[1] !== 0) return pair[0];
  }
  return digit;
}

function joinCounter(
  variant: AnalyzerSupportCounterVariant,
  value: number,
  initialNumberKana: string,
  initialCounterKana: string
): string {
  const digit = digitOf(value);
  let numberKana = initialNumberKana;
  let counterKana = initialCounterKana;
  const head = getCharClass(counterKana[0] ?? '');
  const digitOptions = variant.digitOptions.find(([option]) => option === digit);
  const off = variant.digitOptions.find(([option]) => option === ':off');

  if (off || digitOptions) {
    let modifyCounter = false;
    for (const rawOption of digitOptions?.slice(1) ?? []) {
      const option = String(rawOption);
      if (!option.startsWith(':')) {
        if (modifyCounter) counterKana = option;
        else {
          const stems: Readonly<Record<number, string>> = {
            0: 'れい', 1: 'いち', 2: 'に', 3: 'さん', 4: 'よん',
            5: 'ご', 6: 'ろく', 7: 'なな', 8: 'はち', 9: 'きゅう',
            10: 'じゅう', 100: 'ひゃく', 1000: 'せん', 10000: 'まん'
          };
          const stem = stems[digit] ?? '';
          numberKana = numberKana.slice(0, Math.max(0, numberKana.length - stem.length)) + option;
        }
      } else if (option === ':g') numberKana = geminate(numberKana);
      else if (option === ':r') counterKana = rendaku(counterKana);
      else if (option === ':h') counterKana = rendaku(counterKana, false, true);
      else if (option === ':c') modifyCounter = true;
    }
    return numberKana + counterKana;
  }

  const k = ['ka', 'ki', 'ku', 'ke', 'ko'];
  const s = ['sa', 'shi', 'su', 'se', 'so'];
  const t = ['ta', 'chi', 'tsu', 'te', 'to'];
  const h = ['ha', 'hi', 'fu', 'he', 'ho'];
  const p = ['pa', 'pi', 'pu', 'pe', 'po'];
  if (variant.foreign) {
    if (
      (digit === 6 && [...k, ...p].includes(head))
      || ((digit === 8 || digit === 10) && [...k, ...s, ...t, ...p].includes(head))
      || (digit === 100 && k.includes(head))
    ) numberKana = geminate(numberKana);
    return numberKana + counterKana;
  }

  if (digit === 1) {
    if ([...k, ...s, ...t].includes(head)) numberKana = geminate(numberKana);
    if (h.includes(head)) {
      numberKana = geminate(numberKana);
      counterKana = rendaku(counterKana, false, true);
    }
  } else if (digit === 3) {
    if (h.includes(head)) counterKana = rendaku(counterKana, false, true);
  } else if (digit === 6 || digit === 8 || digit === 10 || digit === 100) {
    if (
      (digit === 6 && [...k, ...p].includes(head))
      || ((digit === 8 || digit === 10) && [...k, ...s, ...t, ...p].includes(head))
      || (digit === 100 && k.includes(head))
    ) numberKana = geminate(numberKana);
    if (h.includes(head)) {
      numberKana = geminate(numberKana);
      counterKana = rendaku(counterKana, false, true);
    }
  } else if ((digit === 1000 || digit === 10000) && h.includes(head)) {
    counterKana = rendaku(counterKana, false, true);
  }
  return numberKana + counterKana;
}

function hifumi(value: number): string {
  return ({
    1: 'ひと', 2: 'ふた', 3: 'み', 4: 'よ', 5: 'いつ', 6: 'む',
    7: 'なな', 8: 'や', 9: 'ここの', 10: 'と'
  } as Readonly<Record<number, string>>)[value] ?? '';
}

function kunDay(value: number): string | null {
  return ({
    1: 'ついたち', 2: 'ふつか', 3: 'みっか', 4: 'よっか', 5: 'いつか',
    6: 'むいか', 7: 'なのか', 8: 'ようか', 9: 'ここのか', 10: 'とうか',
    14: 'じゅうよっか', 20: 'はつか', 24: 'にじゅうよっか', 30: 'みそか'
  } as Readonly<Record<number, string>>)[value] ?? null;
}

function validCounter(
  variant: AnalyzerSupportCounterVariant,
  value: number,
  unique: boolean
): boolean {
  if (!unique) return false;
  if (variant.allowed.length > 0 && !variant.allowed.includes(value)) return false;
  if (variant.className === 'CounterTsu') return value >= 1 && value <= 9;
  if (variant.className === 'CounterDaysOn') return (value > 10 || value === 1) && value !== 20;
  return true;
}

function valueString(variant: AnalyzerSupportCounterVariant, value: number): string {
  if (variant.className === 'CounterHalfhour') return `${value}:30`;
  if (variant.className === 'CounterMonths') {
    return [
      'January', 'February', 'March', 'April', 'May', 'June',
      'July', 'August', 'September', 'October', 'November', 'December'
    ][value - 1] ?? String(value);
  }
  if (variant.className === 'CounterWari') return `${value * 10}%`;
  const ordinal = variant.ordinal
    ? `${value}${value % 100 > 10 && value % 100 < 20 ? 'th'
      : value % 10 === 1 ? 'st' : value % 10 === 2 ? 'nd' : value % 10 === 3 ? 'rd' : 'th'}`
    : String(value);
  const descriptions = variant.suffixDescriptions.length > 0
    ? ` ${[...variant.suffixDescriptions].reverse().join(' ')}`
    : '';
  return `Value: ${ordinal}${descriptions}`;
}

/** Materialize one compiler-resolved counter variant without a runtime cache or DB. */
export function materializeAnalyzerCounter(
  numberText: string,
  variant: AnalyzerSupportCounterVariant,
  unique = true
): PortableCounterValue | null {
  let value: number;
  try {
    value = parseNumber(numberText);
  } catch {
    return null;
  }
  if (!validCounter(variant, value, unique)) return null;

  let reading: string;
  if (variant.className === 'NumberText') {
    reading = numberToKana(value, { separator: ' ' }) as string;
  } else if (variant.className === 'CounterTsu') {
    reading = [
      '', 'ひとつ', 'ふたつ', 'みっつ', 'よっつ', 'いつつ',
      'むっつ', 'ななつ', 'やっつ', 'ここのつ'
    ][value] ?? '';
  } else if (variant.className === 'CounterHifumi' && variant.digitSet.includes(value)) {
    reading = hifumi(value) + variant.kana;
  } else if (variant.className === 'CounterDaysKun') {
    reading = kunDay(value) ?? joinCounter(
      variant, value, numberToKana(value, { separator: ' ' }) as string, variant.kana
    );
  } else if (variant.className === 'CounterPeople' && (value === 1 || value === 2)) {
    reading = value === 1 ? 'ひとり' : 'ふたり';
  } else if (variant.className === 'CounterAge' && value === 20) {
    reading = 'はたち';
  } else {
    reading = joinCounter(
      variant, value, numberToKana(value, { separator: ' ' }) as string, variant.kana
    );
  }
  if (variant.suffix) reading += variant.suffix;

  const rendered = numberText + variant.text;
  return {
    text: rendered,
    reading,
    number: value,
    route: countCharClass(rendered, 'kanji-char') > 0 ? 'kanji' : 'kana',
    value: valueString(variant, value),
    ordinal: variant.ordinal
  };
}

/** Useful for counter presentation; kept here to avoid a dependency on core. */
export function analyzerCounterKanji(value: number, counterText: string): string {
  return numberToKanji(value) + counterText;
}
