import { readFile } from 'node:fs/promises';
import { gunzipSync } from 'node:zlib';
import { asHiragana, geminate, rendaku } from '@ichiran/core/compiler';
import { XMLParser } from 'fast-xml-parser';
import { consumeCompatibilityRow } from './compatibility.js';

export interface KanjidicHintCompatibility {
  readonly literal: string;
  readonly reading: string;
  readonly type: 'ja_on' | 'ja_kun';
  readonly prefix: boolean;
  readonly suffix: boolean;
}

interface HintReading {
  readonly text: string;
  readonly type: 'ja_on' | 'ja_kun';
  readonly prefix: boolean;
  readonly suffix: boolean;
}

type ReadingAlternative = readonly [
  text: string,
  type: string,
  rendaku: 'rendaku' | null,
  geminated: string | null
];
type ReadingMapItem = string | readonly ReadingAlternative[];
type MatchedItem = string | ReadingAlternative;
type ReadingMatch = string | readonly [
  literal: string,
  text: string,
  type: string,
  rendaku: 'rendaku' | null,
  geminated?: string | null
];
type DiffPart = string | readonly [source: string, replacement: string];
type Hint = readonly [kind: 'space' | 'mod', position: number];

export type KanjidicHintReadings = ReadonlyMap<string, readonly HintReading[]>;

const KANJI = /[々ヶ〆一-龯]/;
const parser = new XMLParser({
  ignoreAttributes: false,
  attributeNamePrefix: '@_',
  textNodeName: '#text',
  parseTagValue: false,
  trimValues: true,
  isArray: name => ['character', 'reading'].includes(name)
});

type XmlRecord = Record<string, unknown>;

function record(value: unknown): XmlRecord {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
    ? value as XmlRecord
    : {};
}

function values(value: unknown): unknown[] {
  if (value === undefined || value === null) return [];
  return Array.isArray(value) ? value : [value];
}

function text(value: unknown): string {
  if (typeof value === 'string' || typeof value === 'number') return String(value);
  return String(record(value)['#text'] ?? '');
}

function reading(value: unknown): HintReading | null {
  const node = record(value);
  const type = text(node['@_r_type']);
  if (type !== 'ja_on' && type !== 'ja_kun') return null;

  let normalized = asHiragana(text(node));
  const prefix = normalized.startsWith('-');
  const suffix = normalized.endsWith('-');
  if (prefix) normalized = normalized.slice(1);
  if (suffix) normalized = normalized.slice(0, -1);
  if (type === 'ja_kun') normalized = normalized.split('.')[0];
  return { text: normalized, type, prefix, suffix };
}

/** Loads only the Kanjidic fields used while compiling analyzer easy hints. */
export async function loadKanjidicHintReadings(
  path: string,
  compatibility: readonly (KanjidicHintCompatibility & { readonly id?: string })[] = []
): Promise<KanjidicHintReadings> {
  const file = await readFile(path);
  const xml = (path.endsWith('.gz') ? gunzipSync(file) : file).toString('utf8');
  const document = record(parser.parse(xml));
  const result = new Map<string, HintReading[]>();

  for (const value of values(record(document.kanjidic2).character)) {
    const character = record(value);
    const literal = text(character.literal);
    const rmgroup = record(record(character.reading_meaning).rmgroup);
    const readings = values(rmgroup.reading)
      .map(reading)
      .filter((item): item is HintReading => item !== null);
    if (literal && readings.length > 0) result.set(literal, readings);
  }

  for (const item of compatibility) {
    const readings = result.get(item.literal) ?? [];
    if (readings.some(value => value.text === item.reading && value.type === item.type)) {
      throw new Error(
        `Kanjidic compatibility is stale: ${item.literal}/${item.reading}/${item.type} already exists`
      );
    }
    readings.push({
      text: item.reading,
      type: item.type,
      prefix: item.prefix,
      suffix: item.suffix
    });
    result.set(item.literal, readings);
    consumeCompatibilityRow(item, 'kanjidic-reading');
  }
  return result;
}

function alternatives(reading: HintReading, allowRendaku: boolean): ReadingAlternative[] {
  const main: ReadingAlternative[] = [[reading.text, reading.type, null, null]];
  const final = reading.text.at(-1) ?? '';
  if (reading.text.length > 1 && reading.type === 'ja_on' && 'つくきち'.includes(final)) {
    main.push([geminate(reading.text, true), reading.type, null, final]);
  }
  if (!allowRendaku) return main;
  return [
    ...main,
    ...main.flatMap(value => [
      [rendaku(value[0], true), value[1], 'rendaku', value[3]] as const,
      [rendaku(value[0], true, true), value[1], 'rendaku', value[3]] as const
    ])
  ];
}

function normalReadings(
  readings: KanjidicHintReadings,
  literal: string,
  allowRendaku: boolean
): ReadingAlternative[] {
  const main: ReadingAlternative[] = [];
  const alternate: ReadingAlternative[] = [];
  for (const item of readings.get(literal) ?? []) {
    const [first, ...rest] = alternatives(item, allowRendaku);
    main.push(first);
    alternate.push(...rest);
  }

  const seen = new Set<string>();
  return [...main, ...alternate].filter(item => {
    if (seen.has(item[0])) return false;
    seen.add(item[0]);
    return true;
  });
}

function readingMap(readings: KanjidicHintReadings, surface: string): ReadingMapItem[] {
  const result: ReadingMapItem[] = [];
  let priorKanji: string | null = null;
  for (let index = 0; index < surface.length; index++) {
    const literal = surface[index];
    if (!KANJI.test(literal)) {
      result.push(literal);
      continue;
    }

    if (literal === '々') {
      result.push(priorKanji ? normalReadings(readings, priorKanji, true) : []);
      priorKanji = null;
    } else if (literal === 'ヶ') {
      result.push([['か', 'ja_on', null, null], ['が', 'abbr', null, null]]);
      priorKanji = null;
    } else if (literal === '〆') {
      result.push([['しめ', 'ja_kun', null, null], ['じめ', 'ja_kun', 'rendaku', null]]);
      priorKanji = '締';
    } else {
      result.push(normalReadings(readings, literal, index > 0));
      priorKanji = literal;
    }
  }
  return result;
}

function matchReadingMap(
  map: readonly ReadingMapItem[],
  kana: string,
  start = 0
): readonly [match: readonly MatchedItem[], score: number] | null {
  if (map.length === 0) return start >= kana.length ? [[], 0] : null;
  if (start >= kana.length) return null;

  const [item, ...rest] = map;
  if (typeof item === 'string') {
    if (item !== kana[start]) return null;
    const matched = matchReadingMap(rest, kana, start + 1);
    return matched ? [[item, ...matched[0]], matched[1]] : null;
  }

  const candidates: Array<readonly [readonly MatchedItem[], number]> = [];
  for (let end = start + 1; end <= kana.length; end++) {
    const matched = matchReadingMap(rest, kana, end);
    if (!matched) continue;
    const known = item.find(value => value[0] === kana.slice(start, end));
    candidates.unshift([
      [known ?? [kana.slice(start, end), 'irr', null, null], ...matched[0]],
      matched[1] - (known ? 0 : end - start)
    ]);
  }
  if (candidates.length === 0) return null;
  return candidates.reduce((best, value) => value[1] > best[1] ? value : best);
}

function matchReadings(
  readings: KanjidicHintReadings,
  surface: string,
  kana: string
): ReadingMatch[] | null {
  const matched = matchReadingMap(readingMap(readings, surface), kana);
  if (!matched) return null;

  const result: ReadingMatch[] = [];
  let literal = '';
  matched[0].forEach((item, index) => {
    if (typeof item === 'string') {
      literal += surface[index];
      return;
    }
    if (literal) result.push(literal);
    literal = '';
    const [value, type, rendakuFlag, geminated] = item;
    result.push(geminated
      ? [surface[index], value, type, rendakuFlag, geminated]
      : [surface[index], value, type, rendakuFlag]);
  });
  if (literal) result.push(literal);
  return result;
}

function matchDiff(source: string, target: string): readonly [readonly DiffPart[], number] | null {
  if (source.length === 0 || target.length === 0) return null;
  let matching = 0;
  while (matching < source.length && matching < target.length && source[matching] === target[matching]) matching++;
  if (matching === source.length && matching === target.length) return [[source], source.length];
  if (source.length === 1 || target.length === 1) return [[[source, target]], 0];

  if (matching === 0) {
    let best: readonly [readonly DiffPart[], number] | null = null;
    for (let left = 1; left < source.length; left++) {
      for (let right = 1; right < target.length; right++) {
        if (source[left] !== target[right]) continue;
        const rest = matchDiff(source.slice(left), target.slice(right));
        const candidate = rest
          ? [[[source.slice(0, left), target.slice(0, right)] as const, ...rest[0]], rest[1]] as const
          : null;
        if (candidate && (!best || candidate[1] > best[1])) best = candidate;
      }
    }
    return best;
  }
  if (matching === source.length) {
    return [[source.slice(0, -1), [source.at(-1) ?? '', target[source.length - 1]]], source.length - 1];
  }
  if (matching === target.length) {
    return [[target.slice(0, -1), [source[target.length - 1], target.at(-1) ?? '']], target.length - 1];
  }
  const rest = matchDiff(source.slice(matching), target.slice(matching));
  return rest ? [[source.slice(0, matching), ...rest[0]], rest[1] + matching] : null;
}

function translate(parts: readonly DiffPart[] | readonly ReadingMatch[], position: number): number | null {
  let offset = 0;
  let remaining = position;
  for (const part of parts) {
    if (typeof part === 'string') {
      if (remaining <= part.length) return offset + remaining;
      remaining -= part.length;
      offset += part.length;
      continue;
    }
    const sourceLength = part[0].length;
    const replacementLength = part[1].length;
    if (remaining < sourceLength) return offset + Math.min(1, Math.max(replacementLength, remaining));
    if (remaining === sourceLength) return offset + replacementLength;
    remaining -= sourceLength;
    offset += replacementLength;
  }
  return null;
}

function translateHints(parts: readonly DiffPart[] | readonly ReadingMatch[], hints: readonly Hint[]): Hint[] {
  const result: Hint[] = [];
  for (const [kind, position] of hints) {
    const translated = translate(parts, position);
    if (translated !== null) result.push([kind, translated]);
  }
  return result;
}

export function insertHintMarkers(kana: string, hints: readonly Hint[]): string {
  const markers = new Map<number, string[]>();
  for (const [kind, position] of hints) {
    const at = markers.get(position) ?? [];
    at.unshift(kind === 'space' ? '\u200b' : '\u200c');
    markers.set(position, at);
  }
  let result = '';
  for (let index = 0; index <= kana.length; index++) {
    result += [...(markers.get(index) ?? [])].reverse().join('');
    if (index < kana.length) result += kana[index];
  }
  return result;
}

/** Compiles one upstream def-easy-hint definition without a Kanjidic runtime API. */
export function compileEasyHint(
  readings: KanjidicHintReadings,
  kanjiSplit: string,
  actualKanji: string,
  actualKana: string
): string | null {
  const parts = kanjiSplit.split(' ');
  const expectedKanji = parts.join('');
  const hints: Hint[] = [];
  let position = 0;
  parts.forEach((part, index) => {
    if (index > 0) hints.push(['space', position]);
    if (['は', 'へ', 'には', 'とは'].includes(part)) hints.push(['mod', position + part.length - 1]);
    position += part.length;
  });

  const difference = matchDiff(expectedKanji, actualKanji);
  const kanjiReadings = matchReadings(readings, actualKanji, actualKana);
  if (!difference || !kanjiReadings) return null;
  return insertHintMarkers(
    actualKana,
    translateHints(kanjiReadings, translateHints(difference[0], hints))
  );
}
