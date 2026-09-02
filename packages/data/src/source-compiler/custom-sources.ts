import { readFile } from 'node:fs/promises';
import { romanizeWord } from '@ichiran/core';
import { parseJmdictEntry, streamJmdictXml } from './jmdict.js';
import type { CanonicalEntry } from './model.js';

export const CUSTOM_SOURCE_IDS = {
  extra: 'ichiran-extra-260118',
  municipality: 'ichiran-jichitai-260118',
  ward: 'ichiran-gyoseiku-260118'
} as const;

export const CUSTOM_SOURCE_HASHES = {
  extra: '4a056ebe608cb7bb5284e412688ff634d3516f3249a54d37b33645d7a266093b',
  municipality: '328bd0779b1bb69a4c1bc3773c5ff71cffb59c825377ca7f31eafda3860e3af8',
  ward: '23f706ff84b5c27da86be1d7a6066630d34617e1769edb0673c91d258851ce3f'
} as const;

const EXTRA_ROOT_SEQUENCES = new Map<string, number | null>([
  ['お掛け', 12_294_525],
  ['せず', null],
  ['甲斐もない', 12_294_526],
  ['観了', 12_294_576],
  ['900000', 900_000],
  ['900001', 900_001]
]);

const TYPE_ORDER = '都道府県市区町村';
type MunicipalityType = '都' | '道' | '府' | '県' | '市' | '区' | '町' | '村';
const TYPE_READINGS: Readonly<Record<MunicipalityType, readonly string[]>> = {
  都: ['と'],
  道: ['どう'],
  府: ['ふ'],
  県: ['けん'],
  市: ['し'],
  区: ['く'],
  町: ['ちょう', 'まち'],
  村: ['そん', 'むら']
};
const TYPE_DESCRIPTIONS: Readonly<Record<MunicipalityType, string | null>> = {
  都: 'Metropolis',
  道: null,
  府: 'Prefecture',
  県: 'Prefecture',
  市: '(city)',
  区: 'Ward',
  町: '(town)',
  村: '(village)'
};

export interface ExtraRootDraft {
  readonly kind: 'extra';
  readonly entry: CanonicalEntry;
}

export interface MunicipalityDraft {
  readonly kind: 'municipality';
  readonly sourceOrdinal: number;
  readonly text: string;
  readonly reading: string;
  readonly definition: string;
  readonly type: MunicipalityType;
  readonly prefecture: string | null;
}

export interface WardDraft {
  readonly kind: 'ward';
  readonly sourceOrdinal: number;
  readonly text: string;
  readonly reading: string;
  readonly definition: string;
  readonly city: string;
}

export type GeographicDraft = MunicipalityDraft | WardDraft;

function hiragana(text: string): string {
  return text.normalize('NFKC').replace(/[\u30a1-\u30f6]/g, character =>
    String.fromCharCode(character.charCodeAt(0) - 0x60));
}

function romanizeGeo(reading: string): string {
  const romanized = romanizeWord(hiragana(reading), { method: 'hepburn-simple' });
  let wordStart = true;
  let result = '';
  for (const character of romanized) {
    if (/[a-z0-9]/i.test(character)) {
      result += wordStart ? character.toUpperCase() : character.toLowerCase();
      wordStart = false;
    } else {
      result += character;
      wordStart = true;
    }
  }
  return result;
}

function municipalityType(text: string): MunicipalityType {
  const type = text.at(-1);
  switch (type) {
    case '都': case '道': case '府': case '県':
    case '市': case '区': case '町': case '村':
      return type;
    default:
      throw new Error(`Unknown municipality type: ${text}`);
  }
}

function shortMunicipality(text: string, reading: string): readonly [string, string] {
  if (text.endsWith('道')) return [text, reading];
  const type = municipalityType(text);
  const suffix = TYPE_READINGS[type].find(value => reading.endsWith(value));
  if (!suffix) throw new Error(`Unknown municipality suffix reading: ${text}[${reading}]`);
  return [text.slice(0, -1), reading.slice(0, -suffix.length)];
}

function romanizeMunicipality(text: string, reading: string, includeType = true): string {
  const [, shortReading] = shortMunicipality(text, reading);
  const description = includeType ? TYPE_DESCRIPTIONS[municipalityType(text)] : null;
  return description ? `${romanizeGeo(shortReading)} ${description}` : romanizeGeo(shortReading);
}

function csvRows(content: string, columns: number, sourceId: string): string[][] {
  return content.trim().split(/\r?\n/).map((line, ordinal) => {
    const row = line.split(',');
    if (row.length !== columns) throw new Error(`${sourceId} row ${ordinal} has ${row.length} columns`);
    return row;
  });
}

function municipalityDrafts(content: string): MunicipalityDraft[] {
  const drafts: MunicipalityDraft[] = [];
  let proposalOrdinal = 0;
  for (const row of csvRows(content, 5, CUSTOM_SOURCE_IDS.municipality)) {
    const prefectureText = row[1] ?? '';
    const municipalityText = row[2] ?? '';
    const prefectureReading = hiragana(row[3] ?? '');
    const municipalityReadingRaw = row[4] ?? '';
    const isPrefecture = municipalityText === '';
    const text = isPrefecture ? prefectureText : municipalityText;
    const reading = isPrefecture ? prefectureReading : hiragana(municipalityReadingRaw);
    const type = municipalityType(text);
    const prefecture = isPrefecture
      ? null
      : romanizeMunicipality(prefectureText, prefectureReading);
    const definition = [romanizeMunicipality(text, reading), prefecture]
      .filter((value): value is string => value !== null)
      .join(', ');

    drafts.push({
      kind: 'municipality',
      sourceOrdinal: proposalOrdinal++,
      text,
      reading,
      definition,
      type,
      prefecture
    });

    if (type !== '道') {
      const [shortText, shortReading] = shortMunicipality(text, reading);
      drafts.push({
        kind: 'municipality',
        sourceOrdinal: proposalOrdinal++,
        text: shortText,
        reading: shortReading,
        definition,
        type,
        prefecture
      });
    }
  }

  return drafts.sort((left, right) => TYPE_ORDER.indexOf(left.type) - TYPE_ORDER.indexOf(right.type));
}

function wardDrafts(content: string): WardDraft[] {
  const drafts: WardDraft[] = [];
  let cityText = '';
  let cityReading = '';
  let city = '';
  for (const [sourceOrdinal, row] of csvRows(content, 3, CUSTOM_SOURCE_IDS.ward).entries()) {
    const text = row[1] ?? '';
    const reading = row[2] ?? '';
    if (!text.endsWith('区')) {
      cityText = text;
      cityReading = reading;
      city = romanizeMunicipality(cityText, cityReading, false);
      continue;
    }
    if (!cityText || !text.startsWith(cityText) || !reading.startsWith(cityReading)) {
      throw new Error(`${CUSTOM_SOURCE_IDS.ward} row ${sourceOrdinal} has no matching city header`);
    }
    const wardText = text.slice(cityText.length);
    const wardReading = reading.slice(cityReading.length);
    drafts.push({
      kind: 'ward',
      sourceOrdinal,
      text: wardText,
      reading: wardReading,
      definition: `${romanizeMunicipality(wardText, wardReading)}, ${city}`,
      city
    });
  }
  return drafts;
}

export async function loadExtraRootDrafts(path: string, firstEvent: number): Promise<ExtraRootDraft[]> {
  const drafts: ExtraRootDraft[] = [];
  let sourceOrdinal = 0;
  let event = firstEvent;
  for await (const xml of streamJmdictXml(path)) {
    const match = xml.match(/<ent_seq>(.*?)<\/ent_seq>/s);
    if (!match) throw new Error(`${CUSTOM_SOURCE_IDS.extra} entry ${sourceOrdinal} has no ent_seq`);
    const identity = match[1]?.trim() ?? '';
    const seq = EXTRA_ROOT_SEQUENCES.get(identity);
    if (seq === undefined) throw new Error(`Unpinned extra.xml identity ${identity}`);
    if (seq !== null) {
      const numericXml = xml.replace(/<ent_seq>.*?<\/ent_seq>/s, `<ent_seq>${seq}</ent_seq>`);
      drafts.push({
        kind: 'extra',
        entry: parseJmdictEntry(numericXml, CUSTOM_SOURCE_IDS.extra, sourceOrdinal, event++)
      });
    }
    sourceOrdinal++;
  }
  return drafts;
}

export async function loadGeographicDrafts(
  municipalityPath: string,
  wardPath: string
): Promise<GeographicDraft[]> {
  const [municipality, ward] = await Promise.all([
    readFile(municipalityPath, 'utf8'),
    readFile(wardPath, 'utf8')
  ]);
  return [...municipalityDrafts(municipality), ...wardDrafts(ward)];
}
