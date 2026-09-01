import { createReadStream } from 'node:fs';
import { createGunzip } from 'node:zlib';
import type { Readable } from 'node:stream';
import { XMLParser } from 'fast-xml-parser';
import {
  SENSE_PROPERTY_TAGS,
  type CanonicalEntry,
  type CanonicalForm,
  type CanonicalRestriction,
  type CanonicalSense,
  type CanonicalSenseProperty,
  type SensePropertyTag
} from './model.js';

const MAX_ENTRY_BYTES = 10 * 1024 * 1024;
const ARRAY_TAGS = new Set([
  'k_ele', 'r_ele', 'sense', 'gloss', 'pos', 'misc', 'dial', 'field', 's_inf',
  'stagk', 'stagr', 'ke_pri', 're_pri', 're_inf', 're_restr'
]);

const xmlParser = new XMLParser({
  ignoreAttributes: false,
  attributeNamePrefix: '@_',
  textNodeName: '#text',
  parseTagValue: false,
  trimValues: true,
  ignoreDeclaration: true,
  processEntities: true,
  isArray: (tagName) => ARRAY_TAGS.has(tagName)
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

function nodeText(value: unknown): string {
  if (value === undefined || value === null) return '';
  if (typeof value === 'string' || typeof value === 'number') return String(value);
  if (Array.isArray(value)) return value.map(nodeText).join('');

  const node = record(value);
  const text: string[] = [];
  for (const [key, child] of Object.entries(node)) {
    if (key === '#text') text.push(String(child));
    else if (!key.startsWith('@_')) text.push(nodeText(child));
  }
  return text.join('');
}

function entityName(value: unknown): string {
  const text = nodeText(value);
  return text.startsWith('&') && text.endsWith(';') ? text.slice(1, -1) : text;
}

function commonality(priorityTags: readonly string[]): number | null {
  if (priorityTags.length === 0) return null;
  let common = 0;
  for (const tag of priorityTags) {
    if (!tag.startsWith('nf')) continue;
    const rank = Number.parseInt(tag.slice(2), 10);
    if (Number.isInteger(rank)) common = rank;
  }
  return common;
}

function form(
  node: XmlRecord,
  textTag: string,
  priorityTag: string,
  sourceEvent: number,
  ordinal: number,
  noKanji: boolean
): CanonicalForm {
  const priorityTags = values(node[priorityTag]).map(nodeText);
  return {
    text: nodeText(node[textTag]),
    ordinal,
    sourceOrder: { event: sourceEvent, ordinal },
    common: commonality(priorityTags),
    priorityTags,
    conjugatable: true,
    noKanji,
    best: null
  };
}

function parseKanji(entry: XmlRecord, sourceEvent: number): CanonicalForm[] {
  return values(entry.k_ele).map((value, ordinal) =>
    form(record(value), 'keb', 'ke_pri', sourceEvent, ordinal, false));
}

function parseKana(entry: XmlRecord, sourceEvent: number): {
  readonly forms: CanonicalForm[];
  readonly restrictions: CanonicalRestriction[];
} {
  const forms: CanonicalForm[] = [];
  const restrictions: CanonicalRestriction[] = [];

  for (const value of values(entry.r_ele)) {
    const node = record(value);
    if (values(node.re_inf).some(info => entityName(info) === 'ok')) continue;

    const text = nodeText(node.reb);
    for (const restriction of values(node.re_restr)) {
      restrictions.push({
        reading: text,
        written: nodeText(restriction),
        ordinal: restrictions.length
      });
    }

    forms.push(form(
      node,
      'reb',
      're_pri',
      sourceEvent,
      forms.length,
      node.re_nokanji !== undefined
    ));
  }

  return { forms, restrictions };
}

function parseSense(node: XmlRecord, sourceEvent: number, ordinal: number): CanonicalSense {
  const properties: CanonicalSenseProperty[] = [];
  let sourceOrder = 0;
  for (const tag of SENSE_PROPERTY_TAGS) {
    values(node[tag]).forEach((value, propertyOrdinal) => {
      properties.push({
        tag: tag as SensePropertyTag,
        text: entityName(value),
        ordinal: propertyOrdinal,
        sourceOrder: { event: sourceEvent, ordinal: sourceOrder++ }
      });
    });
  }

  return {
    ordinal,
    glosses: values(node.gloss).map(nodeText),
    properties
  };
}

export function parseJmdictEntry(
  xml: string,
  sourceId: string,
  sourceOrdinal: number,
  sourceEvent = sourceOrdinal
): CanonicalEntry {
  const document = record(xmlParser.parse(xml));
  const entry = record(document.entry);
  const seq = Number.parseInt(nodeText(entry.ent_seq), 10);
  if (!Number.isSafeInteger(seq)) {
    throw new Error(`JMdict entry ${sourceOrdinal} has an invalid sequence`);
  }

  const kanji = parseKanji(entry, sourceEvent);
  const kana = parseKana(entry, sourceEvent);
  return {
    seq,
    source: { sourceId, ordinal: sourceOrdinal },
    kanji,
    kana: kana.forms,
    senses: values(entry.sense).map((value, ordinal) => parseSense(record(value), sourceEvent, ordinal)),
    restrictions: kana.restrictions,
    primaryNoKanji: kana.forms.some(reading => reading.noKanji)
  };
}

export async function* streamJmdictXml(path: string): AsyncGenerator<string> {
  let input: Readable = createReadStream(path);
  if (path.endsWith('.gz')) input = input.pipe(createGunzip());
  input.setEncoding('utf8');

  let buffer = '';
  for await (const chunk of input) {
    buffer += String(chunk);
    if (buffer.length > MAX_ENTRY_BYTES && !buffer.includes('</entry>')) {
      throw new Error(`JMdict entry exceeds ${MAX_ENTRY_BYTES} bytes`);
    }

    while (true) {
      const start = buffer.indexOf('<entry>');
      if (start < 0) {
        if (buffer.length > MAX_ENTRY_BYTES) buffer = buffer.slice(-MAX_ENTRY_BYTES);
        break;
      }

      const close = buffer.indexOf('</entry>', start);
      if (close < 0) {
        buffer = buffer.slice(start);
        break;
      }

      const end = close + '</entry>'.length;
      yield buffer.slice(start, end);
      buffer = buffer.slice(end);
    }
  }
}

export async function* loadJmdictEntries(path: string, sourceId: string): AsyncGenerator<CanonicalEntry> {
  let ordinal = 0;
  for await (const xml of streamJmdictXml(path)) {
    yield parseJmdictEntry(xml, sourceId, ordinal++);
  }
}
