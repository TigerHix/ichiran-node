import { createReadStream } from 'node:fs';
import { createInterface } from 'node:readline';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalRestriction,
  CanonicalSense,
  CanonicalSenseProperty,
  SensePropertyTag,
  SourceOrder
} from './model.js';

type JsonObject = Record<string, unknown>;

function object(value: unknown, label: string): JsonObject {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as JsonObject;
}

function array(value: unknown, label: string): unknown[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value;
}

function string(value: unknown, label: string): string {
  if (typeof value !== 'string') throw new Error(`${label} must be a string`);
  return value;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value)) throw new Error(`${label} must be a safe integer`);
  return Number(value);
}

function boolean(value: unknown, label: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${label} must be a boolean`);
  return value;
}

function nullableInteger(value: unknown, label: string): number | null {
  return value === null ? null : integer(value, label);
}

function nullableString(value: unknown, label: string): string | null {
  return value === null ? null : string(value, label);
}

function sourceOrder(value: unknown, label: string): SourceOrder {
  const row = object(value, label);
  return { event: integer(row.event, `${label}.event`), ordinal: integer(row.ordinal, `${label}.ordinal`) };
}

function form(value: unknown, label: string): CanonicalForm {
  const row = object(value, label);
  return {
    text: string(row.text, `${label}.text`),
    ordinal: integer(row.ordinal, `${label}.ordinal`),
    sourceOrder: sourceOrder(row.sourceOrder, `${label}.sourceOrder`),
    common: nullableInteger(row.common, `${label}.common`),
    priorityTags: array(row.priorityTags, `${label}.priorityTags`)
      .map((tag, index) => string(tag, `${label}.priorityTags[${index}]`)),
    conjugatable: boolean(row.conjugatable, `${label}.conjugatable`),
    noKanji: boolean(row.noKanji, `${label}.noKanji`),
    best: nullableString(row.best, `${label}.best`)
  };
}

function senseTag(value: unknown, label: string): SensePropertyTag {
  const tag = string(value, label);
  switch (tag) {
    case 'pos': case 'misc': case 'dial': case 'field':
    case 's_inf': case 'stagk': case 'stagr': return tag;
    default: throw new Error(`${label} is not a canonical sense-property tag`);
  }
}

function senseProperty(value: unknown, label: string): CanonicalSenseProperty {
  const row = object(value, label);
  return {
    tag: senseTag(row.tag, `${label}.tag`),
    text: string(row.text, `${label}.text`),
    ordinal: integer(row.ordinal, `${label}.ordinal`),
    sourceOrder: sourceOrder(row.sourceOrder, `${label}.sourceOrder`)
  };
}

function sense(value: unknown, label: string): CanonicalSense {
  const row = object(value, label);
  return {
    ordinal: integer(row.ordinal, `${label}.ordinal`),
    glosses: array(row.glosses, `${label}.glosses`)
      .map((gloss, index) => string(gloss, `${label}.glosses[${index}]`)),
    properties: array(row.properties, `${label}.properties`)
      .map((property, index) => senseProperty(property, `${label}.properties[${index}]`))
  };
}

function restriction(value: unknown, label: string): CanonicalRestriction {
  const row = object(value, label);
  return {
    reading: string(row.reading, `${label}.reading`),
    written: string(row.written, `${label}.written`),
    ordinal: integer(row.ordinal, `${label}.ordinal`)
  };
}

export function parseCanonicalEntryJson(value: unknown, label = 'canonical entry'): CanonicalEntry {
  const row = object(value, label);
  const source = object(row.source, `${label}.source`);
  return {
    seq: integer(row.seq, `${label}.seq`),
    source: {
      sourceId: string(source.sourceId, `${label}.source.sourceId`),
      ordinal: integer(source.ordinal, `${label}.source.ordinal`)
    },
    kanji: array(row.kanji, `${label}.kanji`)
      .map((value, index) => form(value, `${label}.kanji[${index}]`)),
    kana: array(row.kana, `${label}.kana`)
      .map((value, index) => form(value, `${label}.kana[${index}]`)),
    senses: array(row.senses, `${label}.senses`)
      .map((value, index) => sense(value, `${label}.senses[${index}]`)),
    restrictions: array(row.restrictions, `${label}.restrictions`)
      .map((value, index) => restriction(value, `${label}.restrictions[${index}]`)),
    primaryNoKanji: boolean(row.primaryNoKanji, `${label}.primaryNoKanji`)
  };
}

export async function* readCanonicalEntryNdjson(path: string): AsyncGenerator<CanonicalEntry> {
  const lines = createInterface({ input: createReadStream(path), crlfDelay: Infinity });
  let lineNumber = 0;
  for await (const line of lines) {
    lineNumber++;
    if (line.trim().length === 0) continue;
    const value: unknown = JSON.parse(line);
    yield parseCanonicalEntryJson(value, `${path}:${lineNumber}`);
  }
}
