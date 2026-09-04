import { Database } from 'bun:sqlite';

import type { LocaleGlossEntrySource } from '../browser-pack/locale-gloss.js';
import type { CanonicalEntry } from './model.js';

export interface TomoshiZhHansRow {
  readonly entryId: string;
  readonly entryData: string;
  readonly zhData: string;
}

export interface TomoshiDatabaseIdentity {
  readonly exportVersion: string;
  readonly sourceSchemaVersion: string;
  readonly exportedAt: string;
}

export interface TomoshiZhHansProjection {
  readonly entries: readonly LocaleGlossEntrySource[];
  readonly stats: {
    readonly baseEntryCount: number;
    readonly baseSenseCount: number;
    readonly sourceEntryCount: number;
    readonly staleSourceEntryCount: number;
    readonly translatedEntryCount: number;
    readonly fallbackEntryCount: number;
    readonly translatedSenseCount: number;
    readonly fallbackSenseCount: number;
    readonly mismatchedSenseCount: number;
    readonly glossCount: number;
  };
}

type JsonRecord = Record<string, unknown>;

function record(value: unknown, label: string): JsonRecord {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as JsonRecord;
}

function array(value: unknown, label: string): readonly unknown[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value;
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${label} must be non-empty text`);
  }
  return value;
}

function json(value: string, label: string): unknown {
  try {
    return JSON.parse(value);
  } catch {
    throw new Error(`${label} is not valid JSON`);
  }
}

function integerId(value: string, label: string): number {
  if (!/^(?:0|[1-9][0-9]*)$/.test(value)) throw new Error(`${label} is not a decimal entry ID`);
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0 || result > 0xffff_ffff) {
    throw new Error(`${label} does not fit uint32`);
  }
  return result;
}

function sourceEnglishGlosses(sourceSense: unknown, label: string): readonly string[] {
  const sense = record(sourceSense, label);
  return array(sense.glosses, `${label}.glosses`).flatMap((value, glossIndex) => {
    const gloss = record(value, `${label}.glosses[${glossIndex}]`);
    const language = gloss.lang;
    if (language !== 'eng') return [];
    return [text(gloss.text, `${label}.glosses[${glossIndex}].text`)];
  });
}

function exactStrings(left: readonly string[], right: readonly string[]): boolean {
  return left.length === right.length && left.every((value, index) => value === right[index]);
}

function chineseGlosses(value: unknown, label: string): readonly string[] {
  const sense = record(value, label);
  if (sense.glosses === undefined) return [];
  return array(sense.glosses, `${label}.glosses`).map((item, glossIndex) => {
    const gloss = record(item, `${label}.glosses[${glossIndex}]`);
    return text(gloss.text, `${label}.glosses[${glossIndex}].text`);
  });
}

function translatedGroups(
  base: CanonicalEntry,
  entryData: string,
  zhData: string
): { readonly groups: LocaleGlossEntrySource['groups']; readonly mismatched: number } {
  const source = record(json(entryData, `Tomoshi entry ${base.seq}`), `Tomoshi entry ${base.seq}`);
  if (String(source.id) !== String(base.seq)) {
    throw new Error(`Tomoshi entry ${base.seq} JSON carries ID ${JSON.stringify(source.id)}`);
  }
  const sourceSenses = array(source.senses, `Tomoshi entry ${base.seq}.senses`);
  const translated = record(json(zhData, `Tomoshi zh entry ${base.seq}`), `Tomoshi zh entry ${base.seq}`);
  const translatedSenses = record(
    translated.senses,
    `Tomoshi zh entry ${base.seq}.senses`
  );
  const baseSenses = new Map(base.senses.map(sense => [sense.ordinal, sense]));
  const groups: LocaleGlossEntrySource['groups'][number][] = [];
  let mismatched = 0;
  for (const [ordinalText, value] of Object.entries(translatedSenses)) {
    const ordinal = integerId(ordinalText, `Tomoshi zh entry ${base.seq} sense key`);
    const baseSense = baseSenses.get(ordinal);
    if (!baseSense) {
      mismatched++;
      continue;
    }
    const sourceSense = sourceSenses[ordinal];
    if (sourceSense === undefined || !exactStrings(
      sourceEnglishGlosses(sourceSense, `Tomoshi entry ${base.seq}.senses[${ordinal}]`),
      baseSense.glosses
    )) {
      mismatched++;
      continue;
    }
    const glosses = chineseGlosses(value, `Tomoshi zh entry ${base.seq}.senses.${ordinal}`);
    if (glosses.length === 0) continue;
    groups.push({
      targets: [ordinal],
      glosses: glosses.map((gloss, ord) => ({ ord, text: gloss })),
      info: []
    });
  }
  groups.sort((left, right) => left.targets[0]! - right.targets[0]!);
  return { groups, mismatched };
}

/**
 * Convert Tomoshi's JMdict-derived SQLite rows into a release-local locale
 * layer. A Chinese sense is accepted only when Tomoshi's captured English
 * gloss list still exactly matches the canonical sense at that ordinal.
 */
export function projectTomoshiZhHans(
  baseEntries: readonly CanonicalEntry[],
  rows: Iterable<TomoshiZhHansRow>
): TomoshiZhHansProjection {
  const ordered = [...baseEntries].sort((left, right) => left.seq - right.seq);
  const baseBySequence = new Map(ordered.map(entry => [entry.seq, entry]));
  if (baseBySequence.size !== ordered.length) throw new Error('Canonical entries contain duplicate sequences');
  const groupsBySequence = new Map<number, LocaleGlossEntrySource['groups']>();
  const seenSource = new Set<number>();
  let sourceEntryCount = 0;
  let staleSourceEntryCount = 0;
  let mismatchedSenseCount = 0;
  for (const row of rows) {
    sourceEntryCount++;
    const seq = integerId(row.entryId, 'Tomoshi row entry_id');
    if (seenSource.has(seq)) throw new Error(`Tomoshi contains duplicate zh-CN entry ${seq}`);
    seenSource.add(seq);
    const base = baseBySequence.get(seq);
    if (!base) {
      staleSourceEntryCount++;
      continue;
    }
    const projected = translatedGroups(base, row.entryData, row.zhData);
    mismatchedSenseCount += projected.mismatched;
    if (projected.groups.length > 0) groupsBySequence.set(seq, projected.groups);
  }

  const entries = ordered.map(entry => ({
    seq: entry.seq,
    groups: groupsBySequence.get(entry.seq) ?? []
  }));
  const baseSenseCount = ordered.reduce((sum, entry) => sum + entry.senses.length, 0);
  const translatedSenseCount = entries.reduce((sum, entry) => sum + entry.groups.reduce(
    (inner, group) => inner + group.targets.length, 0
  ), 0);
  const translatedEntryCount = groupsBySequence.size;
  return {
    entries,
    stats: {
      baseEntryCount: ordered.length,
      baseSenseCount,
      sourceEntryCount,
      staleSourceEntryCount,
      translatedEntryCount,
      fallbackEntryCount: ordered.length - translatedEntryCount,
      translatedSenseCount,
      fallbackSenseCount: baseSenseCount - translatedSenseCount,
      mismatchedSenseCount,
      glossCount: entries.reduce((sum, entry) => sum + entry.groups.reduce(
        (inner, group) => inner + group.glosses.length, 0
      ), 0)
    }
  };
}

export function loadTomoshiZhHans(
  path: string,
  baseEntries: readonly CanonicalEntry[],
  expectedIdentity?: TomoshiDatabaseIdentity
): TomoshiZhHansProjection {
  const database = new Database(path, { readonly: true, strict: true });
  try {
    if (expectedIdentity) {
      const rows = database.query('SELECT key, value FROM meta').all() as Array<{
        readonly key: string;
        readonly value: string;
      }>;
      const meta = new Map(rows.map(row => [row.key, row.value]));
      for (const [key, expected] of [
        ['export_version', expectedIdentity.exportVersion],
        ['source_schema_version', expectedIdentity.sourceSchemaVersion],
        ['exported_at', expectedIdentity.exportedAt]
      ] as const) {
        if (meta.get(key) !== expected) {
          throw new Error(
            `Tomoshi metadata ${key} is ${JSON.stringify(meta.get(key))}; expected ${JSON.stringify(expected)}`
          );
        }
      }
    }
    const statement = database.query(`
      SELECT z.entry_id AS entryId, e.data AS entryData, z.data AS zhData
      FROM zh_defs AS z
      JOIN entries AS e ON e.id = z.entry_id
      WHERE z.locale = 'zh-CN'
      ORDER BY CAST(z.entry_id AS INTEGER)
    `);
    return projectTomoshiZhHans(
      baseEntries,
      statement.iterate() as Iterable<TomoshiZhHansRow>
    );
  } finally {
    database.close();
  }
}
