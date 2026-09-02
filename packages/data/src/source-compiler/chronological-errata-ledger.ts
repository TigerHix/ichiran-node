import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { SENSE_PROPERTY_TAGS, type SensePropertyTag } from './model.js';

const ERRATA_PHASES = [
  'addErrata', 'addErrataFeb17', 'addErrataJan18', 'addErrataMar18', 'addErrataAug18',
  'addErrataJan19', 'addErrataApr19', 'addErrataJan20', 'addErrataApr20', 'addErrataJul20',
  'addErrataJan21', 'addErrataMay21', 'addErrataJan22', 'addErrataDec23', 'addErrataJan25',
  'addErrataJan26', 'addErrataCounters'
] as const;

type QualifiedErrataPhase = typeof ERRATA_PHASES[number];
type FormTable = 'kanji_text' | 'kana_text';
type ReadingOptions = {
  readonly table?: FormTable;
  readonly common?: number | null;
  readonly conjugateP?: boolean;
};
type SensePredicate = {
  readonly predicate:
    | '() => true'
    | "(prop) => prop.tag === 'pos' && prop.text === 'n'"
    | "(prop) => prop.tag === 'pos' && prop.text === 'prt'";
};

interface QualifiedErrataRowBase<Operation extends string, Arguments extends readonly unknown[]> {
  /** Stable identity within the pinned upstream chronology. */
  readonly id: string;
  readonly event: number;
  readonly phase: QualifiedErrataPhase;
  readonly operation: Operation;
  readonly arguments: Arguments;
  readonly sourceLine: number;
  readonly preservedBehavior: string;
}

type NoArgumentOperation =
  | 'conjugateDa'
  | 'addDehaJaReadings'
  | 'removeHiraganaNokanji'
  | 'addGozaimasuConjs';

export type QualifiedErrataRow =
  | QualifiedErrataRowBase<NoArgumentOperation, readonly []>
  | QualifiedErrataRowBase<'setCommon', readonly [FormTable, number, string, number | null]>
  | QualifiedErrataRowBase<'setPrimaryNokanji', readonly [number, boolean]>
  | QualifiedErrataRowBase<'addPrimaryNokanji' | 'addConjReading', readonly [number, string]>
  | QualifiedErrataRowBase<
    'deleteReading' | 'addReading',
    readonly [number, string] | readonly [number, string, ReadingOptions]
  >
  | QualifiedErrataRowBase<'replaceReading', readonly [number, string, string]>
  | QualifiedErrataRowBase<'replaceReadingConj', readonly [number, FormTable, string, string]>
  | QualifiedErrataRowBase<'deleteSenseProp', readonly [number, SensePropertyTag, string]>
  | QualifiedErrataRowBase<'addSenseProp', readonly [number, number, SensePropertyTag, string]>
  | QualifiedErrataRowBase<'deleteSenses', readonly [number, SensePredicate]>
  | QualifiedErrataRowBase<'rearrangeReadingsConj', readonly [number, FormTable, string]>
  | QualifiedErrataRowBase<'addNewSense', readonly [number, readonly string[], readonly string[]]>
  | QualifiedErrataRowBase<'addGloss', readonly [number, number, ...string[]]>
  | QualifiedErrataRowBase<'deleteConjugation', readonly [number, number]>;

export interface QualifiedErrataLedger {
  readonly formatVersion: 1;
  readonly authority: {
    readonly upstreamRepository: string;
    readonly upstreamCommit: string;
    readonly upstreamPath: string;
    readonly upstreamSha256: string;
    readonly migrationPortPath: string;
  };
  readonly rows: readonly QualifiedErrataRow[];
}

type UnknownRecord = Record<string, unknown>;
const PROPERTY_TAGS = new Set<string>(SENSE_PROPERTY_TAGS);

function requiredRecord(value: unknown, label: string): UnknownRecord {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as UnknownRecord;
}

function exactKeys(value: UnknownRecord, keys: readonly string[], label: string): void {
  const expected = new Set(keys);
  const unknown = Object.keys(value).filter(key => !expected.has(key));
  const missing = keys.filter(key => !(key in value));
  if (unknown.length > 0) throw new Error(`${label} has unknown field ${unknown[0]}`);
  if (missing.length > 0) throw new Error(`${label} is missing field ${missing[0]}`);
}

function requiredText(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${label} must be a non-empty string`);
  return value;
}

function requiredInteger(
  value: unknown,
  label: string,
  minimum = 0,
  maximum = Number.MAX_SAFE_INTEGER
): number {
  if (!Number.isSafeInteger(value) || Number(value) < minimum || Number(value) > maximum) {
    throw new Error(`${label} must be an integer from ${minimum} through ${maximum}`);
  }
  return Number(value);
}

function requiredBoolean(value: unknown, label: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${label} must be boolean`);
  return value;
}

function requiredTable(value: unknown, label: string): FormTable {
  if (value !== 'kanji_text' && value !== 'kana_text') throw new Error(`${label} is unsupported`);
  return value;
}

function requiredTag(value: unknown, label: string): SensePropertyTag {
  if (typeof value !== 'string' || !PROPERTY_TAGS.has(value)) throw new Error(`${label} is unsupported`);
  return value as SensePropertyTag;
}

function requiredStringArray(value: unknown, label: string): string[] {
  if (!Array.isArray(value) || value.length === 0) throw new Error(`${label} must be a non-empty array`);
  return value.map((item, index) => requiredText(item, `${label}[${index}]`));
}

function readingOptions(value: unknown, label: string): ReadingOptions {
  const result = requiredRecord(value, label);
  const allowed = ['table', 'common', 'conjugateP'];
  const unknown = Object.keys(result).filter(key => !allowed.includes(key));
  if (unknown.length > 0) throw new Error(`${label} has unknown field ${unknown[0]}`);
  const parsed: { table?: FormTable; common?: number | null; conjugateP?: boolean } = {};
  if ('table' in result) parsed.table = requiredTable(result.table, `${label}.table`);
  if ('common' in result) parsed.common = result.common === null
    ? null : requiredInteger(result.common, `${label}.common`, 0, 0xffff);
  if ('conjugateP' in result) parsed.conjugateP = requiredBoolean(result.conjugateP, `${label}.conjugateP`);
  return parsed;
}

function argumentsArray(value: unknown, label: string, lengths: readonly number[]): unknown[] {
  if (!Array.isArray(value) || !lengths.includes(value.length)) {
    throw new Error(`${label} must contain ${lengths.join(' or ')} values`);
  }
  return value;
}

function parsedRowBase(
  row: UnknownRecord,
  index: number,
  operation: string
): Omit<QualifiedErrataRowBase<string, readonly []>, 'operation' | 'arguments'> {
  const phase = row.phase;
  if (typeof phase !== 'string' || !(ERRATA_PHASES as readonly string[]).includes(phase)) {
    throw new Error(`errata row ${index}.phase is unsupported`);
  }
  const event = requiredInteger(row.event, `errata row ${index}.event`);
  if (event !== index) throw new Error(`errata row ${index}.event must equal its chronological index`);
  const sourceLine = requiredInteger(row.sourceLine, `errata row ${index}.sourceLine`, 1);
  const argumentsDigest = createHash('sha256')
    .update(JSON.stringify(row.arguments))
    .digest('hex')
    .slice(0, 16);
  return {
    id: `${phase}:${sourceLine}:${operation}:${argumentsDigest}`,
    event,
    phase: phase as QualifiedErrataPhase,
    sourceLine,
    preservedBehavior: requiredText(row.preservedBehavior, `errata row ${index}.preservedBehavior`)
  };
}

function parseQualifiedErrataRow(value: unknown, index: number): QualifiedErrataRow {
  const label = `errata row ${index}`;
  const row = requiredRecord(value, label);
  exactKeys(row, ['event', 'phase', 'operation', 'arguments', 'sourceLine', 'preservedBehavior'], label);
  const operation = requiredText(row.operation, `${label}.operation`);
  const base = parsedRowBase(row, index, operation);
  const args = (lengths: readonly number[]) => argumentsArray(row.arguments, `${label}.arguments`, lengths);
  const seq = (value: unknown, name = 'entry') => requiredInteger(value, `${label}.${name}`, 1);
  switch (operation) {
    case 'conjugateDa':
    case 'addDehaJaReadings':
    case 'removeHiraganaNokanji':
    case 'addGozaimasuConjs':
      args([0]);
      return { ...base, operation, arguments: [] };
    case 'setCommon': {
      const value = args([4]);
      const common = value[3] === null ? null : requiredInteger(value[3], `${label}.common`, 0, 0xffff);
      return { ...base, operation, arguments: [
        requiredTable(value[0], `${label}.table`), seq(value[1]),
        requiredText(value[2], `${label}.text`), common
      ] };
    }
    case 'setPrimaryNokanji': {
      const value = args([2]);
      return { ...base, operation, arguments: [seq(value[0]), requiredBoolean(value[1], `${label}.value`)] };
    }
    case 'addPrimaryNokanji':
    case 'addConjReading': {
      const value = args([2]);
      return { ...base, operation, arguments: [seq(value[0]), requiredText(value[1], `${label}.text`)] };
    }
    case 'deleteReading':
    case 'addReading': {
      const value = args([2, 3]);
      const head = [seq(value[0]), requiredText(value[1], `${label}.text`)] as const;
      return value.length === 2
        ? { ...base, operation, arguments: head }
        : { ...base, operation, arguments: [...head, readingOptions(value[2], `${label}.options`)] };
    }
    case 'replaceReading': {
      const value = args([3]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredText(value[1], `${label}.from`), requiredText(value[2], `${label}.to`)
      ] };
    }
    case 'replaceReadingConj': {
      const value = args([4]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredTable(value[1], `${label}.table`),
        requiredText(value[2], `${label}.from`), requiredText(value[3], `${label}.to`)
      ] };
    }
    case 'deleteSenseProp': {
      const value = args([3]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredTag(value[1], `${label}.tag`), requiredText(value[2], `${label}.text`)
      ] };
    }
    case 'addSenseProp': {
      const value = args([4]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredInteger(value[1], `${label}.senseOrdinal`),
        requiredTag(value[2], `${label}.tag`), requiredText(value[3], `${label}.text`)
      ] };
    }
    case 'deleteSenses': {
      const value = args([2]);
      const predicate = requiredRecord(value[1], `${label}.predicate`);
      exactKeys(predicate, ['predicate'], `${label}.predicate`);
      const code = requiredText(predicate.predicate, `${label}.predicate.predicate`);
      if (code !== '() => true'
        && code !== "(prop) => prop.tag === 'pos' && prop.text === 'n'"
        && code !== "(prop) => prop.tag === 'pos' && prop.text === 'prt'") {
        throw new Error(`${label}.predicate.predicate is unsupported`);
      }
      return { ...base, operation, arguments: [seq(value[0]), { predicate: code }] };
    }
    case 'rearrangeReadingsConj': {
      const value = args([3]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredTable(value[1], `${label}.table`), requiredText(value[2], `${label}.prefix`)
      ] };
    }
    case 'addNewSense': {
      const value = args([3]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredStringArray(value[1], `${label}.positions`),
        requiredStringArray(value[2], `${label}.glosses`)
      ] };
    }
    case 'addGloss': {
      const value = args([3]);
      return { ...base, operation, arguments: [
        seq(value[0]), requiredInteger(value[1], `${label}.senseOrdinal`),
        ...value.slice(2).map((item, itemIndex) => requiredText(item, `${label}.glosses[${itemIndex}]`))
      ] };
    }
    case 'deleteConjugation': {
      const value = args([2]);
      return { ...base, operation, arguments: [seq(value[0], 'target'), seq(value[1])] };
    }
    default:
      throw new Error(`${label}.operation is unsupported: ${operation}`);
  }
}

export async function loadQualifiedErrata(path: string): Promise<QualifiedErrataLedger> {
  const parsed: unknown = JSON.parse(await readFile(path, 'utf8'));
  const root = requiredRecord(parsed, 'qualified errata ledger');
  exactKeys(root, ['formatVersion', 'authority', 'rows'], 'qualified errata ledger');
  if (root.formatVersion !== 1 || !Array.isArray(root.rows)) {
    throw new Error('Unsupported qualified errata ledger');
  }
  const authority = requiredRecord(root.authority, 'qualified errata authority');
  exactKeys(authority, [
    'upstreamRepository', 'upstreamCommit', 'upstreamPath', 'upstreamSha256', 'migrationPortPath'
  ], 'qualified errata authority');
  const upstreamCommit = requiredText(authority.upstreamCommit, 'qualified errata authority.upstreamCommit');
  const upstreamSha256 = requiredText(authority.upstreamSha256, 'qualified errata authority.upstreamSha256');
  if (!/^[0-9a-f]{40}$/u.test(upstreamCommit)) throw new Error('qualified errata authority.upstreamCommit is invalid');
  if (!/^[0-9a-f]{64}$/u.test(upstreamSha256)) throw new Error('qualified errata authority.upstreamSha256 is invalid');
  const rows = root.rows.map(parseQualifiedErrataRow);
  const ids = new Set<string>();
  for (const row of rows) {
    if (ids.has(row.id)) throw new Error(`Duplicate qualified errata identity ${row.id}`);
    ids.add(row.id);
  }
  return {
    formatVersion: 1,
    authority: {
      upstreamRepository: requiredText(authority.upstreamRepository, 'qualified errata authority.upstreamRepository'),
      upstreamCommit,
      upstreamPath: requiredText(authority.upstreamPath, 'qualified errata authority.upstreamPath'),
      upstreamSha256,
      migrationPortPath: requiredText(authority.migrationPortPath, 'qualified errata authority.migrationPortPath')
    },
    rows
  };
}
