import { readFile } from 'node:fs/promises';
import type { KanjidicHintCompatibility } from './kanjidic-hints.js';
import type {
  CanonicalEntry,
  CanonicalSenseProperty,
  ConjugationProperty,
  SensePropertyTag
} from './model.js';
import { SENSE_PROPERTY_TAGS } from './model.js';
import type { EmissionRule } from './conjugation-emissions.js';
import { sameEmissionRule } from './conjugation-identity.js';

export interface KanjidicCompatibilityRow extends KanjidicHintCompatibility {
  readonly id: string;
  readonly kind: 'kanjidic-reading';
  readonly provenance: object;
  readonly preservedBehavior: string;
}

export interface CanonicalCompatibilityRow {
  readonly id: string;
  readonly kind: 'canonical-sense-property';
  readonly seq: number;
  readonly senseOrdinal: number;
  readonly tag: SensePropertyTag;
  readonly text: string;
  readonly provenance: object;
  readonly preservedBehavior: string;
}

export interface ConjugationPositionCompatibilityRow {
  readonly id: string;
  readonly kind: 'conjugation-position';
  readonly seq: number;
  readonly pos: string;
  readonly provenance: object;
  readonly preservedBehavior: string;
}

/** One qualified generated-target split caused only by historical insertion order. */
export interface PhysicalTargetOrderCompatibilityRow {
  readonly id: string;
  readonly kind: 'physical-target-order';
  readonly seq: number;
  readonly competingCreatorSeq: number;
  readonly property: ConjugationProperty;
  readonly provenance: object;
  readonly preservedBehavior: string;
}

/**
 * One reviewed historical physical-target lineage that did not receive an
 * added source reading. The complete rule declaration distinguishes physical
 * targets that share a semantic property but use different suffix variants.
 */
interface ConjugationReadingLineageCompatibilityBase {
  readonly id: string;
  readonly kind: 'conjugation-reading-lineage';
  readonly seq: number;
  readonly route: 'kana' | 'kanji';
  readonly sourceText: string;
  readonly rule: EmissionRule;
  readonly provenance: object;
  readonly preservedBehavior: string;
}

export type ConjugationReadingLineageCompatibilityRow =
  | (ConjugationReadingLineageCompatibilityBase & { readonly lineageStep: 'first' })
  | (ConjugationReadingLineageCompatibilityBase & {
    readonly lineageStep: 'either';
    /** Exact secondary declarations that reused the omitted physical target. */
    readonly secondaryRules: readonly EmissionRule[];
  });

export interface SourceCompatibilityLedger {
  readonly formatVersion: 1;
  readonly rows: readonly (
    KanjidicCompatibilityRow
    | CanonicalCompatibilityRow
    | ConjugationPositionCompatibilityRow
    | ConjugationReadingLineageCompatibilityRow
    | PhysicalTargetOrderCompatibilityRow
  )[];
}

interface CompatibilityOwner {
  readonly usedBy: Map<string, string>;
}

const OWNER_BY_ROW = new WeakMap<object, CompatibilityOwner>();
const OWNER_BY_LEDGER = new WeakMap<SourceCompatibilityLedger, CompatibilityOwner>();

/** Record the concrete compiler phase that consumed one reviewed compatibility row. */
export function consumeCompatibilityRow(row: { readonly id?: string }, phase: string): void {
  if (row.id === undefined) return;
  const owner = OWNER_BY_ROW.get(row);
  if (!owner) return;
  const prior = owner.usedBy.get(row.id);
  if (prior !== undefined && prior !== phase) {
    throw new Error(`Compatibility ${row.id} was consumed by both ${prior} and ${phase}`);
  }
  owner.usedBy.set(row.id, phase);
}

/** Fail release qualification when any compatibility row stopped affecting its named phase. */
export function assertSourceCompatibilityConsumed(
  ledger: SourceCompatibilityLedger
): readonly { readonly id: string; readonly kind: string; readonly phase: string }[] {
  const owner = OWNER_BY_LEDGER.get(ledger);
  if (!owner) throw new Error('Source compatibility ledger was not loaded through the strict boundary');
  const evidence = ledger.rows.map(row => ({
    id: row.id,
    kind: row.kind,
    phase: owner.usedBy.get(row.id) ?? ''
  }));
  const stale = evidence.find(row => row.phase === '');
  if (stale) throw new Error(`Compatibility ${stale.id} was not consumed`);
  return evidence;
}

type JsonObject = Record<string, unknown>;

const PROPERTY_TAGS = new Set<string>(SENSE_PROPERTY_TAGS);

function record(value: unknown, label: string): JsonObject {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as JsonObject;
}

function exactKeys(value: JsonObject, keys: readonly string[], label: string): void {
  const expected = new Set(keys);
  const unknown = Object.keys(value).filter(key => !expected.has(key));
  const missing = keys.filter(key => !(key in value));
  if (unknown.length > 0) throw new Error(`${label} has unknown field ${unknown[0]}`);
  if (missing.length > 0) throw new Error(`${label} is missing field ${missing[0]}`);
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${label} must be a non-empty string`);
  return value;
}

function integer(value: unknown, label: string, minimum = 0, maximum = Number.MAX_SAFE_INTEGER): number {
  if (!Number.isSafeInteger(value) || Number(value) < minimum || Number(value) > maximum) {
    throw new Error(`${label} must be an integer from ${minimum} through ${maximum}`);
  }
  return Number(value);
}

function flag(value: unknown, label: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${label} must be boolean`);
  return value;
}

function nullableFlag(value: unknown, label: string): boolean | null {
  if (value !== null && typeof value !== 'boolean') throw new Error(`${label} must be boolean or null`);
  return value;
}

function provenance(value: unknown, label: string): object {
  const result = record(value, label);
  if (Object.keys(result).length === 0) throw new Error(`${label} must not be empty`);
  return result;
}

function property(value: unknown, label: string): ConjugationProperty {
  const result = record(value, label);
  exactKeys(result, ['pos', 'type', 'negative', 'formal'], label);
  return {
    pos: text(result.pos, `${label}.pos`),
    type: integer(result.type, `${label}.type`, 0, 0xffff),
    negative: nullableFlag(result.negative, `${label}.negative`),
    formal: nullableFlag(result.formal, `${label}.formal`)
  };
}

function rule(value: unknown, label: string): EmissionRule {
  const result = record(value, label);
  exactKeys(
    result,
    ['pos', 'type', 'negative', 'formal', 'order', 'stem', 'okuri', 'euphr', 'euphk'],
    label
  );
  return {
    ...property({
      pos: result.pos,
      type: result.type,
      negative: result.negative,
      formal: result.formal
    }, label),
    order: integer(result.order, `${label}.order`, 0, 0xffff),
    stem: integer(result.stem, `${label}.stem`, 0, 0xffff),
    okuri: typeof result.okuri === 'string' ? result.okuri : text(result.okuri, `${label}.okuri`),
    euphr: typeof result.euphr === 'string' ? result.euphr : text(result.euphr, `${label}.euphr`),
    euphk: typeof result.euphk === 'string' ? result.euphk : text(result.euphk, `${label}.euphk`)
  };
}

function commonFields(value: JsonObject, label: string): {
  readonly id: string;
  readonly provenance: object;
  readonly preservedBehavior: string;
} {
  return {
    id: text(value.id, `${label}.id`),
    provenance: provenance(value.provenance, `${label}.provenance`),
    preservedBehavior: text(value.preservedBehavior, `${label}.preservedBehavior`)
  };
}

function parseRow(value: unknown, index: number): SourceCompatibilityLedger['rows'][number] {
  const label = `compatibility row ${index}`;
  const row = record(value, label);
  const kind = text(row.kind, `${label}.kind`);
  switch (kind) {
    case 'kanjidic-reading': {
      exactKeys(row, [
        'id', 'kind', 'literal', 'reading', 'type', 'prefix', 'suffix',
        'provenance', 'preservedBehavior'
      ], label);
      const type = row.type;
      if (type !== 'ja_on' && type !== 'ja_kun') throw new Error(`${label}.type is unsupported`);
      return {
        ...commonFields(row, label),
        kind,
        literal: text(row.literal, `${label}.literal`),
        reading: text(row.reading, `${label}.reading`),
        type,
        prefix: flag(row.prefix, `${label}.prefix`),
        suffix: flag(row.suffix, `${label}.suffix`)
      };
    }
    case 'canonical-sense-property': {
      exactKeys(row, [
        'id', 'kind', 'seq', 'senseOrdinal', 'tag', 'text', 'provenance', 'preservedBehavior'
      ], label);
      const tag = text(row.tag, `${label}.tag`);
      if (!PROPERTY_TAGS.has(tag)) throw new Error(`${label}.tag is unsupported`);
      return {
        ...commonFields(row, label),
        kind,
        seq: integer(row.seq, `${label}.seq`, 1),
        senseOrdinal: integer(row.senseOrdinal, `${label}.senseOrdinal`),
        tag: tag as SensePropertyTag,
        text: text(row.text, `${label}.text`)
      };
    }
    case 'conjugation-position': {
      exactKeys(row, ['id', 'kind', 'seq', 'pos', 'provenance', 'preservedBehavior'], label);
      return {
        ...commonFields(row, label),
        kind,
        seq: integer(row.seq, `${label}.seq`, 1),
        pos: text(row.pos, `${label}.pos`)
      };
    }
    case 'physical-target-order': {
      exactKeys(row, [
        'id', 'kind', 'seq', 'competingCreatorSeq', 'property', 'provenance', 'preservedBehavior'
      ], label);
      return {
        ...commonFields(row, label),
        kind,
        seq: integer(row.seq, `${label}.seq`, 1),
        competingCreatorSeq: integer(row.competingCreatorSeq, `${label}.competingCreatorSeq`, 1),
        property: property(row.property, `${label}.property`)
      };
    }
    case 'conjugation-reading-lineage': {
      const lineageStep = row.lineageStep;
      if (lineageStep !== 'first' && lineageStep !== 'either') {
        throw new Error(`${label}.lineageStep is unsupported`);
      }
      exactKeys(row, lineageStep === 'first'
        ? ['id', 'kind', 'seq', 'route', 'sourceText', 'rule', 'lineageStep', 'provenance', 'preservedBehavior']
        : [
          'id', 'kind', 'seq', 'route', 'sourceText', 'rule', 'lineageStep', 'secondaryRules',
          'provenance', 'preservedBehavior'
        ], label);
      if (row.route !== 'kana' && row.route !== 'kanji') throw new Error(`${label}.route is unsupported`);
      const route: 'kana' | 'kanji' = row.route;
      const base = {
        ...commonFields(row, label),
        kind,
        seq: integer(row.seq, `${label}.seq`, 1),
        route,
        sourceText: text(row.sourceText, `${label}.sourceText`),
        rule: rule(row.rule, `${label}.rule`)
      };
      if (lineageStep === 'first') return { ...base, lineageStep };
      if (!Array.isArray(row.secondaryRules) || row.secondaryRules.length === 0) {
        throw new Error(`${label}.secondaryRules must be a non-empty array`);
      }
      return {
        ...base,
        lineageStep,
        secondaryRules: row.secondaryRules.map((value, ruleIndex) =>
          rule(value, `${label}.secondaryRules[${ruleIndex}]`))
      };
    }
    default:
      throw new Error(`${label}.kind is unsupported: ${kind}`);
  }
}

function semanticIdentity(row: SourceCompatibilityLedger['rows'][number]): string {
  const { id: _id, provenance: _provenance, preservedBehavior: _preservedBehavior, ...semantic } = row;
  return JSON.stringify(semantic);
}

export async function loadSourceCompatibility(path: string): Promise<SourceCompatibilityLedger> {
  const value: unknown = JSON.parse(await readFile(path, 'utf8'));
  const root = record(value, 'source compatibility ledger');
  exactKeys(root, ['formatVersion', 'rows'], 'source compatibility ledger');
  if (root.formatVersion !== 1 || !Array.isArray(root.rows)) {
    throw new Error('Unsupported source compatibility ledger');
  }
  const rows = root.rows.map(parseRow);
  const ids = new Set<string>();
  const identities = new Set<string>();
  for (const row of rows) {
    if (ids.has(row.id)) throw new Error(`Duplicate compatibility row id ${row.id}`);
    ids.add(row.id);
    const identity = semanticIdentity(row);
    if (identities.has(identity)) throw new Error(`Duplicate compatibility semantic identity ${row.id}`);
    identities.add(identity);
  }
  const ledger: SourceCompatibilityLedger = { formatVersion: 1, rows };
  const owner: CompatibilityOwner = { usedBy: new Map() };
  OWNER_BY_LEDGER.set(ledger, owner);
  for (const row of rows) OWNER_BY_ROW.set(row, owner);
  return ledger;
}

export function kanjidicCompatibility(
  ledger: SourceCompatibilityLedger
): KanjidicHintCompatibility[] {
  return ledger.rows.filter((row): row is KanjidicCompatibilityRow => row.kind === 'kanjidic-reading');
}

export function conjugationPositionCompatibility(
  ledger: SourceCompatibilityLedger
): ConjugationPositionCompatibilityRow[] {
  return ledger.rows.filter(
    (row): row is ConjugationPositionCompatibilityRow => row.kind === 'conjugation-position'
  );
}

export function conjugationReadingLineageCompatibility(
  ledger: SourceCompatibilityLedger
): ConjugationReadingLineageCompatibilityRow[] {
  return ledger.rows.filter(
    (row): row is ConjugationReadingLineageCompatibilityRow =>
      row.kind === 'conjugation-reading-lineage'
  );
}

export function physicalTargetOrderCompatibility(
  ledger: SourceCompatibilityLedger
): PhysicalTargetOrderCompatibilityRow[] {
  return ledger.rows.filter(
    (row): row is PhysicalTargetOrderCompatibilityRow => row.kind === 'physical-target-order'
  );
}

/** True when the historical reading omission removes this exact route/form path. */
export function omitsConjugationReadingLineage(
  row: ConjugationReadingLineageCompatibilityRow,
  value: {
    readonly rootSeq: number;
    readonly route: 'kana' | 'kanji';
    readonly sourceText: string;
    readonly firstRule: EmissionRule;
    readonly secondRule: EmissionRule | null;
  }
): boolean {
  const omitted = row.seq === value.rootSeq
    && row.route === value.route
    && row.sourceText === value.sourceText
    && (sameEmissionRule(row.rule, value.firstRule)
      || (row.lineageStep === 'either'
        && value.secondRule !== null
        && row.secondaryRules.some(rule => sameEmissionRule(rule, value.secondRule!))));
  return omitted;
}

export function applyCanonicalCompatibility(
  entries: readonly CanonicalEntry[],
  ledger: SourceCompatibilityLedger,
  event: number
): CanonicalEntry[] {
  const bySeq = new Map(entries.map(entry => [entry.seq, entry]));
  let ordinal = 0;
  for (const row of ledger.rows) {
    if (row.kind !== 'canonical-sense-property') continue;
    const entry = bySeq.get(row.seq);
    const sense = entry?.senses.find(value => value.ordinal === row.senseOrdinal);
    if (!entry || !sense) throw new Error(`Compatibility ${row.id} names a missing canonical sense`);
    if (sense.properties.some(property => property.tag === row.tag && property.text === row.text)) {
      throw new Error(`Compatibility ${row.id} is stale: canonical property already exists`);
    }
    const property: CanonicalSenseProperty = {
      tag: row.tag,
      text: row.text,
      ordinal: 0,
      sourceOrder: { event, ordinal: ordinal++ }
    };
    bySeq.set(entry.seq, {
      ...entry,
      senses: entry.senses.map(value => value === sense
        ? { ...value, properties: [...value.properties, property] }
        : value)
    });
    consumeCompatibilityRow(row, 'canonical-sense-property');
  }
  return [...bySeq.values()].sort((left, right) => left.seq - right.seq);
}
