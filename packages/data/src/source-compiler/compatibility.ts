import { readFile } from 'node:fs/promises';
import type { KanjidicHintCompatibility } from './kanjidic-hints.js';
import type {
  CanonicalEntry,
  CanonicalSenseProperty,
  ConjugationProperty,
  SensePropertyTag
} from './model.js';
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
  readonly qualifiedTargetSeq: number;
  readonly competingTargetSeq: number;
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

export async function loadSourceCompatibility(path: string): Promise<SourceCompatibilityLedger> {
  const value = JSON.parse(await readFile(path, 'utf8')) as SourceCompatibilityLedger;
  if (value.formatVersion !== 1 || !Array.isArray(value.rows)) {
    throw new Error('Unsupported source compatibility ledger');
  }
  return value;
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
  return row.seq === value.rootSeq
    && row.route === value.route
    && row.sourceText === value.sourceText
    && (sameEmissionRule(row.rule, value.firstRule)
      || (row.lineageStep === 'either'
        && value.secondRule !== null
        && row.secondaryRules.some(rule => sameEmissionRule(rule, value.secondRule!))));
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
    if (sense.properties.some(property => property.tag === row.tag && property.text === row.text)) continue;
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
  }
  return [...bySeq.values()].sort((left, right) => left.seq - right.seq);
}
