import { readFile } from 'node:fs/promises';
import type { KanjidicHintCompatibility } from './kanjidic-hints.js';
import type {
  CanonicalEntry,
  CanonicalSenseProperty,
  SensePropertyTag
} from './model.js';

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

export interface SourceCompatibilityLedger {
  readonly formatVersion: 1;
  readonly rows: readonly (
    KanjidicCompatibilityRow
    | CanonicalCompatibilityRow
    | ConjugationPositionCompatibilityRow
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
