import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalRoute,
  CanonicalSense,
  CanonicalSenseProperty,
  SensePropertyTag
} from './model.js';
import {
  type QualifiedErrataLedger,
  type QualifiedErrataRow
} from './chronological-errata-ledger.js';
export {
  loadQualifiedErrata,
  type QualifiedErrataLedger,
  type QualifiedErrataRow
} from './chronological-errata-ledger.js';

const PROPERTY_TAGS = new Set<SensePropertyTag>([
  'pos', 'misc', 'dial', 'field', 's_inf', 'stagk', 'stagr'
]);

export interface AppliedErrata {
  readonly entries: readonly CanonicalEntry[];
  readonly edits: readonly {
    readonly event: number;
    readonly phase: string;
    readonly operation: string;
    readonly sourceLine: number;
    readonly preservedBehavior: string;
  }[];
  readonly conjugationRows: readonly QualifiedErrataRow[];
  /** Stable identities for the 93 rows that were already satisfied or deferred. */
  readonly noopRowIds: readonly string[];
  readonly counts: {
    readonly declared: number;
    readonly applied: number;
    readonly noops: number;
    readonly demotedRoots: number;
  };
  readonly nextEvent: number;
}

function object(value: unknown): Record<string, unknown> {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
    ? value as Record<string, unknown> : {};
}

function number(value: unknown, label: string): number {
  if (typeof value !== 'number' || !Number.isSafeInteger(value)) throw new Error(`${label} must be an integer`);
  return value;
}

function string(value: unknown, label: string): string {
  if (typeof value !== 'string') throw new Error(`${label} must be a string`);
  return value;
}

function boolean(value: unknown, label: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${label} must be boolean`);
  return value;
}

function routeForTable(value: unknown): CanonicalRoute {
  if (value === 'kanji_text') return 'kanji';
  if (value === 'kana_text') return 'kana';
  throw new Error(`Unknown form table ${JSON.stringify(value)}`);
}

function kanaRoute(text: string): CanonicalRoute {
  return /^[ァ-ヺヽヾーぁ-ゔゝゞ]+$/u.test(text) ? 'kana' : 'kanji';
}

function forms(entry: CanonicalEntry, route: CanonicalRoute): readonly CanonicalForm[] {
  return route === 'kanji' ? entry.kanji : entry.kana;
}

function withForms(entry: CanonicalEntry, route: CanonicalRoute, value: readonly CanonicalForm[]): CanonicalEntry {
  return route === 'kanji' ? { ...entry, kanji: value } : { ...entry, kana: value };
}

function mapForms(
  entry: CanonicalEntry,
  route: CanonicalRoute,
  change: (values: readonly CanonicalForm[]) => readonly CanonicalForm[]
): CanonicalEntry {
  return withForms(entry, route, change(forms(entry, route)));
}

function sameStringSet(left: readonly string[], right: readonly string[]): boolean {
  return [...left].sort().join('\u0000') === [...right].sort().join('\u0000');
}

function addSenseProperty(
  entry: CanonicalEntry,
  senseOrdinal: number,
  tag: SensePropertyTag,
  text: string,
  event: number
): CanonicalEntry | null {
  const sense = entry.senses.find(value => value.ordinal === senseOrdinal);
  if (!sense || sense.properties.some(property => property.tag === tag && property.text === text)) return null;
  const property: CanonicalSenseProperty = {
    tag,
    text,
    ordinal: 0,
    sourceOrder: { event, ordinal: sense.properties.length }
  };
  return {
    ...entry,
    senses: entry.senses.map(value => value === sense
      ? { ...value, properties: [...value.properties, property] }
      : value)
  };
}

function deleteSenseProperty(entry: CanonicalEntry, tag: string, text: string): CanonicalEntry | null {
  let removed = false;
  const senses = entry.senses.map(sense => {
    const properties = sense.properties.filter(property => {
      const keep = property.tag !== tag || property.text !== text;
      if (!keep) removed = true;
      return keep;
    });
    return properties.length === sense.properties.length ? sense : { ...sense, properties };
  });
  return removed ? { ...entry, senses } : null;
}

function deleteSenses(entry: CanonicalEntry, predicate: string): CanonicalEntry | null {
  const match = predicate.includes('=> true') || predicate.includes('constantly')
    ? (_sense: CanonicalSense) => true
    : predicate.includes("prop.text === 'n'")
      ? (sense: CanonicalSense) => sense.properties.some(property => property.tag === 'pos' && property.text === 'n')
      : predicate.includes("prop.text === 'prt'")
        ? (sense: CanonicalSense) => sense.properties.some(property => property.tag === 'pos' && property.text === 'prt')
        : null;
  if (!match) throw new Error(`Unsupported delete-senses predicate ${predicate}`);
  const senses = entry.senses.filter(sense => !match(sense));
  return senses.length === entry.senses.length ? null : { ...entry, senses };
}

function addNewSense(
  entry: CanonicalEntry,
  positions: readonly string[],
  glosses: readonly string[],
  event: number
): CanonicalEntry | null {
  if (entry.senses.some(sense =>
    sameStringSet(sense.glosses, glosses)
    && sameStringSet(
      sense.properties.filter(property => property.tag === 'pos').map(property => property.text),
      positions
    ))) return null;

  const priorPosition = [...entry.senses].reverse().flatMap(sense =>
    [...sense.properties].reverse().filter(property => property.tag === 'pos').map(property => property.text)
  )[0];
  const properties = priorPosition !== undefined && positions.length === 1 && positions[0] === priorPosition
    ? []
    : positions.map((text, ordinal): CanonicalSenseProperty => ({
      tag: 'pos', text, ordinal, sourceOrder: { event, ordinal }
    }));
  const sense: CanonicalSense = {
    ordinal: entry.senses.reduce((maximum, value) => Math.max(maximum, value.ordinal), -1) + 1,
    glosses,
    properties
  };
  return { ...entry, senses: [...entry.senses, sense] };
}

function replacePrefix(entry: CanonicalEntry, route: CanonicalRoute, from: string, to: string): CanonicalEntry | null {
  let changed = false;
  const next = mapForms(entry, route, values => values.map(form => {
    if (!form.text.startsWith(from)) return form;
    changed = true;
    return { ...form, text: to + form.text.slice(from.length) };
  }));
  return changed ? next : null;
}

function applyDirectRow(entry: CanonicalEntry, row: QualifiedErrataRow, event: number): CanonicalEntry | null {
  const args = row.arguments;
  switch (row.operation) {
    case 'conjugateDa': {
      return addSenseProperty(entry, 0, 'pos', 'cop-da', event);
    }
    case 'setCommon': {
      const route = routeForTable(args[0]);
      const text = string(args[2], 'setCommon text');
      const common = args[3] === null ? null : number(args[3], 'setCommon rank');
      let changed = false;
      const next = mapForms(entry, route, values => values.map(form => {
        if (form.text !== text || form.common === common) return form;
        changed = true;
        return { ...form, common };
      }));
      return changed ? next : null;
    }
    case 'setPrimaryNokanji': {
      const value = boolean(args[1], 'primary-no-kanji');
      return entry.primaryNoKanji === value ? null : { ...entry, primaryNoKanji: value };
    }
    case 'addPrimaryNokanji': {
      const text = string(args[1], 'primary no-kanji form');
      const route = kanaRoute(text);
      let changed = entry.primaryNoKanji !== true;
      const next = mapForms({ ...entry, primaryNoKanji: true }, route, values => values.map(form => {
        if (form.text !== text || form.noKanji) return form;
        changed = true;
        return { ...form, noKanji: true };
      }));
      return changed ? next : null;
    }
    case 'deleteReading': {
      const text = string(args[1], 'deleted reading');
      const options = object(args[2]);
      const route = options.table === undefined ? kanaRoute(text) : routeForTable(options.table);
      const current = forms(entry, route);
      const retained = current.filter(form => form.text !== text);
      if (retained.length === current.length) return null;
      return withForms(entry, route, retained.map((form, ordinal) => ({ ...form, ordinal })));
    }
    case 'addReading': {
      const text = string(args[1], 'added reading');
      const options = object(args[2]);
      const route = options.table === undefined ? kanaRoute(text) : routeForTable(options.table);
      const current = forms(entry, route);
      if (current.some(form => form.text === text)) return null;
      const common = options.common === undefined || options.common === null
        ? null
        : number(options.common, 'added reading common rank');
      const conjugatable = options.conjugateP === undefined
        ? true
        : boolean(options.conjugateP, 'added reading conjugatable');
      const ordinal = current.reduce((maximum, form) => Math.max(maximum, form.ordinal), -1) + 1;
      return withForms(entry, route, [...current, {
        text,
        ordinal,
        sourceOrder: { event, ordinal },
        common,
        priorityTags: [],
        conjugatable,
        noKanji: false,
        best: null
      }]);
    }
    case 'replaceReading': {
      const from = string(args[1], 'old reading');
      const to = string(args[2], 'new reading');
      const route = kanaRoute(from);
      return replacePrefix(entry, route, from, to);
    }
    case 'replaceReadingConj': {
      return replacePrefix(
        entry,
        routeForTable(args[1]),
        string(args[2], 'old reading prefix'),
        string(args[3], 'new reading prefix')
      );
    }
    case 'deleteSenseProp': {
      return deleteSenseProperty(entry, string(args[1], 'property tag'), string(args[2], 'property text'));
    }
    case 'addSenseProp': {
      const tag = string(args[2], 'property tag') as SensePropertyTag;
      if (!PROPERTY_TAGS.has(tag)) throw new Error(`Unsupported property tag ${tag}`);
      return addSenseProperty(
        entry,
        number(args[1], 'sense ordinal'),
        tag,
        string(args[3], 'property text'),
        event
      );
    }
    case 'deleteSenses': {
      return deleteSenses(entry, string(object(args[1]).predicate, 'sense predicate'));
    }
    case 'rearrangeReadingsConj': {
      const route = routeForTable(args[1]);
      const prefix = string(args[2], 'reading prefix');
      const current = forms(entry, route);
      const ordered = [
        ...current.filter(form => form.text.startsWith(prefix)),
        ...current.filter(form => !form.text.startsWith(prefix))
      ].map((form, ordinal) => ({ ...form, ordinal }));
      return ordered.every((form, index) => form === current[index] && form.ordinal === current[index]!.ordinal)
        ? null
        : withForms(entry, route, ordered);
    }
    case 'addNewSense': {
      const positions = args[1];
      const glosses = args[2];
      if (!Array.isArray(positions) || !positions.every(value => typeof value === 'string')) {
        throw new Error('addNewSense positions must be strings');
      }
      if (!Array.isArray(glosses) || !glosses.every(value => typeof value === 'string')) {
        throw new Error('addNewSense glosses must be strings');
      }
      return addNewSense(entry, positions, glosses, event);
    }
    case 'addGloss': {
      const senseOrdinal = number(args[1], 'gloss sense ordinal');
      const additions = args.slice(2).map(value => string(value, 'gloss'));
      let changed = false;
      const senses = entry.senses.map(sense => {
        if (sense.ordinal !== senseOrdinal) return sense;
        const glosses = [...sense.glosses];
        for (const gloss of additions) {
          if (!glosses.includes(gloss)) {
            glosses.push(gloss);
            changed = true;
          }
        }
        return changed ? { ...sense, glosses } : sense;
      });
      return changed ? { ...entry, senses } : null;
    }
    case 'removeHiraganaNokanji': {
      const hasHiraganaNoKanji = entry.kana.some(form =>
        /^[ぁ-ゔゝゞー]+$/u.test(form.text) && form.noKanji);
      return entry.primaryNoKanji && hasHiraganaNoKanji
        ? { ...entry, primaryNoKanji: false }
        : null;
    }
    case 'addDehaJaReadings':
    case 'addGozaimasuConjs':
    case 'deleteConjugation':
    case 'addConjReading':
      return null;
  }
}

const CONJUGATION_OPERATIONS = new Set([
  'conjugateDa', 'addDehaJaReadings', 'addGozaimasuConjs', 'deleteConjugation',
  'addConjReading', 'rearrangeReadingsConj', 'replaceReadingConj'
]);

const NON_DIRECT_OPERATIONS = new Set([
  'addDehaJaReadings', 'addGozaimasuConjs', 'deleteConjugation', 'addConjReading'
]);

function rowEntrySeq(row: QualifiedErrataRow): number {
  if (row.operation === 'conjugateDa') return 2_089_020;
  return number(row.arguments[row.operation === 'setCommon' ? 1 : 0], `${row.operation} entry`);
}

/** Apply the pinned chronological edit declarations directly to canonical entries. */
export function applyQualifiedErrata(
  input: Iterable<CanonicalEntry>,
  ledger: QualifiedErrataLedger,
  firstEvent: number
): AppliedErrata {
  const entries = new Map<number, CanonicalEntry>();
  for (const entry of input) entries.set(entry.seq, entry);
  const edits: AppliedErrata['edits'][number][] = [];
  const conjugationRows: QualifiedErrataRow[] = [];
  const noopRowIds: string[] = [];

  for (const row of ledger.rows) {
    if (CONJUGATION_OPERATIONS.has(row.operation)) conjugationRows.push(row);
    const entry = row.operation === 'removeHiraganaNokanji' || NON_DIRECT_OPERATIONS.has(row.operation)
      ? null
      : entries.get(rowEntrySeq(row));
    let applied = false;
    if (row.operation === 'removeHiraganaNokanji') {
      for (const [seq, current] of entries) {
        const next = applyDirectRow(current, row, firstEvent + row.event);
        if (next) {
          entries.set(seq, next);
          applied = true;
        }
      }
    } else if (!NON_DIRECT_OPERATIONS.has(row.operation) && entry) {
      const next = applyDirectRow(entry, row, firstEvent + row.event);
      if (next) {
        entries.set(entry.seq, next);
        applied = true;
      }
    }
    if (applied) edits.push({
      event: firstEvent + row.event,
      phase: row.phase,
      operation: row.operation,
      sourceLine: row.sourceLine,
      preservedBehavior: row.preservedBehavior
    });
    else noopRowIds.push(row.id);
  }

  let demotedRoots = 0;
  for (const [seq, entry] of entries) {
    if (entry.senses.length !== 0) continue;
    entries.delete(seq);
    demotedRoots++;
  }
  return {
    entries: [...entries.values()].sort((left, right) => left.seq - right.seq),
    edits,
    conjugationRows,
    noopRowIds,
    counts: {
      declared: ledger.rows.length,
      applied: edits.length,
      noops: ledger.rows.length - edits.length,
      demotedRoots
    },
    nextEvent: firstEvent + ledger.rows.length
  };
}
