import { createHash } from 'node:crypto';
import {
  loadAllConjugationRules,
  type ConjugationRulePaths
} from '../data/conj-rules.js';
import type {
  MorphologyManualPatchSource,
  MorphologySource
} from '../browser-pack/morphology-compiler.js';
import {
  emitCanonicalConjugations,
  emitPrimaryConjugations,
  type ConjugationEmission
} from './conjugation-emissions.js';
import type { QualifiedErrataRow } from './chronological-errata.js';
import {
  canonicalMorphologySource,
  type ExtraConjugationPosition
} from './morphology-input.js';
import type {
  CanonicalEntry,
  CanonicalForm,
  CanonicalRoute,
  ConjugationProperty
} from './model.js';
import { sameConjugationProperty } from './conjugation-identity.js';

export interface ConjugationSuppression {
  readonly route: CanonicalRoute;
  readonly rootSeq: number;
  readonly sourceText: string;
  readonly surface: string;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
  readonly provenance: {
    readonly event: number;
    readonly sourceLine: number;
    readonly oracleTargetSeq: number;
  };
}

export interface RegeneratedConjugationLineage {
  readonly rootSeq: number;
  readonly surface: string;
  readonly pos: string;
  readonly conjType: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
  readonly sourceText: string;
}

export interface ChronologicalConjugationFold {
  readonly manualPatches: readonly MorphologyManualPatchSource[];
  readonly suppressions: readonly ConjugationSuppression[];
  readonly regeneratedLineages: readonly RegeneratedConjugationLineage[];
  readonly counts: {
    readonly rows: number;
    readonly dehaJaPatches: number;
    readonly gozaimasuPatches: number;
    readonly manualPatches: number;
    readonly suppressions: number;
    readonly regeneratedReadings: number;
    readonly regeneratedLineages: number;
    readonly reorderedReadings: number;
    readonly replacedReadings: number;
  };
}

export interface ChronologicalConjugationFoldOptions {
  readonly dataPath?: string;
  readonly conjugationRules?: ConjugationRulePaths;
}

export interface ChronologicalMorphologySourceOptions extends ChronologicalConjugationFoldOptions {
  readonly extraPositions?: readonly ExtraConjugationPosition[];
}

interface SuppressionIdentity {
  readonly rootSeq: number;
  readonly oracleTargetSeq: number;
  readonly route: CanonicalRoute;
  readonly sourceText: string;
  readonly surface: string;
  readonly property: ConjugationProperty;
  readonly sourceEmission: 'required' | 'qualified-ghost';
}

const QUALIFIED_SUPPRESSIONS: readonly SuppressionIdentity[] = [
  {
    rootSeq: 2_257_550,
    oracleTargetSeq: 2_029_110,
    route: 'kana',
    sourceText: 'ない',
    surface: 'な',
    property: { pos: 'adj-i', type: 51, negative: null, formal: null },
    sourceEmission: 'qualified-ghost'
  },
  {
    rootSeq: 2_684_620,
    oracleTargetSeq: 2_086_640,
    route: 'kana',
    sourceText: 'しい',
    surface: 'し',
    property: { pos: 'adj-i', type: 51, negative: null, formal: null },
    sourceEmission: 'required'
  }
];

const GOZAIMASU_ROOTS = [1_612_690, 2_253_080] as const;
const GOZAIMASU_RULES = [
  { type: 1, negative: true, suffix: 'せん' },
  { type: 2, negative: null, suffix: 'した' },
  { type: 3, negative: null, suffix: 'して' },
  { type: 9, negative: null, suffix: 'しょう' },
  { type: 11, negative: null, suffix: 'したら' },
  { type: 12, negative: null, suffix: 'したり' }
] as const;

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function flagOrder(value: boolean | null): number {
  return value === null ? -1 : value ? 1 : 0;
}

function comparePatches(left: MorphologyManualPatchSource, right: MorphologyManualPatchSource): number {
  return compareText(left.route, right.route)
    || compareText(left.surface, right.surface)
    || left.rootSeq - right.rootSeq
    || compareText(left.sourceText, right.sourceText)
    || compareText(left.pos, right.pos)
    || left.conjType - right.conjType
    || flagOrder(left.negative) - flagOrder(right.negative)
    || flagOrder(left.formal) - flagOrder(right.formal)
    || compareText(JSON.stringify(left), JSON.stringify(right));
}

function compareRegenerated(
  left: RegeneratedConjugationLineage,
  right: RegeneratedConjugationLineage
): number {
  return left.rootSeq - right.rootSeq
    || compareText(left.surface, right.surface)
    || compareText(left.pos, right.pos)
    || left.conjType - right.conjType
    || flagOrder(left.negative) - flagOrder(right.negative)
    || flagOrder(left.formal) - flagOrder(right.formal)
    || compareText(left.sourceText, right.sourceText);
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value)) throw new Error(`${label} must be a safe integer`);
  return Number(value);
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string') throw new Error(`${label} must be a string`);
  return value;
}

function noArguments(row: QualifiedErrataRow): void {
  if (row.arguments.length !== 0) throw new Error(`${row.operation} does not accept arguments`);
}

function requiredEntry(entries: ReadonlyMap<number, CanonicalEntry>, seq: number): CanonicalEntry {
  const entry = entries.get(seq);
  if (!entry) throw new RangeError(`Conjugation errata references missing root ${seq}`);
  return entry;
}

function routeForms(entry: CanonicalEntry, route: CanonicalRoute): readonly CanonicalForm[] {
  return route === 'kanji' ? entry.kanji : entry.kana;
}

function routeForTable(value: unknown): CanonicalRoute {
  if (value === 'kanji_text') return 'kanji';
  if (value === 'kana_text') return 'kana';
  throw new Error(`Unknown conjugation reading table ${JSON.stringify(value)}`);
}

function sourceForm(entry: CanonicalEntry, route: CanonicalRoute, sourceText: string): CanonicalForm {
  const form = routeForms(entry, route).find(value => value.text === sourceText);
  if (!form) throw new Error(`Root ${entry.seq} has no ${route} source ${sourceText}`);
  return form;
}

function manualPatch(
  entry: CanonicalEntry,
  route: CanonicalRoute,
  surface: string,
  sourceText: string,
  property: ConjugationProperty
): MorphologyManualPatchSource {
  const source = sourceForm(entry, route, sourceText);
  return {
    route,
    surface,
    rootSeq: entry.seq,
    pos: property.pos,
    conjType: property.type,
    negative: property.negative,
    formal: property.formal,
    sourceText,
    sourceCounterpart: source.best,
    targetCounterpart: null,
    ord: source.ordinal,
    common: source.common
  };
}

function dehaJaPatches(entry: CanonicalEntry): MorphologyManualPatchSource[] {
  const patches: MorphologyManualPatchSource[] = [];
  for (const emission of emitPrimaryConjugations(entry)) {
    for (const form of emission.forms) {
      if (form.route !== 'kana' || !form.surface.startsWith('では')) continue;
      const sourceText = form.sourceText.startsWith('では')
        ? `じゃ${form.sourceText.slice(2)}`
        : form.sourceText;
      patches.push(manualPatch(
        entry,
        'kana',
        `じゃ${form.surface.slice(2)}`,
        sourceText,
        emission.first
      ));
    }
  }
  return patches;
}

function replaceFinalSu(source: string, suffix: string): string {
  if (!source.endsWith('す')) throw new Error(`ございます source does not end in す: ${source}`);
  return source.slice(0, -1) + suffix;
}

function gozaimasuPatches(entries: ReadonlyMap<number, CanonicalEntry>): MorphologyManualPatchSource[] {
  const patches: MorphologyManualPatchSource[] = [];
  for (const seq of GOZAIMASU_ROOTS) {
    const entry = requiredEntry(entries, seq);
    for (const [route, forms] of [['kanji', entry.kanji], ['kana', entry.kana]] as const) {
      for (const form of forms) {
        for (const rule of GOZAIMASU_RULES) {
          patches.push(manualPatch(entry, route, replaceFinalSu(form.text, rule.suffix), form.text, {
            pos: 'exp',
            type: rule.type,
            negative: rule.negative,
            formal: null
          }));
        }
      }
    }
  }
  return patches;
}

function regeneratedReadingLineages(
  entry: CanonicalEntry,
  reading: string,
  emissions: readonly ConjugationEmission[]
): RegeneratedConjugationLineage[] {
  const rootHasReading = [...entry.kanji, ...entry.kana].some(form => form.text === reading);
  if (!rootHasReading) throw new Error(`addConjReading source is absent from root ${entry.seq}: ${reading}`);
  const rows = emissions.filter(emission => emission.stage === 'primary').flatMap(emission =>
    emission.forms.filter(form => form.sourceText === reading).map(form => ({
      rootSeq: entry.seq,
      surface: form.surface,
      pos: emission.first.pos,
      conjType: emission.first.type,
      negative: emission.first.negative,
      formal: emission.first.formal,
      sourceText: form.sourceText
    }))
  );
  if (rows.length === 0) {
    throw new Error(`addConjReading source emitted no lineage for root ${entry.seq}: ${reading}`);
  }
  return rows;
}

function validateRearrangedReadings(entry: CanonicalEntry, route: CanonicalRoute, prefix: string): void {
  const ordered = [...routeForms(entry, route)].sort((left, right) => left.ordinal - right.ordinal);
  if (!ordered.some(form => form.text.startsWith(prefix))) {
    throw new Error(`rearrangeReadingsConj prefix is absent from root ${entry.seq}: ${prefix}`);
  }
  let passedPrefix = false;
  for (const form of ordered) {
    if (!form.text.startsWith(prefix)) passedPrefix = true;
    else if (passedPrefix) throw new Error(`rearrangeReadingsConj order was not applied to root ${entry.seq}`);
  }
}

function validateReplacedReadings(
  entry: CanonicalEntry,
  route: CanonicalRoute,
  from: string,
  to: string,
  emissions: readonly ConjugationEmission[]
): void {
  const roots = routeForms(entry, route);
  if (roots.some(form => form.text.startsWith(from)) || !roots.some(form => form.text.startsWith(to))) {
    throw new Error(`replaceReadingConj was not applied to root ${entry.seq}: ${from} -> ${to}`);
  }
  const generated = emissions.flatMap(emission => emission.forms).filter(form => form.route === route);
  if (generated.some(form =>
    form.sourceText.startsWith(from)
    || form.surface.startsWith(from)
    || form.intermediate?.startsWith(from))) {
    throw new Error(`replaceReadingConj old prefix survived emission for root ${entry.seq}: ${from}`);
  }
  if (!generated.some(form => form.sourceText.startsWith(to))) {
    throw new Error(`replaceReadingConj new prefix emitted no lineage for root ${entry.seq}: ${to}`);
  }
}

function suppressionFor(
  row: QualifiedErrataRow,
  entry: CanonicalEntry,
  oracleTargetSeq: number,
  emissions: readonly ConjugationEmission[]
): ConjugationSuppression {
  const identity = QUALIFIED_SUPPRESSIONS.find(value =>
    value.rootSeq === entry.seq && value.oracleTargetSeq === oracleTargetSeq);
  if (!identity) throw new Error(`Unreviewed deleteConjugation ${oracleTargetSeq}/${entry.seq}`);
  const emission = emissions.find(value =>
    value.stage === 'primary'
    && sameConjugationProperty(value.first, identity.property)
    && value.forms.some(form =>
      form.route === identity.route
      && form.sourceText === identity.sourceText
      && form.surface === identity.surface));
  if (!emission && identity.sourceEmission === 'required') {
    throw new Error(`deleteConjugation has no source-native semantic match ${oracleTargetSeq}/${entry.seq}`);
  }
  return {
    route: identity.route,
    rootSeq: identity.rootSeq,
    sourceText: identity.sourceText,
    surface: identity.surface,
    first: identity.property,
    second: null,
    provenance: { event: row.event, sourceLine: row.sourceLine, oracleTargetSeq }
  };
}

/** Fold every conjugation-affecting ledger row over final canonical roots. */
export function foldChronologicalConjugationErrata(
  input: Iterable<CanonicalEntry>,
  rows: readonly QualifiedErrataRow[],
  options: ChronologicalConjugationFoldOptions = {}
): ChronologicalConjugationFold {
  loadAllConjugationRules(options.conjugationRules ?? options.dataPath ?? 'data');
  const entries = new Map<number, CanonicalEntry>();
  for (const entry of input) {
    if (entries.has(entry.seq)) throw new RangeError(`Duplicate canonical root ${entry.seq}`);
    entries.set(entry.seq, entry);
  }
  const emissionCache = new Map<number, readonly ConjugationEmission[]>();
  const emissions = (entry: CanonicalEntry): readonly ConjugationEmission[] => {
    let value = emissionCache.get(entry.seq);
    if (!value) {
      value = emitCanonicalConjugations(entry);
      emissionCache.set(entry.seq, value);
    }
    return value;
  };
  const patches: MorphologyManualPatchSource[] = [];
  const suppressions: ConjugationSuppression[] = [];
  const regeneratedLineages: RegeneratedConjugationLineage[] = [];
  let priorEvent = -1;
  let dehaJaCount = 0;
  let gozaimasuCount = 0;
  let regeneratedReadings = 0;
  let reorderedReadings = 0;
  let replacedReadings = 0;

  for (const row of rows) {
    if (row.event <= priorEvent) throw new Error('Conjugation errata rows must be in chronological event order');
    priorEvent = row.event;
    switch (row.operation) {
      case 'conjugateDa': {
        noArguments(row);
        const entry = requiredEntry(entries, 2_089_020);
        if (!entry.senses.some(sense => sense.properties.some(property =>
          property.tag === 'pos' && property.text === 'cop-da'))) {
          throw new Error('conjugateDa was not applied to canonical root 2089020');
        }
        if (!emissions(entry).some(emission => emission.first.pos === 'cop')) {
          throw new Error('conjugateDa emitted no canonical copula lineage');
        }
        break;
      }
      case 'addDehaJaReadings': {
        noArguments(row);
        const additions = dehaJaPatches(requiredEntry(entries, 2_089_020));
        if (additions.length === 0) throw new Error('addDehaJaReadings produced no patches');
        patches.push(...additions);
        dehaJaCount += additions.length;
        break;
      }
      case 'addGozaimasuConjs': {
        noArguments(row);
        const additions = gozaimasuPatches(entries);
        patches.push(...additions);
        gozaimasuCount += additions.length;
        break;
      }
      case 'deleteConjugation': {
        if (row.arguments.length !== 2) throw new Error('deleteConjugation requires target and root sequences');
        const oracleTargetSeq = integer(row.arguments[0], 'deleteConjugation target');
        const entry = requiredEntry(entries, integer(row.arguments[1], 'deleteConjugation root'));
        suppressions.push(suppressionFor(row, entry, oracleTargetSeq, emissions(entry)));
        break;
      }
      case 'addConjReading': {
        if (row.arguments.length !== 2) throw new Error('addConjReading requires root and reading');
        const entry = requiredEntry(entries, integer(row.arguments[0], 'addConjReading root'));
        regeneratedLineages.push(...regeneratedReadingLineages(
          entry,
          text(row.arguments[1], 'addConjReading reading'),
          emissions(entry)
        ));
        regeneratedReadings++;
        break;
      }
      case 'rearrangeReadingsConj': {
        if (row.arguments.length !== 3) throw new Error('rearrangeReadingsConj requires root, table and prefix');
        const entry = requiredEntry(entries, integer(row.arguments[0], 'rearrangeReadingsConj root'));
        validateRearrangedReadings(
          entry,
          routeForTable(row.arguments[1]),
          text(row.arguments[2], 'rearrangeReadingsConj prefix')
        );
        reorderedReadings++;
        break;
      }
      case 'replaceReadingConj': {
        if (row.arguments.length !== 4) throw new Error('replaceReadingConj requires root, table and two prefixes');
        const entry = requiredEntry(entries, integer(row.arguments[0], 'replaceReadingConj root'));
        validateReplacedReadings(
          entry,
          routeForTable(row.arguments[1]),
          text(row.arguments[2], 'replaceReadingConj old prefix'),
          text(row.arguments[3], 'replaceReadingConj new prefix'),
          emissions(entry)
        );
        replacedReadings++;
        break;
      }
      default:
        throw new Error(`Unsupported conjugation errata operation ${row.operation}`);
    }
  }

  const manualPatches = [...new Map(patches.map(patch => [JSON.stringify(patch), patch])).values()]
    .sort(comparePatches);
  return {
    manualPatches,
    suppressions,
    regeneratedLineages: regeneratedLineages.sort(compareRegenerated),
    counts: {
      rows: rows.length,
      dehaJaPatches: dehaJaCount,
      gozaimasuPatches: gozaimasuCount,
      manualPatches: manualPatches.length,
      suppressions: suppressions.length,
      regeneratedReadings,
      regeneratedLineages: regeneratedLineages.length,
      reorderedReadings,
      replacedReadings
    }
  };
}

/** Canonical pack-writer input with chronological manual conjugations attached. */
export function chronologicalMorphologySource(
  entries: readonly CanonicalEntry[],
  rows: readonly QualifiedErrataRow[],
  options: ChronologicalMorphologySourceOptions = {}
): MorphologySource {
  const fold = foldChronologicalConjugationErrata(entries, rows, {
    ...(options.dataPath === undefined ? {} : { dataPath: options.dataPath }),
    ...(options.conjugationRules === undefined
      ? {}
      : { conjugationRules: options.conjugationRules })
  });
  return canonicalMorphologySource(entries, options.extraPositions ?? [], fold.manualPatches);
}

export function manualPatchDigest(rows: readonly MorphologyManualPatchSource[]): string {
  const hash = createHash('sha256');
  for (const row of [...rows].sort(comparePatches)) hash.update(JSON.stringify(row)).update('\n');
  return hash.digest('hex');
}

export function regeneratedLineageDigest(rows: readonly RegeneratedConjugationLineage[]): string {
  const hash = createHash('sha256');
  for (const row of [...rows].sort(compareRegenerated)) hash.update(JSON.stringify(row)).update('\n');
  return hash.digest('hex');
}
