import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import type { ConjugationSuppression } from './conjugation-errata.js';
import {
  conjugationSelectionsFromMorphology,
  emitConfiguredConjugations,
  type ConfiguredConjugationSelection
} from './conjugation-emission-order.js';
import {
  emitPrimaryConjugations,
  emitSecondaryConjugations,
  type ConjugationEmission,
  type EmissionForm
} from './conjugation-emissions.js';
import {
  consumeCompatibilityRow,
  omitsConjugationReadingLineage,
  type ConjugationReadingLineageCompatibilityRow
} from './compatibility.js';
import {
  conjugationPropertyKey,
  sameConjugationProperty
} from './conjugation-identity.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';

export interface ChronologicalConjugationPosition {
  readonly rootSeq: number;
  readonly pos: string;
  readonly event: number;
}

export interface ConjugationSchedulerInput {
  readonly entries: readonly CanonicalEntry[];
  readonly positionsByRoot: ReadonlyMap<number, readonly string[]>;
  /** Exact create-root identities from CustomCompilation; never inferred from seq/sourceId. */
  readonly customRootSeqs: ReadonlySet<number>;
  /** CustomCompilation.nextEvent, also the first applied errata form event. */
  readonly firstErrataEvent: number;
  /** Ledger-owned positions such as 2089020 cop and historical 1008340 cop. */
  readonly chronologicalPositions: readonly ChronologicalConjugationPosition[];
  readonly suppressions: readonly ConjugationSuppression[];
  /** Seven reviewed historical add-conj-reading physical-lineage omissions. */
  readonly lineageCompatibility: readonly ConjugationReadingLineageCompatibilityRow[];
  readonly morphology: CompiledMorphologyArtifact;
}

export interface StreamedScheduledEmission {
  readonly ordinal: number;
  readonly phase: ConjugationPhase;
  readonly phaseOrder: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly emission: ConjugationEmission;
}

interface Filter {
  readonly rootSeq: number;
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly sourceText: string | null;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
}

export const CONJUGATION_PHASE = {
  baseDirect: 0,
  basePrimary: 1,
  baseSecondary: 2,
  customDirect: 3,
  customPrimary: 4,
  customSecondary: 5,
  chronological: 6
} as const;

export type ConjugationPhase =
  typeof CONJUGATION_PHASE[keyof typeof CONJUGATION_PHASE];

/** Named subranges within the chronological phase; each retains declaration order. */
export const CONJUGATION_CHRONOLOGICAL_ORDER = {
  patchTargetCreation: 40_000_000,
  patchOccurrence: 50_000_000,
  regeneratedReading: 60_000_000
} as const;

const PHASE_STRIDE = 100_000_000;

export function conjugationPhasePrecedence(
  phase: ConjugationPhase,
  order: number
): number {
  if (!Number.isSafeInteger(order) || order < 0 || order >= PHASE_STRIDE) {
    throw new Error(`Conjugation phase order is outside its phase: ${phase}/${order}`);
  }
  const value = phase * PHASE_STRIDE + order;
  if (value > 0xffff_fffe) {
    throw new Error(`Generated creation precedence is outside uint32: ${phase}/${order}`);
  }
  return value;
}

export function directConjugationPhase(
  entry: CanonicalEntry,
  event: number,
  customRootSeqs: ReadonlySet<number>,
  firstErrataEvent: number
): ConjugationPhase {
  if (event >= firstErrataEvent) return CONJUGATION_PHASE.chronological;
  return customRootSeqs.has(entry.seq)
    ? CONJUGATION_PHASE.customDirect : CONJUGATION_PHASE.baseDirect;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function matches(filter: Filter, emission: ConjugationEmission, form: EmissionForm): boolean {
  return filter.rootSeq === emission.rootSeq
    && filter.route === form.route
    && filter.surface === form.surface
    && (filter.sourceText === null || filter.sourceText === form.sourceText)
    && sameConjugationProperty(filter.first, emission.first)
    && (filter.second === null
      ? emission.second === null
      : emission.second !== null && sameConjugationProperty(filter.second, emission.second));
}

function ruleProperty(
  morphology: CompiledMorphologyArtifact,
  ruleId: number
): ConjugationProperty {
  const rule = morphology.rules[ruleId];
  if (!rule) throw new Error(`Unknown morphology rule ${ruleId}`);
  return {
    pos: rule.pos,
    type: rule.type,
    negative: rule.negative,
    formal: rule.formal
  };
}

function filters(input: ConjugationSchedulerInput): readonly Filter[] {
  const suppressions = input.suppressions.map(value => ({
    rootSeq: value.rootSeq,
    route: value.route,
    surface: value.surface,
    sourceText: value.sourceText,
    first: value.first,
    second: value.second
  }));
  const tombstones = input.morphology.tombstones.map(value => ({
    rootSeq: value.rootSeq,
    route: value.route,
    surface: value.surface,
    sourceText: null,
    first: ruleProperty(input.morphology, value.firstRule),
    second: value.secondRule === null
      ? null : ruleProperty(input.morphology, value.secondRule)
  }));
  return [...suppressions, ...tombstones];
}

function compareEntry(left: CanonicalEntry, right: CanonicalEntry): number {
  return left.source.ordinal - right.source.ordinal || left.seq - right.seq;
}

function positionKey(rootSeq: number, pos: string): string {
  return `${rootSeq}\u0000${pos}`;
}

function positionSources(
  selection: ConfiguredConjugationSelection,
  rootSeq: number,
  pos: string
): ReadonlySet<string> {
  const sources = selection.sourcesByPosition.get(pos);
  if (!sources) throw new Error(`Configured root ${rootSeq} has no sources for ${pos}`);
  return sources;
}

function markLateSecondarySources(
  emission: ConjugationEmission,
  firstErrataEvent: number
): ConjugationEmission {
  if (emission.stage !== 'primary') return emission;
  const replacements = new Map<EmissionForm, EmissionForm>();
  let changed = false;
  const physicalForms = emission.physicalForms.map(form => {
    if (form.sourceEvent < firstErrataEvent) return form;
    changed ||= form.secondaryEligible;
    const replacement = { ...form, secondaryEligible: false };
    replacements.set(form, replacement);
    return replacement;
  });
  if (!changed) return emission;
  return {
    ...emission,
    physicalForms,
    forms: emission.forms.map(form => replacements.get(form) ?? form)
  };
}

function filterEmission(
  emission: ConjugationEmission,
  values: readonly Filter[],
  lineageCompatibility: readonly ConjugationReadingLineageCompatibilityRow[],
  lineageCompatibilityHits: Set<string>
): ConjugationEmission | null {
  const omittedLineage = (form: EmissionForm): boolean => {
    const row = lineageCompatibility.find(value => omitsConjugationReadingLineage(value, {
      rootSeq: emission.rootSeq,
      route: form.route,
      sourceText: form.sourceText,
      firstRule: form.firstRule,
      secondRule: form.secondRule
    }));
    if (!row) return false;
    lineageCompatibilityHits.add(row.id);
    consumeCompatibilityRow(row, 'conjugation-reading-lineage');
    return true;
  };
  const physicalForms = emission.physicalForms.filter(form => !omittedLineage(form));
  if (physicalForms.length === 0) return null;
  const forms = emission.forms.filter(form =>
    !values.some(filter => matches(filter, emission, form))
    && !omittedLineage(form));
  return forms.length === emission.forms.length
    && physicalForms.length === emission.physicalForms.length
    ? emission : { ...emission, physicalForms, forms };
}

/**
 * Replayable bounded-memory schedule. Only one root's emissions are live at a
 * time; global phases are reproduced by regenerating that root in later passes.
 */
export function iterateScheduledConjugations(
  input: ConjugationSchedulerInput
): Iterable<StreamedScheduledEmission> {
  const entriesBySeq = new Map(input.entries.map(entry => [entry.seq, entry]));
  if (entriesBySeq.size !== input.entries.length) throw new Error('Canonical entries contain duplicate ids');
  const chronological = new Map<string, ChronologicalConjugationPosition>();
  for (const value of input.chronologicalPositions) {
    if (!entriesBySeq.has(value.rootSeq)) throw new Error(`Chronological position has missing root ${value.rootSeq}`);
    const key = positionKey(value.rootSeq, value.pos);
    if (chronological.has(key)) throw new Error(`Duplicate chronological position ${key}`);
    chronological.set(key, value);
  }
  const allSelections = conjugationSelectionsFromMorphology(input.morphology);
  const baseSelections = new Map<number, ConfiguredConjugationSelection>();
  const customSelections = new Map<number, ConfiguredConjugationSelection>();
  for (const [rootSeq, positions] of input.positionsByRoot) {
    const source = allSelections.get(rootSeq);
    if (!source) throw new Error(`Configured root ${rootSeq} is absent from morphology root keys`);
    const selectedPositions = positions.filter(pos => !chronological.has(positionKey(rootSeq, pos)));
    if (selectedPositions.length === 0) continue;
    const destination = input.customRootSeqs.has(rootSeq) ? customSelections : baseSelections;
    destination.set(rootSeq, {
      positions: selectedPositions,
      sourcesByPosition: new Map(selectedPositions.map(pos => {
        return [pos, positionSources(source, rootSeq, pos)];
      }))
    });
  }
  for (const value of input.chronologicalPositions) {
    if (!(input.positionsByRoot.get(value.rootSeq) ?? []).includes(value.pos)) {
      throw new Error(`Chronological position is absent from source map ${value.rootSeq}/${value.pos}`);
    }
  }
  const aliases = [...new Set(input.morphology.rules.map(conjugationPropertyKey))].sort(compareText);
  const aliasByProperty = new Map(aliases.map((key, alias) => [key, alias]));
  const selectedFilters = filters(input);
  const alias = (property: ConjugationProperty): number => {
    const key = conjugationPropertyKey(property);
    const result = aliasByProperty.get(key);
    if (result === undefined) throw new Error(`Scheduled property has no morphology alias ${key}`);
    return result;
  };
  const baseEntries = input.entries.filter(entry => !input.customRootSeqs.has(entry.seq)).sort(compareEntry);
  const customEntries = input.entries.filter(entry => input.customRootSeqs.has(entry.seq)).sort(compareEntry);

  function* phaseRows(
    entries: readonly CanonicalEntry[],
    selections: ReadonlyMap<number, ConfiguredConjugationSelection>,
    phase: ConjugationPhase,
    secondary: boolean,
    lineageCompatibilityHits: Set<string>
  ): Generator<Omit<StreamedScheduledEmission, 'ordinal'>> {
    let phaseOrder = 0;
    for (const entry of entries) {
      const selection = selections.get(entry.seq);
      if (!selection) continue;
      const primary = emitPrimaryConjugations(entry, {
        positions: selection.positions,
        sourcesByPosition: selection.sourcesByPosition
      }).map(emission => markLateSecondarySources(emission, input.firstErrataEvent))
        .map(emission => filterEmission(
          emission,
          selectedFilters,
          input.lineageCompatibility,
          lineageCompatibilityHits
        ))
        .filter((emission): emission is ConjugationEmission => emission !== null);
      const values = secondary
        ? primary.flatMap(emission => emitSecondaryConjugations(emission, {
          enforceSurfaceRoute: true
        }))
          .map(emission => filterEmission(
            emission,
            selectedFilters,
            input.lineageCompatibility,
            lineageCompatibilityHits
          ))
          .filter((emission): emission is ConjugationEmission => emission !== null)
        : primary;
      for (const emission of values) {
        yield {
          phase,
          phaseOrder: phaseOrder++,
          firstAlias: alias(emission.first),
          secondAlias: emission.second === null ? null : alias(emission.second),
          emission
        };
      }
    }
  }

  return {
    *[Symbol.iterator](): Iterator<StreamedScheduledEmission> {
      let ordinal = 0;
      const lineageCompatibilityHits = new Set<string>();
      const groups = [
        phaseRows(
          baseEntries, baseSelections, CONJUGATION_PHASE.basePrimary, false, lineageCompatibilityHits
        ),
        phaseRows(
          baseEntries, baseSelections, CONJUGATION_PHASE.baseSecondary, true, lineageCompatibilityHits
        ),
        phaseRows(
          customEntries, customSelections, CONJUGATION_PHASE.customPrimary, false, lineageCompatibilityHits
        ),
        phaseRows(
          customEntries, customSelections, CONJUGATION_PHASE.customSecondary, true, lineageCompatibilityHits
        )
      ];
      for (const group of groups) {
        for (const row of group) yield { ordinal: ordinal++, ...row };
      }
      let phaseOrder = 0;
      for (const position of [...input.chronologicalPositions].sort((left, right) =>
        left.event - right.event
        || left.rootSeq - right.rootSeq
        || compareText(left.pos, right.pos))) {
        const entry = entriesBySeq.get(position.rootSeq)!;
        const source = allSelections.get(position.rootSeq);
        if (!source) throw new Error(`Chronological root ${position.rootSeq} is absent from morphology root keys`);
        for (const raw of emitConfiguredConjugations(entry, {
          positions: [position.pos],
          sourcesByPosition: new Map([[
            position.pos,
            positionSources(source, position.rootSeq, position.pos)
          ]])
        })) {
          const emission = filterEmission(
            raw,
            selectedFilters,
            input.lineageCompatibility,
            lineageCompatibilityHits
          );
          if (!emission) continue;
          yield {
            ordinal: ordinal++,
            phase: CONJUGATION_PHASE.chronological,
            phaseOrder: phaseOrder++,
            firstAlias: alias(emission.first),
            secondAlias: emission.second === null ? null : alias(emission.second),
            emission
          };
        }
      }
      for (const row of input.lineageCompatibility) {
        if (!lineageCompatibilityHits.has(row.id)) {
          throw new Error(`Conjugation-reading lineage compatibility ${row.id} is stale`);
        }
      }
    }
  };
}
