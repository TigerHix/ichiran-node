import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import type { ConjugationSuppression } from './conjugation-errata.js';
import {
  lookupClassKey,
  type LookupClassPrecedence
} from './analyzer-generated-order.js';
import type { GeneratedLookupOccurrence } from './analyzer-generated-records.js';
import {
  denseEmissionPrecedence,
  conjugationSelectionsFromMorphology,
  emitConfiguredConjugations,
  type ConfiguredConjugationSelection,
  type EmissionPrecedence
} from './conjugation-emission-order.js';
import {
  conjugationEmissionKey,
  emitPrimaryConjugations,
  emitSecondaryConjugations,
  type ConjugationEmission,
  type EmissionForm
} from './conjugation-emissions.js';
import type {
  PhysicalConjugationResult,
  PhysicalTarget
} from './conjugation-emissions-physical.js';
import {
  omitsConjugationReadingLineage,
  type ConjugationReadingLineageCompatibilityRow
} from './compatibility.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';

export type ConjugationSchedulePhase =
  | 'base-primary'
  | 'base-secondary'
  | 'custom-primary'
  | 'custom-secondary'
  | 'chronological';

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

export interface ScheduledConjugationBuild {
  readonly emissions: readonly ConjugationEmission[];
  readonly precedence: EmissionPrecedence;
  readonly creationByEmission: ReadonlyMap<string, readonly [phase: number, order: number]>;
  readonly counts: {
    readonly basePrimary: number;
    readonly baseSecondary: number;
    readonly customPrimary: number;
    readonly customSecondary: number;
    readonly chronological: number;
    readonly appliedSuppressions: number;
    readonly ghostSuppressions: number;
    readonly appliedTombstones: number;
    readonly unmatchedTombstones: number;
  };
}

export interface StreamedScheduledEmission {
  readonly ordinal: number;
  readonly phase: number;
  readonly phaseOrder: number;
  readonly firstAlias: number;
  readonly secondAlias: number | null;
  readonly emission: ConjugationEmission;
}

interface ScheduledEmission {
  readonly emission: ConjugationEmission;
  readonly phase: number;
  readonly order: number;
}

interface Filter {
  readonly rootSeq: number;
  readonly route: 'kana' | 'kanji';
  readonly surface: string;
  readonly sourceText: string | null;
  readonly first: ConjugationProperty;
  readonly second: ConjugationProperty | null;
}

interface Creation {
  readonly key: string;
  readonly tuple: readonly number[];
}

const PHASE = {
  baseDirect: 0,
  basePrimary: 1,
  baseSecondary: 2,
  customDirect: 3,
  customPrimary: 4,
  customSecondary: 5,
  chronological: 6
} as const;

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function sameProperty(left: ConjugationProperty, right: ConjugationProperty): boolean {
  return left.pos === right.pos
    && left.type === right.type
    && left.negative === right.negative
    && left.formal === right.formal;
}

function matches(filter: Filter, emission: ConjugationEmission, form: EmissionForm): boolean {
  return filter.rootSeq === emission.rootSeq
    && filter.route === form.route
    && filter.surface === form.surface
    && (filter.sourceText === null || filter.sourceText === form.sourceText)
    && sameProperty(filter.first, emission.first)
    && (filter.second === null
      ? emission.second === null
      : emission.second !== null && sameProperty(filter.second, emission.second));
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

function filters(input: ConjugationSchedulerInput): {
  readonly values: readonly Filter[];
  readonly suppressionCount: number;
} {
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
  return { values: [...suppressions, ...tombstones], suppressionCount: suppressions.length };
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

function scheduleGroup(
  entries: readonly CanonicalEntry[],
  selections: ReadonlyMap<number, ConfiguredConjugationSelection>,
  primaryPhase: number,
  secondaryPhase: number,
  firstErrataEvent: number
): { readonly primary: ScheduledEmission[]; readonly secondary: ScheduledEmission[] } {
  const primary: ScheduledEmission[] = [];
  for (const entry of [...entries].sort(compareEntry)) {
    const selection = selections.get(entry.seq);
    if (!selection) continue;
    const emitted = emitPrimaryConjugations(entry, {
      positions: selection.positions,
      sourcesByPosition: selection.sourcesByPosition
    }).map(emission => markLateSecondarySources(emission, firstErrataEvent));
    for (const emission of emitted) {
      primary.push({ emission, phase: primaryPhase, order: primary.length });
    }
  }
  const secondary: ScheduledEmission[] = [];
  for (const row of primary) {
    for (const emission of emitSecondaryConjugations(row.emission, { enforceSurfaceRoute: true })) {
      secondary.push({ emission, phase: secondaryPhase, order: secondary.length });
    }
  }
  return { primary, secondary };
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

function applyFilters(
  scheduled: readonly ScheduledEmission[],
  values: readonly Filter[]
): { readonly rows: ScheduledEmission[]; readonly hits: Uint32Array } {
  const hits = new Uint32Array(values.length);
  const rows = scheduled.map(row => {
    const forms = row.emission.forms.filter(form => {
      let keep = true;
      values.forEach((filter, index) => {
        if (!matches(filter, row.emission, form)) return;
        hits[index]++;
        keep = false;
      });
      return keep;
    });
    const emission = forms.length === row.emission.forms.length
      ? row.emission : { ...row.emission, forms };
    return { ...row, emission };
  });
  return { rows, hits };
}

function filterEmission(
  emission: ConjugationEmission,
  values: readonly Filter[],
  lineageCompatibility: readonly ConjugationReadingLineageCompatibilityRow[]
): ConjugationEmission | null {
  const omittedLineage = (form: EmissionForm): boolean => lineageCompatibility.some(row =>
    omitsConjugationReadingLineage(row, {
      rootSeq: emission.rootSeq,
      route: form.route,
      sourceText: form.sourceText,
      firstRule: form.firstRule,
      secondRule: form.secondRule
    }));
  const physicalForms = emission.physicalForms.filter(form => !omittedLineage(form));
  if (physicalForms.length === 0) return null;
  const forms = emission.forms.filter(form =>
    !values.some(filter => matches(filter, emission, form))
    && !omittedLineage(form));
  return forms.length === emission.forms.length
    && physicalForms.length === emission.physicalForms.length
    ? emission : { ...emission, physicalForms, forms };
}

function semanticPropertyKey(value: ConjugationProperty): string {
  return JSON.stringify([value.pos, value.type, value.negative, value.formal]);
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
  const aliases = [...new Set(input.morphology.rules.map(semanticPropertyKey))].sort(compareText);
  const aliasByProperty = new Map(aliases.map((key, alias) => [key, alias]));
  const selectedFilters = filters(input).values;
  const alias = (property: ConjugationProperty): number => {
    const result = aliasByProperty.get(semanticPropertyKey(property));
    if (result === undefined) throw new Error(`Scheduled property has no morphology alias ${semanticPropertyKey(property)}`);
    return result;
  };
  const baseEntries = input.entries.filter(entry => !input.customRootSeqs.has(entry.seq)).sort(compareEntry);
  const customEntries = input.entries.filter(entry => input.customRootSeqs.has(entry.seq)).sort(compareEntry);

  function* phaseRows(
    entries: readonly CanonicalEntry[],
    selections: ReadonlyMap<number, ConfiguredConjugationSelection>,
    phase: number,
    secondary: boolean
  ): Generator<Omit<StreamedScheduledEmission, 'ordinal'>> {
    let phaseOrder = 0;
    for (const entry of entries) {
      const selection = selections.get(entry.seq);
      if (!selection) continue;
      const primary = emitPrimaryConjugations(entry, {
        positions: selection.positions,
        sourcesByPosition: selection.sourcesByPosition
      }).map(emission => markLateSecondarySources(emission, input.firstErrataEvent))
        .map(emission => filterEmission(emission, selectedFilters, input.lineageCompatibility))
        .filter((emission): emission is ConjugationEmission => emission !== null);
      const values = secondary
        ? primary.flatMap(emission => emitSecondaryConjugations(emission, {
          enforceSurfaceRoute: true
        }))
          .map(emission => filterEmission(emission, selectedFilters, input.lineageCompatibility))
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
      const groups = [
        phaseRows(baseEntries, baseSelections, PHASE.basePrimary, false),
        phaseRows(baseEntries, baseSelections, PHASE.baseSecondary, true),
        phaseRows(customEntries, customSelections, PHASE.customPrimary, false),
        phaseRows(customEntries, customSelections, PHASE.customSecondary, true)
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
          const emission = filterEmission(raw, selectedFilters, input.lineageCompatibility);
          if (!emission) continue;
          yield {
            ordinal: ordinal++,
            phase: PHASE.chronological,
            phaseOrder: phaseOrder++,
            firstAlias: alias(emission.first),
            secondAlias: emission.second === null ? null : alias(emission.second),
            emission
          };
        }
      }
    }
  };
}

/** Build the single source-native global emission schedule recorded by M2. */
export function scheduleSourceNativeConjugations(
  input: ConjugationSchedulerInput
): ScheduledConjugationBuild {
  const entriesBySeq = new Map(input.entries.map(entry => [entry.seq, entry]));
  if (entriesBySeq.size !== input.entries.length) throw new Error('Canonical entries contain duplicate ids');
  const chronological = new Map<string, ChronologicalConjugationPosition>();
  for (const value of input.chronologicalPositions) {
    if (!entriesBySeq.has(value.rootSeq)) throw new Error(`Chronological position has missing root ${value.rootSeq}`);
    const key = positionKey(value.rootSeq, value.pos);
    if (chronological.has(key)) throw new Error(`Duplicate chronological position ${key}`);
    chronological.set(key, value);
  }
  const basePositions = new Map<number, string[]>();
  const customPositions = new Map<number, string[]>();
  const allSelections = conjugationSelectionsFromMorphology(input.morphology);
  for (const [rootSeq, positions] of input.positionsByRoot) {
    if (!entriesBySeq.has(rootSeq)) throw new Error(`Position map has missing root ${rootSeq}`);
    for (const pos of positions) {
      if (chronological.has(positionKey(rootSeq, pos))) continue;
      const destination = input.customRootSeqs.has(rootSeq) ? customPositions : basePositions;
      const values = destination.get(rootSeq) ?? [];
      if (!values.includes(pos)) values.push(pos);
      destination.set(rootSeq, values);
    }
  }
  for (const value of input.chronologicalPositions) {
    if (!(input.positionsByRoot.get(value.rootSeq) ?? []).includes(value.pos)) {
      throw new Error(`Chronological position is absent from source map ${value.rootSeq}/${value.pos}`);
    }
  }

  const baseEntries = input.entries.filter(entry => !input.customRootSeqs.has(entry.seq));
  const customEntries = input.entries.filter(entry => input.customRootSeqs.has(entry.seq));
  const selected = (
    positions: ReadonlyMap<number, readonly string[]>
  ): ReadonlyMap<number, ConfiguredConjugationSelection> => new Map(
    [...positions].map(([seq, values]) => {
      const source = allSelections.get(seq);
      if (!source) throw new Error(`Configured root ${seq} is absent from morphology root keys`);
      return [seq, {
        positions: values,
        sourcesByPosition: new Map(values.map(pos => {
          return [pos, positionSources(source, seq, pos)];
        }))
      }];
    })
  );
  const base = scheduleGroup(
    baseEntries,
    selected(basePositions),
    PHASE.basePrimary,
    PHASE.baseSecondary,
    input.firstErrataEvent
  );
  const custom = scheduleGroup(
    customEntries,
    selected(customPositions),
    PHASE.customPrimary,
    PHASE.customSecondary,
    input.firstErrataEvent
  );
  const chronologicalRows: ScheduledEmission[] = [];
  for (const position of [...input.chronologicalPositions].sort((left, right) =>
    left.event - right.event
    || left.rootSeq - right.rootSeq
    || compareText(left.pos, right.pos))) {
    const entry = entriesBySeq.get(position.rootSeq)!;
    const source = allSelections.get(position.rootSeq);
    if (!source) throw new Error(`Chronological root ${position.rootSeq} is absent from morphology root keys`);
    const values = emitConfiguredConjugations(entry, {
      positions: [position.pos],
      sourcesByPosition: new Map([[
        position.pos,
        positionSources(source, position.rootSeq, position.pos)
      ]])
    });
    for (const emission of values) {
      chronologicalRows.push({
        emission,
        phase: PHASE.chronological,
        order: chronologicalRows.length
      });
    }
  }
  const raw = [
    ...base.primary,
    ...base.secondary,
    ...custom.primary,
    ...custom.secondary,
    ...chronologicalRows
  ];
  const selectedFilters = filters(input);
  const filtered = applyFilters(raw, selectedFilters.values);
  const compatible = filtered.rows.flatMap(row => {
    const emission = filterEmission(row.emission, [], input.lineageCompatibility);
    return emission === null ? [] : [{ ...row, emission }];
  });
  const emissions = compatible.map(row => row.emission);
  const precedence = denseEmissionPrecedence(emissions);
  const creationByEmission = new Map(compatible.map(row => [
    conjugationEmissionKey(row.emission),
    [row.phase, row.order] as const
  ]));
  const suppressionHits = filtered.hits.slice(0, selectedFilters.suppressionCount);
  const tombstoneHits = filtered.hits.slice(selectedFilters.suppressionCount);
  return {
    emissions,
    precedence,
    creationByEmission,
    counts: {
      basePrimary: base.primary.length,
      baseSecondary: base.secondary.length,
      customPrimary: custom.primary.length,
      customSecondary: custom.secondary.length,
      chronological: chronologicalRows.length,
      appliedSuppressions: suppressionHits.filter(value => value > 0).length,
      ghostSuppressions: suppressionHits.filter(value => value === 0).length,
      appliedTombstones: tombstoneHits.filter(value => value > 0).length,
      unmatchedTombstones: tombstoneHits.filter(value => value === 0).length
    }
  };
}

function compareTuple(left: readonly number[], right: readonly number[]): number {
  const length = Math.max(left.length, right.length);
  for (let index = 0; index < length; index++) {
    const difference = (left[index] ?? -1) - (right[index] ?? -1);
    if (difference !== 0) return difference;
  }
  return 0;
}

function directPhase(
  entry: CanonicalEntry,
  event: number,
  customRootSeqs: ReadonlySet<number>,
  firstErrataEvent: number
): number {
  if (event >= firstErrataEvent) return PHASE.chronological;
  return customRootSeqs.has(entry.seq) ? PHASE.customDirect : PHASE.baseDirect;
}

/**
 * Derive strict class creation precedence from lexical form events, generated
 * target allocation, and chronological patch additions. No PostgreSQL heap
 * identity or sequence-id ordering participates.
 */
export function deriveLookupClassPrecedence(input: {
  readonly entries: readonly CanonicalEntry[];
  readonly customRootSeqs: ReadonlySet<number>;
  readonly firstErrataEvent: number;
  readonly schedule: ScheduledConjugationBuild;
  readonly physical: PhysicalConjugationResult;
  readonly occurrences: readonly GeneratedLookupOccurrence[];
}): LookupClassPrecedence {
  const creations = new Map<string, Creation>();
  const add = (key: string, tuple: readonly number[]): void => {
    const prior = creations.get(key);
    if (!prior || compareTuple(prior.tuple, tuple) < 0) creations.set(key, { key, tuple });
  };
  for (const entry of input.entries) {
    for (const form of entry.kana) {
      const phase = directPhase(
        entry,
        form.sourceOrder.event,
        input.customRootSeqs,
        input.firstErrataEvent
      );
      add(lookupClassKey('kana', form.text, entry.seq), [
        phase, form.sourceOrder.event, form.sourceOrder.ordinal, form.ordinal, entry.seq
      ]);
    }
    for (const form of entry.kanji) {
      const phase = directPhase(
        entry,
        form.sourceOrder.event,
        input.customRootSeqs,
        input.firstErrataEvent
      );
      add(lookupClassKey('kanji', form.text, entry.seq), [
        phase, form.sourceOrder.event, form.sourceOrder.ordinal, form.ordinal, entry.seq
      ]);
    }
  }

  const targets = new Map(input.physical.targets.map(target => [target.seq, target]));
  const generatedCreation = new Map<number, readonly [number, number]>();
  for (const binding of input.physical.bindings) {
    const target = targets.get(binding.targetSeq);
    if (!target || target.origin !== 'generated' || generatedCreation.has(target.seq)) continue;
    const creation = input.schedule.creationByEmission.get(binding.emissionKey);
    if (!creation) throw new Error(`Generated target has unscheduled binding ${binding.emissionKey}`);
    generatedCreation.set(target.seq, creation);
  }
  for (const target of input.physical.targets) {
    if (target.origin !== 'generated') continue;
    const creation = generatedCreation.get(target.seq);
    if (!creation) throw new Error(`Generated target ${target.seq} has no creation event`);
    for (const text of target.kana) {
      add(lookupClassKey('kana', text, target.seq), [...creation, target.seq]);
    }
    for (const text of target.kanji) {
      add(lookupClassKey('kanji', text, target.seq), [...creation, target.seq]);
    }
  }
  let patchOrder = 0;
  for (const occurrence of input.occurrences) {
    if (occurrence.kind !== 'patch') continue;
    add(lookupClassKey(occurrence.route, occurrence.surface, occurrence.targetSeq), [
      PHASE.chronological, Number.MAX_SAFE_INTEGER - 1, patchOrder++, occurrence.targetSeq
    ]);
  }

  const ordered = [...creations.values()].sort((left, right) =>
    compareTuple(left.tuple, right.tuple) || compareText(left.key, right.key));
  return new Map(ordered.map((value, order) => [value.key, order]));
}

export function lexicalTargets(entries: readonly CanonicalEntry[]): PhysicalTarget[] {
  return entries.map(entry => ({
    seq: entry.seq,
    kanji: entry.kanji.map(form => form.text),
    kana: entry.kana.map(form => form.text),
    secondaryForms: [
      ...entry.kanji.filter(form => form.conjugatable).map(form => ({
        route: 'kanji' as const, text: form.text, counterpart: form.best
      })),
      ...entry.kana.filter(form => form.conjugatable).map(form => ({
        route: 'kana' as const, text: form.text, counterpart: form.best
      }))
    ],
    conjugatable: [...entry.kanji, ...entry.kana].some(form => form.conjugatable),
    origin: 'lexical'
  }));
}
