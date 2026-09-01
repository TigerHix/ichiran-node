import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import { isRootPayloadKanaSurface } from '../browser-pack/root-payload.js';
import {
  GeneratedProjectionSpoolWriter,
  type GeneratedProjectionSpoolSummary
} from './generated-projection-spool.js';
import {
  StreamingPhysicalTargetAllocator,
  lexicalPhysicalTarget,
  type PhysicalTarget
} from './conjugation-emissions-physical.js';
import type {
  ConjugationEmission,
  EmissionForm,
  EmissionRule
} from './conjugation-emissions.js';
import {
  iterateScheduledConjugations,
  type ConjugationSchedulerInput
} from './conjugation-scheduler.js';
import type { CanonicalEntry, ConjugationProperty } from './model.js';
import type { RegeneratedConjugationLineage } from './conjugation-errata.js';
import { replayConjugationReading } from './conjugation-reading-replay.js';
import type { PhysicalTargetOrderCompatibilityRow } from './compatibility.js';

const PHASE_STRIDE = 100_000_000;

export interface GeneratedProjectionStreamInput extends ConjugationSchedulerInput {
  /** Chronological addConjReading declarations, including source-only CSR terminals. */
  readonly regeneratedLineages: readonly RegeneratedConjugationLineage[];
  readonly physicalTargetOrderCompatibility: readonly PhysicalTargetOrderCompatibilityRow[];
  readonly firstGeneratedSeq: number;
  readonly pathsPath: string;
  readonly occurrencesPath: string;
}

export interface GeneratedProjectionStreamResult {
  readonly pathsPath: string;
  readonly occurrencesPath: string;
  readonly spool: GeneratedProjectionSpoolSummary;
  readonly targets: readonly PhysicalTarget[];
  readonly ruleAliases: readonly number[];
  readonly aliasProperties: readonly ConjugationProperty[];
  readonly phases: Readonly<Record<number, number>>;
  readonly patches: number;
  readonly regeneratedTargetForms: number;
}

interface DeferredReadingReplay {
  readonly pathOrdinal: number;
  readonly targetSeq: number;
  readonly rootSeq: number;
  readonly emission: ConjugationEmission;
}

interface DirectCreation {
  readonly key: string;
  readonly phase: number;
  readonly tuple: readonly [event: number, source: number, form: number, seq: number, route: number, text: string];
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function semanticPropertyKey(value: ConjugationProperty): string {
  return JSON.stringify([value.pos, value.type, value.negative, value.formal]);
}

function ruleKey(value: EmissionRule): string {
  return JSON.stringify([
    value.pos, value.type, value.negative, value.formal, value.order,
    value.stem, value.okuri, value.euphr, value.euphk
  ]);
}

function compiledRuleKey(
  value: CompiledMorphologyArtifact['rules'][number]
): string {
  return JSON.stringify([
    value.pos, value.type, value.negative, value.formal, value.ordinal,
    value.stem, value.okuri, value.euphr, value.euphk
  ]);
}

function emissionRule(
  value: CompiledMorphologyArtifact['rules'][number]
): EmissionRule {
  return {
    pos: value.pos,
    type: value.type,
    negative: value.negative,
    formal: value.formal,
    order: value.ordinal,
    stem: value.stem,
    okuri: value.okuri,
    euphr: value.euphr,
    euphk: value.euphk
  };
}

function patchOnlyEmission(
  rootSeq: number,
  ordinal: number,
  patches: readonly CompiledMorphologyArtifact['patches'][number][],
  morphology: CompiledMorphologyArtifact
): ConjugationEmission {
  const firstRuleId = patches[0]!.firstRule;
  const secondRuleId = patches[0]!.secondRule;
  const firstRule = emissionRule(morphology.rules[firstRuleId]!);
  const secondRule = secondRuleId === null ? null : emissionRule(morphology.rules[secondRuleId]!);
  const forms: EmissionForm[] = patches.map(patch => ({
    route: patch.route,
    surface: patch.surface,
    sourceText: patch.sourceText,
    sourceEvent: Number.MAX_SAFE_INTEGER,
    sourceOrdinal: patch.ord,
    secondaryEligible: false,
    physicalCounterpart: patch.route === 'kana' ? patch.form : patch.reading,
    intermediate: patch.intermediate,
    firstRule,
    secondRule
  }));
  return {
    rootSeq,
    rootEvent: Number.MAX_SAFE_INTEGER,
    stage: secondRule === null ? 'primary' : 'secondary',
    ordinal,
    first: {
      pos: firstRule.pos, type: firstRule.type,
      negative: firstRule.negative, formal: firstRule.formal
    },
    second: secondRule === null ? null : {
      pos: secondRule.pos, type: secondRule.type,
      negative: secondRule.negative, formal: secondRule.formal
    },
    via: null,
    physicalForms: forms,
    forms
  };
}

function pathKey(rootSeq: number, firstAlias: number, secondAlias: number | null): string {
  return `${rootSeq}\u0000${firstAlias}\u0000${secondAlias ?? -1}`;
}

export function generatedLookupClassKey(
  targetSeq: number,
  route: 'kana' | 'kanji',
  surface: string
): string {
  return `${targetSeq}\u0000${route}\u0000${surface}`;
}

function phasePrecedence(phase: number, order: number): number {
  const value = phase * PHASE_STRIDE + order;
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_fffe) {
    throw new Error(`Generated creation precedence is outside uint32: ${phase}/${order}`);
  }
  return value;
}

function directPhase(
  entry: CanonicalEntry,
  event: number,
  customRootSeqs: ReadonlySet<number>,
  firstErrataEvent: number
): number {
  if (event >= firstErrataEvent) return 6;
  return customRootSeqs.has(entry.seq) ? 3 : 0;
}

function compareDirect(left: DirectCreation, right: DirectCreation): number {
  if (left.phase !== right.phase) return left.phase - right.phase;
  const difference = left.tuple[0] - right.tuple[0]
    || left.tuple[1] - right.tuple[1]
    || left.tuple[2] - right.tuple[2]
    || left.tuple[3] - right.tuple[3]
    || left.tuple[4] - right.tuple[4];
  if (difference !== 0) return difference;
  return compareText(left.tuple[5], right.tuple[5]);
}

export function directGeneratedLookupClassPrecedence(
  input: Pick<GeneratedProjectionStreamInput, 'entries' | 'customRootSeqs' | 'firstErrataEvent'>
): ReadonlyMap<string, number> {
  const rows: DirectCreation[] = [];
  for (const entry of input.entries) {
    for (const [route, forms] of [['kana', entry.kana], ['kanji', entry.kanji]] as const) {
      for (const form of forms) {
        if (isRootPayloadKanaSurface(form.text) !== (route === 'kana')) continue;
        const phase = directPhase(
          entry,
          form.sourceOrder.event,
          input.customRootSeqs,
          input.firstErrataEvent
        );
        rows.push({
          key: generatedLookupClassKey(entry.seq, route, form.text),
          phase,
          tuple: [
            form.sourceOrder.event,
            form.sourceOrder.ordinal,
            form.ordinal,
            entry.seq,
            route === 'kana' ? 0 : 1,
            form.text
          ]
        });
      }
    }
  }
  rows.sort(compareDirect);
  const next = new Map<number, number>();
  const result = new Map<string, number>();
  for (const row of rows) {
    const order = next.get(row.phase) ?? 0;
    next.set(row.phase, order + 1);
    result.set(row.key, phasePrecedence(row.phase, order));
  }
  return result;
}

function ruleProjection(morphology: CompiledMorphologyArtifact): {
  readonly ruleIds: ReadonlyMap<string, number>;
  readonly aliases: readonly number[];
  readonly properties: readonly ConjugationProperty[];
} {
  const keys = [...new Set(morphology.rules.map(semanticPropertyKey))].sort(compareText);
  const aliasByProperty = new Map(keys.map((key, alias) => [key, alias]));
  const aliases = morphology.rules.map(rule => {
    const alias = aliasByProperty.get(semanticPropertyKey(rule));
    if (alias === undefined) throw new Error('Compiled rule has no semantic alias');
    return alias;
  });
  const properties = keys.map(key => {
    const [pos, type, negative, formal] = JSON.parse(key) as [
      string, number, boolean | null, boolean | null
    ];
    return { pos, type, negative, formal };
  });
  return {
    ruleIds: new Map(morphology.rules.map((rule, id) => [compiledRuleKey(rule), id])),
    aliases,
    properties
  };
}

/**
 * Write the complete scheduled generated relation without retaining emissions
 * or surfaces. The two concrete spool files are consumed by the bounded M6
 * reducers and remain owned by the caller after this function returns.
 */
export function writeScheduledGeneratedProjection(
  input: GeneratedProjectionStreamInput
): GeneratedProjectionStreamResult {
  const rules = ruleProjection(input.morphology);
  const directPrecedence = directGeneratedLookupClassPrecedence(input);
  const lexicalFallback = new Map<string, number>();
  const allocator = new StreamingPhysicalTargetAllocator(
    input.entries.map(lexicalPhysicalTarget),
    input.firstGeneratedSeq,
    input.physicalTargetOrderCompatibility
  );
  const writer = new GeneratedProjectionSpoolWriter(input.pathsPath, input.occurrencesPath);
  const patchPaths = new Map<string, number>();
  const regeneratedReadings = new Map<number, Set<string>>();
  for (const value of input.regeneratedLineages) {
    const readings = regeneratedReadings.get(value.rootSeq) ?? new Set<string>();
    readings.add(value.sourceText);
    regeneratedReadings.set(value.rootSeq, readings);
  }
  const entriesBySeq = new Map(input.entries.map(entry => [entry.seq, entry]));
  const deferredReadingReplays: DeferredReadingReplay[] = [];
  const requiredPatchPaths = new Set(input.morphology.patches.map(patch => pathKey(
    patch.rootSeq,
    rules.aliases[patch.firstRule]!,
    patch.secondRule === null ? null : rules.aliases[patch.secondRule]!
  )));
  const phases: Record<number, number> = {};
  let emissions = 0;
  try {
    for (const row of iterateScheduledConjugations(input)) {
      const creationPrecedence = phasePrecedence(row.phase, row.phaseOrder);
      const emission = allocator.expandSecondary(
        row.emission,
        row.emission.rootSeq,
        row.firstAlias
      );
      const binding = allocator.add({ ...row, emission, creationPrecedence });
      writer.writePath({
        ordinal: row.ordinal,
        rootSeq: emission.rootSeq,
        firstAlias: row.firstAlias,
        secondAlias: row.secondAlias,
        targetSeq: binding.targetSeq,
        viaTargetSeq: binding.viaTargetSeq
      });
      const semanticKey = pathKey(emission.rootSeq, row.firstAlias, row.secondAlias);
      if (requiredPatchPaths.has(semanticKey)) patchPaths.set(semanticKey, row.ordinal);
      if (regeneratedReadings.has(emission.rootSeq)) {
        deferredReadingReplays.push({
          pathOrdinal: row.ordinal,
          targetSeq: binding.targetSeq,
          rootSeq: emission.rootSeq,
          emission
        });
      }
      const installedForms = new Set(emission.forms);
      for (const form of emission.physicalForms) {
        const firstRule = rules.ruleIds.get(ruleKey(form.firstRule));
        const secondRule = form.secondRule === null ? null : rules.ruleIds.get(ruleKey(form.secondRule));
        if (firstRule === undefined || (form.secondRule !== null && secondRule === undefined)) {
          throw new Error(`Emission ${row.ordinal} references an uncompiled rule declaration`);
        }
        const physicalKey = generatedLookupClassKey(binding.targetSeq, form.route, form.surface);
        const knownPrecedence = directPrecedence.get(physicalKey)
          ?? binding.targetCreationPrecedence;
        let precedence: number;
        if (knownPrecedence !== null && knownPrecedence !== undefined) {
          precedence = knownPrecedence;
        } else {
          precedence = lexicalFallback.get(physicalKey) ?? creationPrecedence;
          lexicalFallback.set(physicalKey, precedence);
        }
        writer.writeOccurrence({
          pathOrdinal: row.ordinal,
          precedence,
          firstRule,
          secondRule: secondRule ?? null,
          route: form.route,
          kind: 'emission',
          installed: installedForms.has(form),
          surface: form.surface,
          physicalCounterpart: form.physicalCounterpart
        });
      }
      emissions++;
      phases[row.phase] = (phases[row.phase] ?? 0) + 1;
    }
    let regeneratedTargetForms = 0;
    for (const [replayOrder, replay] of deferredReadingReplays.entries()) {
      const entry = entriesBySeq.get(replay.rootSeq)!;
      for (const reading of regeneratedReadings.get(replay.rootSeq)!) {
        const route = isRootPayloadKanaSurface(reading) ? 'kana' : 'kanji';
        const rootForms = route === 'kana' ? entry.kana : entry.kanji;
        const rootBase = [...rootForms].sort((left, right) =>
          left.ordinal - right.ordinal || compareText(left.text, right.text))[0];
        const target = allocator.target(replay.targetSeq);
        const targetBase = (route === 'kana' ? target.kana : target.kanji)[0];
        if (!rootBase || !targetBase) continue;
        const source = replay.emission.physicalForms.find(form =>
          form.route === route && form.surface === targetBase);
        if (!source) continue;
        const surface = replayConjugationReading(rootBase.text, reading, targetBase);
        if (!allocator.appendChronologicalForm(replay.targetSeq, route, targetBase, surface)) continue;
        const firstRule = rules.ruleIds.get(ruleKey(source.firstRule));
        const secondRule = source.secondRule === null ? null : rules.ruleIds.get(ruleKey(source.secondRule));
        if (firstRule === undefined || (source.secondRule !== null && secondRule === undefined)) {
          throw new Error(`addConjReading replay for root ${replay.rootSeq} has an uncompiled rule`);
        }
        // Preserve the upstream source transformation as a checked semantic
        // fact even though the surface spool does not store source text.
        replayConjugationReading(rootBase.text, reading, source.sourceText);
        writer.writeOccurrence({
          pathOrdinal: replay.pathOrdinal,
          precedence: phasePrecedence(6, 60_000_000 + replayOrder),
          firstRule,
          secondRule: secondRule ?? null,
          route,
          kind: 'emission',
          installed: false,
          surface,
          physicalCounterpart: null
        });
        regeneratedTargetForms++;
      }
    }
    const patchGroups = new Map<string, CompiledMorphologyArtifact['patches'][number][]>();
    for (const patch of input.morphology.patches) {
      const firstAlias = rules.aliases[patch.firstRule];
      const secondAlias = patch.secondRule === null ? null : rules.aliases[patch.secondRule];
      if (firstAlias === undefined || (patch.secondRule !== null && secondAlias === undefined)) {
        throw new Error(`Manual patch for root ${patch.rootSeq} has no rule alias`);
      }
      const key = pathKey(patch.rootSeq, firstAlias, secondAlias ?? null);
      if (patchPaths.has(key)) continue;
      const values = patchGroups.get(key) ?? [];
      values.push(patch);
      patchGroups.set(key, values);
    }
    const orderedPatchGroups = [...patchGroups].sort((left, right) =>
      Number(left[1][0]!.secondRule !== null) - Number(right[1][0]!.secondRule !== null));
    for (const [patchOrder, [key, patches]] of orderedPatchGroups.entries()) {
      const firstAlias = rules.aliases[patches[0]!.firstRule]!;
      const secondAlias = patches[0]!.secondRule === null
        ? null : rules.aliases[patches[0]!.secondRule]!;
      const emission = patchOnlyEmission(patches[0]!.rootSeq, patchOrder, patches, input.morphology);
      const creationPrecedence = phasePrecedence(6, 40_000_000 + patchOrder);
      const binding = allocator.add({
        ordinal: emissions,
        firstAlias,
        secondAlias,
        creationPrecedence,
        emission
      });
      writer.writePath({
        ordinal: emissions,
        rootSeq: emission.rootSeq,
        firstAlias,
        secondAlias,
        targetSeq: binding.targetSeq,
        viaTargetSeq: binding.viaTargetSeq
      });
      patchPaths.set(key, emissions);
      emissions++;
      phases[6] = (phases[6] ?? 0) + 1;
    }
    if (emissions === 0) throw new Error('Scheduled generated projection emitted no paths');
    for (const rootSeq of regeneratedReadings.keys()) {
      if (!deferredReadingReplays.some(value => value.rootSeq === rootSeq)) {
        throw new Error(`Chronological addConjReading root has no conjugation path: ${rootSeq}`);
      }
    }
    const patchKeys = new Set<string>();
    for (const [patchOrder, patch] of input.morphology.patches.entries()) {
      const firstAlias = rules.aliases[patch.firstRule];
      const secondAlias = patch.secondRule === null ? null : rules.aliases[patch.secondRule];
      if (firstAlias === undefined || (patch.secondRule !== null && secondAlias === undefined)) {
        throw new Error(`Manual patch for root ${patch.rootSeq} has no rule alias`);
      }
      const ordinal = patchPaths.get(pathKey(patch.rootSeq, firstAlias, secondAlias ?? null));
      if (ordinal === undefined) {
        throw new Error(`Manual patch for root ${patch.rootSeq} has no scheduled semantic path`);
      }
      const duplicateKey = JSON.stringify([
        patch.route, patch.surface, patch.rootSeq, patch.firstRule, patch.secondRule
      ]);
      if (patchKeys.has(duplicateKey)) continue;
      patchKeys.add(duplicateKey);
      writer.writeOccurrence({
        pathOrdinal: ordinal,
        precedence: phasePrecedence(6, 50_000_000 + patchOrder),
        firstRule: patch.firstRule,
        secondRule: patch.secondRule,
        route: patch.route,
        kind: 'patch',
        installed: true,
        surface: patch.surface,
        physicalCounterpart: patch.route === 'kana' ? patch.form : patch.reading
      });
    }
    const spool = writer.close();
    if (spool.paths !== emissions) {
      throw new Error(`Generated spool wrote ${spool.paths}/${emissions} paths`);
    }
    return {
      pathsPath: input.pathsPath,
      occurrencesPath: input.occurrencesPath,
      spool,
      targets: allocator.finish(),
      ruleAliases: rules.aliases,
      aliasProperties: rules.properties,
      phases,
      patches: patchKeys.size,
      regeneratedTargetForms
    };
  } catch (error) {
    writer.abort();
    throw error;
  }
}
