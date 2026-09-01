import type {
  AnalyzerSupportCollisionSource,
  AnalyzerSupportGeneratedSource
} from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import { compileAnalyzerSupportCollisions } from './analyzer-support-collisions.js';
import {
  compileSourceNativeLookupOrder,
  type LookupClassPrecedence
} from './analyzer-generated-order.js';
import { compileGeneratedRecords } from './analyzer-generated-records.js';
import {
  orderConjugationEmissions,
  type EmissionPrecedence
} from './conjugation-emission-order.js';
import {
  assignPhysicalTargets,
  lexicalPhysicalTarget,
  type PhysicalConjugationResult
} from './conjugation-emissions-physical.js';
import type { ConjugationEmission } from './conjugation-emissions.js';
import type { CanonicalEntry } from './model.js';
import {
  deriveLookupClassPrecedence,
  scheduleSourceNativeConjugations,
  type ChronologicalConjugationPosition,
  type ScheduledConjugationBuild
} from './conjugation-scheduler.js';
import type { ConjugationSuppression } from './conjugation-errata.js';
import type { ConjugationReadingLineageCompatibilityRow } from './compatibility.js';

export interface SourceNativeGeneratedInput {
  readonly entries: readonly CanonicalEntry[];
  readonly emissions: readonly ConjugationEmission[];
  /** Exact root POS declarations selected by MorphologySource.roots. */
  readonly positionsByRoot: ReadonlyMap<number, readonly string[]>;
  /** Dense global phase/declaration order owned by the central scheduler. */
  readonly emissionPrecedence: EmissionPrecedence;
  /** Strict creation order for every ambiguous physical surface class. */
  readonly lookupClassPrecedence: LookupClassPrecedence;
  readonly firstGeneratedSeq: number;
  readonly morphology: CompiledMorphologyArtifact;
}

export interface SourceNativeGeneratedBuild {
  readonly orderedEmissions: readonly ConjugationEmission[];
  readonly physical: PhysicalConjugationResult;
  readonly collisions: readonly AnalyzerSupportCollisionSource[];
  readonly generated: AnalyzerSupportGeneratedSource;
  readonly schedule?: ScheduledConjugationBuild;
}

export interface ScheduledSourceNativeGeneratedInput {
  readonly entries: readonly CanonicalEntry[];
  readonly positionsByRoot: ReadonlyMap<number, readonly string[]>;
  readonly customRootSeqs: ReadonlySet<number>;
  readonly firstErrataEvent: number;
  readonly chronologicalPositions: readonly ChronologicalConjugationPosition[];
  readonly suppressions: readonly ConjugationSuppression[];
  readonly lineageCompatibility: readonly ConjugationReadingLineageCompatibilityRow[];
  readonly firstGeneratedSeq: number;
  readonly morphology: CompiledMorphologyArtifact;
}

function validateConfiguredPositions(
  emissions: readonly ConjugationEmission[],
  positionsByRoot: ReadonlyMap<number, readonly string[]>
): void {
  for (const emission of emissions) {
    const configured = positionsByRoot.get(emission.rootSeq) ?? [];
    if (!configured.includes(emission.first.pos)) {
      throw new Error(
        `Emission root ${emission.rootSeq} uses unconfigured position ${emission.first.pos}`
      );
    }
  }
}

/**
 * Complete PostgreSQL-free generated/analyzer-support projection. Important
 * mutation order is supplied explicitly; this function owns only validation,
 * physical allocation, semantic overlay facts, collision facts, and encoding
 * input assembly.
 */
export function compileSourceNativeGeneratedInput(
  input: SourceNativeGeneratedInput
): SourceNativeGeneratedBuild {
  validateConfiguredPositions(input.emissions, input.positionsByRoot);
  const orderedEmissions = orderConjugationEmissions(
    input.emissions,
    input.emissionPrecedence
  );
  const physical = assignPhysicalTargets(
    orderedEmissions,
    input.entries.map(lexicalPhysicalTarget),
    input.firstGeneratedSeq
  );
  const records = compileGeneratedRecords(
    input.entries,
    orderedEmissions,
    physical,
    input.morphology,
    input.emissionPrecedence
  );
  const order = compileSourceNativeLookupOrder(
    input.entries,
    records.occurrences,
    input.morphology,
    records.ruleAliases,
    records.aliasCount,
    input.lookupClassPrecedence
  );
  const collisions = compileAnalyzerSupportCollisions(
    input.entries,
    orderedEmissions,
    physical,
    input.morphology
  );
  const { occurrences: _occurrences, ...generatedRecords } = records;
  return {
    orderedEmissions,
    physical,
    collisions,
    generated: { ...generatedRecords, ...order }
  };
}

/** Release orchestration with the permanent M2 phase scheduler and precedence. */
export function compileScheduledSourceNativeGeneratedInput(
  input: ScheduledSourceNativeGeneratedInput
): SourceNativeGeneratedBuild {
  const schedule = scheduleSourceNativeConjugations({
    entries: input.entries,
    positionsByRoot: input.positionsByRoot,
    customRootSeqs: input.customRootSeqs,
    firstErrataEvent: input.firstErrataEvent,
    chronologicalPositions: input.chronologicalPositions,
    suppressions: input.suppressions,
    lineageCompatibility: input.lineageCompatibility,
    morphology: input.morphology
  });
  const physical = assignPhysicalTargets(
    schedule.emissions,
    input.entries.map(lexicalPhysicalTarget),
    input.firstGeneratedSeq
  );
  const records = compileGeneratedRecords(
    input.entries,
    schedule.emissions,
    physical,
    input.morphology,
    schedule.precedence
  );
  const lookupClassPrecedence = deriveLookupClassPrecedence({
    entries: input.entries,
    customRootSeqs: input.customRootSeqs,
    firstErrataEvent: input.firstErrataEvent,
    schedule,
    physical,
    occurrences: records.occurrences
  });
  const order = compileSourceNativeLookupOrder(
    input.entries,
    records.occurrences,
    input.morphology,
    records.ruleAliases,
    records.aliasCount,
    lookupClassPrecedence
  );
  const collisions = compileAnalyzerSupportCollisions(
    input.entries,
    schedule.emissions,
    physical,
    input.morphology
  );
  const { occurrences: _occurrences, ...generatedRecords } = records;
  return {
    orderedEmissions: schedule.emissions,
    physical,
    collisions,
    generated: { ...generatedRecords, ...order },
    schedule
  };
}
