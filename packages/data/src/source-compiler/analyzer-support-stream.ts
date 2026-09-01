import type { AnalyzerSupportSource } from '../browser-pack/analyzer-support.js';
import type { CompiledMorphologyArtifact } from '../browser-pack/morphology-format.js';
import {
  compileBoundedGeneratedProjection
} from './analyzer-generated-stream.js';
import {
  compileAnalyzerSupportAnnotations
} from './analyzer-support-annotations.js';
import { compileCanonicalCounters } from './analyzer-support-counters.js';
import {
  createBoundedSourceNativeSplitPartResolver
} from './analyzer-support-split-resolver.js';
import { compileBoundedCanonicalSuffixes } from './analyzer-support-suffixes-stream.js';
import type { GeneratedProjectionStreamResult } from './generated-projection-stream.js';
import type { KanjidicHintReadings } from './kanjidic-hints.js';
import type { CanonicalEntry } from './model.js';

export interface BoundedSourceNativeAnalyzerSupportInput {
  readonly projection: GeneratedProjectionStreamResult;
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: CompiledMorphologyArtifact;
  readonly temporaryDirectory: string;
  readonly kanjidicReadings: KanjidicHintReadings;
  /** Source-owned lexical creation phase inputs, shared with the scheduler. */
  readonly customRootSeqs: ReadonlySet<number>;
  readonly firstErrataEvent: number;
  readonly maxOccurrenceChunkRows?: number;
}

export interface BoundedAnalyzerSupportSummary {
  readonly semanticPaths: number;
  readonly semanticPathSha256: string;
  readonly generatedRecords: number;
  readonly generatedProjectionSha256: string;
  readonly occurrenceRows: number;
  readonly occurrenceSurfaces: number;
  readonly maxOccurrenceSurfaceRows: number;
  readonly collisions: number;
  readonly suffixes: number;
  readonly suffixClasses: number;
  readonly counters: number;
  readonly splits: number;
  readonly hints: number;
}

export interface BoundedSourceNativeAnalyzerSupportResult {
  readonly support: AnalyzerSupportSource;
  readonly summary: BoundedAnalyzerSupportSummary;
}

/**
 * Reduce a complete scheduled projection to the compiler-owned analyzer facts.
 * The caller owns the spool files and keeps them alive for surface-index output.
 */
export function compileBoundedSourceNativeAnalyzerSupport(
  input: BoundedSourceNativeAnalyzerSupportInput
): BoundedSourceNativeAnalyzerSupportResult {
  const generated = compileBoundedGeneratedProjection(input);
  const suffix = compileBoundedCanonicalSuffixes(input);
  const counters = compileCanonicalCounters(input.entries);
  const partResolver = createBoundedSourceNativeSplitPartResolver(input);
  const annotations = compileAnalyzerSupportAnnotations({
    entries: input.entries,
    morphology: input.morphology,
    collisions: generated.collisions,
    partResolver,
    kanjidicReadings: input.kanjidicReadings
  });
  const support: AnalyzerSupportSource = {
    suffixes: suffix.suffixes,
    suffixClasses: suffix.suffixClasses,
    counters,
    splits: annotations.splits,
    hints: annotations.hints,
    collisions: generated.collisions,
    generated: generated.generated
  };
  return {
    support,
    summary: {
      semanticPaths: generated.generated.semanticPaths,
      semanticPathSha256: generated.semanticPathSha256,
      generatedRecords: generated.generated.records.length,
      generatedProjectionSha256: generated.generated.projectionSha256,
      occurrenceRows: generated.occurrenceRows,
      occurrenceSurfaces: generated.occurrenceSurfaces,
      maxOccurrenceSurfaceRows: generated.maxOccurrenceSurfaceRows,
      collisions: generated.collisions.length,
      suffixes: suffix.suffixes.length,
      suffixClasses: suffix.suffixClasses.length,
      counters: counters.length,
      splits: annotations.splits.length,
      hints: annotations.hints.length
    }
  };
}
