import type {
  AnalyzerGeneratedFacts
} from './analyzer-annotations.js';
import type { AnalyzerSupportRoute, AnalyzerSupportSplit, AnalyzerSupportReader } from './analyzer-support.js';
import type { MorphologyReader } from './morphology.js';
import type { RootPayloadReader } from './root-payload.js';
import type { SurfaceIndex } from './surface-index.js';

export interface PortableAnalyzerAnnotations {
  split(
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string,
    kind?: 'split' | 'segsplit'
  ): AnalyzerSupportSplit | null;
  hint(
    definitionSeq: number,
    route: AnalyzerSupportRoute,
    surface: string,
    reading: string
  ): string | null;
  generated?(
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number]
  ): AnalyzerGeneratedFacts | null;
  lookupOrder?(
    route: AnalyzerSupportRoute,
    surface: string,
    rootSeq: number,
    aliases: readonly [number] | readonly [number, number] | null
  ): number | null;
}

export interface PortableAnalyzerSource {
  readonly surface: SurfaceIndex;
  readonly roots: RootPayloadReader;
  readonly morphology: MorphologyReader;
  readonly support: AnalyzerSupportReader;
  /** Defaults to the hot support reader in tests and legacy packs. */
  readonly annotations?: PortableAnalyzerAnnotations;
}
