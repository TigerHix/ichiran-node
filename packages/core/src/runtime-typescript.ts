import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource,
  type AnalyzerAnnotationsGzipDecoder
} from './analyzer-annotations.js';
import {
  ANALYZER_SUPPORT_SECTION_ID,
  AnalyzerSupportReader
} from './analyzer-support.js';
import {
  DetailStoreReader,
  openDetailStore,
  type DetailGzipDecoder,
  type DetailRandomAccessSource
} from './details.js';
import { MORPHOLOGY_SECTION_ID, MorphologyReader } from './morphology.js';
import { openPack } from './pack.js';
import { ROOT_PAYLOAD_SECTION_ID, RootPayloadReader } from './root-payload.js';
import { openSurfaceIndex, SURFACE_INDEX_SECTION_ID } from './surface-index.js';
import {
  PortableAnalyzer,
  type PortableAnalyzeOptions,
  type PortableAnalysisResult
} from './analyzer.js';
import { projectProductAnalysis } from './public-analysis.js';

export interface TypeScriptRuntimeSource {
  readonly hot: Uint8Array;
  readonly details: DetailRandomAccessSource;
  readonly decodeGzip: AnalyzerAnnotationsGzipDecoder & DetailGzipDecoder;
}

/** Frozen TypeScript analyzer retained only as a differential oracle. */
export class TypeScriptOracleRuntime {
  readonly surface;
  readonly roots;
  readonly morphology;
  readonly support;
  readonly annotations;
  readonly #detailSource: DetailRandomAccessSource;
  readonly #decodeGzip: DetailGzipDecoder;
  #detailsPromise: Promise<DetailStoreReader> | null = null;

  private constructor(
    surface: ReturnType<typeof openSurfaceIndex>,
    roots: RootPayloadReader,
    morphology: MorphologyReader,
    support: AnalyzerSupportReader,
    annotations: AnalyzerAnnotationsReader,
    detailSource: DetailRandomAccessSource,
    decodeGzip: DetailGzipDecoder
  ) {
    this.surface = surface;
    this.roots = roots;
    this.morphology = morphology;
    this.support = support;
    this.annotations = annotations;
    this.#detailSource = detailSource;
    this.#decodeGzip = decodeGzip;
  }

  static async open(source: TypeScriptRuntimeSource): Promise<TypeScriptOracleRuntime> {
    const pack = openPack(source.hot);
    const surface = openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID));
    const roots = new RootPayloadReader(pack.getSection(ROOT_PAYLOAD_SECTION_ID));
    const morphology = new MorphologyReader(pack.getSection(MORPHOLOGY_SECTION_ID));
    const support = new AnalyzerSupportReader(pack.getSection(ANALYZER_SUPPORT_SECTION_ID));
    const annotations = await AnalyzerAnnotationsReader.open(
      analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
      source.decodeGzip
    );
    await annotations.preloadAllGenerated();
    return new TypeScriptOracleRuntime(
      surface,
      roots,
      morphology,
      support,
      annotations,
      source.details,
      source.decodeGzip
    );
  }

  analyze(text: string, options: PortableAnalyzeOptions = {}): Promise<PortableAnalysisResult> {
    return this.#run(analyzer => analyzer.analyze(text, options));
  }

  /** Public Rust wire contract projected from the frozen TypeScript oracle. */
  analyzeProduct(
    text: string,
    options: PortableAnalyzeOptions = {}
  ): Promise<PortableAnalysisResult> {
    return this.#run(analyzer => projectProductAnalysis(analyzer.analyze(text, options)));
  }

  romanize(text: string, options: PortableAnalyzeOptions = {}): Promise<string> {
    return this.#run(analyzer => analyzer.romanize(text, options));
  }

  legacy(text: string, options: PortableAnalyzeOptions = {}): Promise<unknown> {
    return this.#run(async analyzer => {
      const result = analyzer.analyze(text, options);
      return analyzer.serializeLegacyDetailed(result, await this.#details());
    });
  }

  async describe(entryIndex: number): Promise<Awaited<ReturnType<DetailStoreReader['entry']>>> {
    return (await this.#details()).entry(entryIndex);
  }

  #details(): Promise<DetailStoreReader> {
    this.#detailsPromise ??= openDetailStore(this.#detailSource, this.#decodeGzip);
    return this.#detailsPromise;
  }

  async #run<T>(operation: (analyzer: PortableAnalyzer) => T | Promise<T>): Promise<T> {
    const annotations = this.annotations.createPreloaded();
    const analyzer = new PortableAnalyzer({
      surface: this.surface,
      roots: this.roots,
      morphology: this.morphology,
      support: this.support,
      annotations
    });
    const loaded = new Set<string>();
    try {
      while (true) {
        try {
          return await operation(analyzer);
        } catch (error) {
          if (!(error instanceof AnalyzerAnnotationNotLoadedError)) throw error;
          const key = `${error.kind}:${error.blockIndex ?? ''}:${error.definitionSeq}`;
          if (loaded.has(key)) {
            throw new Error(`${error.message} remained unavailable after preload`);
          }
          loaded.add(key);
          await annotations.preloadMissing(error);
        }
      }
    } finally {
      annotations.clear();
    }
  }
}
