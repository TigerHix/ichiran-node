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
  DictionaryReader,
  LexiconStoreReader,
  LocaleGlossStoreReader,
  type DictionaryGzipDecoder,
  type DictionaryRandomAccessSource
} from './dictionary.js';
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
  readonly lexicon: {
    readonly source: DictionaryRandomAccessSource;
    readonly sha256: string;
  };
  readonly locales: Readonly<Record<string, DictionaryRandomAccessSource>>;
  readonly decodeGzip: AnalyzerAnnotationsGzipDecoder & DictionaryGzipDecoder;
}

export interface TypeScriptDictionaryOptions {
  readonly locale?: string;
}

/** Frozen TypeScript analyzer retained only as a differential oracle. */
export class TypeScriptOracleRuntime {
  readonly surface;
  readonly roots;
  readonly morphology;
  readonly support;
  readonly annotations;
  readonly #lexiconSource: TypeScriptRuntimeSource['lexicon'];
  readonly #localeSources: TypeScriptRuntimeSource['locales'];
  readonly #decodeGzip: DictionaryGzipDecoder;
  #lexiconPromise: Promise<LexiconStoreReader> | null = null;
  readonly #localePromises = new Map<string, Promise<LocaleGlossStoreReader>>();

  private constructor(
    surface: ReturnType<typeof openSurfaceIndex>,
    roots: RootPayloadReader,
    morphology: MorphologyReader,
    support: AnalyzerSupportReader,
    annotations: AnalyzerAnnotationsReader,
    lexiconSource: TypeScriptRuntimeSource['lexicon'],
    localeSources: TypeScriptRuntimeSource['locales'],
    decodeGzip: DictionaryGzipDecoder
  ) {
    this.surface = surface;
    this.roots = roots;
    this.morphology = morphology;
    this.support = support;
    this.annotations = annotations;
    this.#lexiconSource = lexiconSource;
    this.#localeSources = localeSources;
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
      source.lexicon,
      source.locales,
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

  legacy(
    text: string,
    options: PortableAnalyzeOptions & TypeScriptDictionaryOptions = {}
  ): Promise<unknown> {
    return this.#run(async analyzer => {
      const { locale = 'en', ...analyzeOptions } = options;
      const result = analyzer.analyze(text, analyzeOptions);
      return analyzer.serializeLegacyDetailed(result, await this.#dictionary(locale));
    });
  }

  async describe(entryIndex: number, options: TypeScriptDictionaryOptions = {}) {
    return (await this.#dictionary(options.locale ?? 'en')).entry(entryIndex);
  }

  async #dictionary(locale: string): Promise<DictionaryReader> {
    const [lexicon, selected, english] = await Promise.all([
      this.#lexicon(),
      this.#locale(locale),
      this.#locale('en')
    ]);
    return new DictionaryReader(lexicon, selected, english);
  }

  #lexicon(): Promise<LexiconStoreReader> {
    this.#lexiconPromise ??= LexiconStoreReader.open(
      this.#lexiconSource.source,
      this.#decodeGzip
    );
    return this.#lexiconPromise;
  }

  #locale(locale: string): Promise<LocaleGlossStoreReader> {
    const source = this.#localeSources[locale];
    if (!source) return Promise.reject(new Error(`Dictionary locale is not installed: ${locale}`));
    let promise = this.#localePromises.get(locale);
    if (!promise) {
      promise = this.#lexicon().then(lexicon => LocaleGlossStoreReader.open(
        source,
        this.#decodeGzip,
        { locale, lexiconSha256: this.#lexiconSource.sha256, entryCount: lexicon.manifest.entryCount }
      ));
      this.#localePromises.set(locale, promise);
    }
    return promise;
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
