import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  ANALYZER_SUPPORT_SECTION_ID,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsReader,
  AnalyzerSupportReader,
  DetailStoreReader,
  MORPHOLOGY_SECTION_ID,
  MorphologyReader,
  PortableAnalyzer,
  analyzerAnnotationsMemorySource,
  openDetailStore,
  openPack,
  openSurfaceIndex,
  ROOT_PAYLOAD_SECTION_ID,
  RootPayloadReader,
  SURFACE_INDEX_SECTION_ID,
  type PortableAnalyzeOptions,
  type PortableAnalysisResult,
  type DetailRandomAccessSource
} from '@ichiran/portable';
import type { InstalledFiles } from './install.js';

async function decodeGzip(
  compressed: Uint8Array,
  expectedByteLength: number
): Promise<Uint8Array> {
  const owned = new Uint8Array(compressed.byteLength);
  owned.set(compressed);
  const stream = new Blob([owned.buffer])
    .stream()
    .pipeThrough(new DecompressionStream('gzip'));
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer());
  if (bytes.byteLength !== expectedByteLength) {
    throw new Error(
      `Decoded detail block has ${bytes.byteLength} bytes; expected ${expectedByteLength}`
    );
  }
  return bytes;
}

async function detailSource(
  handle: FileSystemFileHandle
): Promise<DetailRandomAccessSource> {
  const file = await handle.getFile();
  return {
    byteLength: file.size,
    async read(offset, byteLength) {
      return new Uint8Array(await file.slice(offset, offset + byteLength).arrayBuffer());
    }
  };
}

/** Immutable readers owned by the analyzer Worker for one installed pack. */
export class AnalyzerRuntime {
  readonly surface;
  readonly roots;
  readonly morphology;
  readonly support;
  readonly annotations;
  readonly #detailsHandle: FileSystemFileHandle;
  #detailsPromise: Promise<DetailStoreReader> | null = null;

  private constructor(
    surface: ReturnType<typeof openSurfaceIndex>,
    roots: RootPayloadReader,
    morphology: MorphologyReader,
    support: AnalyzerSupportReader,
    annotations: AnalyzerAnnotationsReader,
    detailsHandle: FileSystemFileHandle
  ) {
    this.surface = surface;
    this.roots = roots;
    this.morphology = morphology;
    this.support = support;
    this.annotations = annotations;
    this.#detailsHandle = detailsHandle;
  }

  static async open(files: InstalledFiles): Promise<AnalyzerRuntime> {
    const hotBytes = new Uint8Array(await (await files.hot.getFile()).arrayBuffer());
    const pack = openPack(hotBytes);
    const surface = openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID));
    const roots = new RootPayloadReader(pack.getSection(ROOT_PAYLOAD_SECTION_ID));
    const morphology = new MorphologyReader(pack.getSection(MORPHOLOGY_SECTION_ID));
    const support = new AnalyzerSupportReader(pack.getSection(ANALYZER_SUPPORT_SECTION_ID));
    const annotations = await AnalyzerAnnotationsReader.open(
      analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
      decodeGzip
    );
    await annotations.preloadAllGenerated();
    return new AnalyzerRuntime(surface, roots, morphology, support, annotations, files.details);
  }

  analyze(text: string, options: PortableAnalyzeOptions = {}): Promise<PortableAnalysisResult> {
    return this.#run(analyzer => analyzer.analyze(text, options));
  }

  romanize(text: string): Promise<string> {
    return this.#run(analyzer => analyzer.romanize(text));
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
    this.#detailsPromise ??= detailSource(this.#detailsHandle)
      .then(source => openDetailStore(source, decodeGzip));
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
