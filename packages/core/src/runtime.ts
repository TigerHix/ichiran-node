import type { PortableAnalyzeOptions } from './analyzer-options.js';
import type { PortableAnalysisResult as AnalysisResult } from './analyzer-result-contract.js';
import type { AnalyzerEntityHint as EntityHint } from './analyzer-types.js';
import {
  DetailStoreError,
  type DetailEntry as DictionaryEntry,
  type DetailStoreErrorCode
} from './details-contract.js';
import type { RomanizationName as RomanizationScheme } from './romanization-contract.js';
import init, {
  detail_prefix_length,
  WasmDetailStore,
  WasmKernel,
  type InitOutput
} from './rust-kernel/generated/ichiran_kernel.js';
import { AnalyzerInputError, validatePortableAnalyzeRequest } from './analyzer-options.js';

interface DetailRange {
  readonly offset: number;
  readonly byteLength: number;
}

export type AnalyzerErrorCode =
  | 'invalid-input'
  | 'invalid-pack'
  | 'not-found'
  | 'internal';

export class AnalyzerError extends Error {
  readonly code: AnalyzerErrorCode;

  constructor(code: AnalyzerErrorCode, message: string) {
    super(message);
    this.name = 'AnalyzerError';
    this.code = code;
  }
}

export interface AnalyzeOptions {
  readonly limit?: number;
  readonly entities?: readonly EntityHint[];
  readonly normalizePunctuation?: boolean;
}

export interface RomanizeOptions {
  readonly method?: RomanizationScheme;
  readonly entities?: readonly EntityHint[];
  readonly normalizePunctuation?: boolean;
}

export interface RandomAccessSource {
  readonly byteLength: number;
  read(offset: number, byteLength: number): Promise<Uint8Array>;
  dispose?(): void;
}

export interface AnalyzerSource {
  /** Installed, uncompressed hot pack bytes. */
  readonly hot: Uint8Array;
  /** Installed, uncompressed random-access detail store. */
  readonly details: RandomAccessSource;
  /** Hosts may supply the emitted WASM bytes when URL loading is unavailable. */
  readonly wasm?: Uint8Array;
}

export interface AnalyzerDiagnostics {
  readonly openMs: number;
  readonly transientBytes: number;
  readonly wasmLinearMemoryBytes: number;
  readonly kernelPayloadBytes: number;
  readonly detailResidentBytes: number;
  readonly workerHeapBytes: number | null;
}

interface RuntimeState {
  readonly kernel: WasmKernel;
  readonly details: WasmDetailStore;
  readonly detailSource: RandomAccessSource;
  readonly memory: WebAssembly.Memory;
  readonly openMs: number;
  readonly transientBytes: number;
}

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });
const runtimeStates = new WeakMap<Analyzer, RuntimeState>();
let initialized: Promise<InitOutput> | null = null;

export const ANALYZER_WASM_URL = new URL(
  './rust-kernel/generated/ichiran_kernel_bg.wasm',
  import.meta.url
);

function initialize(wasm?: Uint8Array): Promise<InitOutput> {
  if (initialized) return initialized;
  const attempt = wasm === undefined
    ? init()
    : init({ module_or_path: wasm });
  initialized = attempt;
  void attempt.catch(() => {
    if (initialized === attempt) initialized = null;
  });
  return attempt;
}

function utf16(text: string): Uint16Array {
  const units = new Uint16Array(text.length);
  for (let index = 0; index < text.length; index++) units[index] = text.charCodeAt(index);
  return units;
}

function fromUtf16(units: Uint16Array): string {
  let output = '';
  for (let offset = 0; offset < units.length; offset += 1024) {
    output += String.fromCharCode(...units.subarray(offset, offset + 1024));
  }
  return output;
}

function json<T>(bytes: Uint8Array): T {
  return JSON.parse(decoder.decode(bytes)) as T;
}

function optionsJson(options: ReturnType<typeof validatePortableAnalyzeRequest>['options']): Uint8Array {
  return encoder.encode(JSON.stringify(options));
}

const romanizationSchemes = new Set<RomanizationScheme>([
  'hepburn-basic',
  'hepburn-simple',
  'hepburn-passport',
  'hepburn-traditional',
  'hepburn-modified',
  'kunrei-siki'
]);

function romanizationScheme(value: unknown): RomanizationScheme | '' {
  if (value === undefined) return '';
  if (typeof value === 'string' && romanizationSchemes.has(value as RomanizationScheme)) {
    return value as RomanizationScheme;
  }
  throw new AnalyzerInputError('method must be a supported romanization scheme');
}

async function readExact(
  source: RandomAccessSource,
  offset: number,
  byteLength: number,
  code: DetailStoreErrorCode
): Promise<Uint8Array> {
  const bytes = await source.read(offset, byteLength);
  if (bytes.byteLength !== byteLength) {
    throw new DetailStoreError(
      code,
      `Detail source returned ${bytes.byteLength} bytes; expected ${byteLength}`
    );
  }
  return bytes;
}

function now(): number {
  return globalThis.performance?.now() ?? Date.now();
}

function errorCode(error: unknown): unknown {
  return error instanceof Error
    ? (error as Error & { readonly code?: unknown }).code
    : undefined;
}

/** Collapse implementation-specific failures into the stable product contract. */
export function analyzerError(
  error: unknown,
  fallback: AnalyzerErrorCode = 'internal'
): AnalyzerError {
  if (error instanceof AnalyzerError) return error;
  const code = errorCode(error);
  let publicCode = fallback;
  if (error instanceof AnalyzerInputError || code === 'invalid-input') {
    publicCode = 'invalid-input';
  } else if (code === 'out-of-range' && fallback === 'not-found') {
    publicCode = 'not-found';
  } else if (
    error instanceof DetailStoreError
    || code === 'invalid-header'
    || code === 'unsupported-version'
    || code === 'invalid-directory'
    || code === 'corrupt-section'
    || code === 'corrupt-payload'
    || code === 'corrupt-index'
    || code === 'corrupt-block'
    || code === 'missing-section'
    || code === 'out-of-range'
  ) {
    publicCode = 'invalid-pack';
  }
  const message = error instanceof Error ? error.message : String(error);
  return new AnalyzerError(publicCode, message);
}

function call<T>(operation: () => T, fallback?: AnalyzerErrorCode): T {
  try {
    return operation();
  } catch (error) {
    throw analyzerError(error, fallback);
  }
}

/**
 * The production analyzer. It owns its random-access source after a successful
 * open and releases both WASM state and the source when disposed.
 */
export class Analyzer {
  readonly #kernel: WasmKernel;
  readonly #details: WasmDetailStore;
  readonly #detailSource: RandomAccessSource;
  #disposed = false;

  private constructor(
    kernel: WasmKernel,
    details: WasmDetailStore,
    detailSource: RandomAccessSource,
    state: Omit<RuntimeState, 'kernel' | 'details' | 'detailSource'>
  ) {
    this.#kernel = kernel;
    this.#details = details;
    this.#detailSource = detailSource;
    runtimeStates.set(this, { kernel, details, detailSource, ...state });
  }

  static async open(source: AnalyzerSource): Promise<Analyzer> {
    const started = now();
    let kernel: WasmKernel | null = null;
    try {
      const [wasm, header] = await Promise.all([
        initialize(source.wasm),
        readExact(source.details, 0, 96, 'invalid-header')
      ]);
      const prefixLength = detail_prefix_length(header, source.details.byteLength);
      const prefix = await readExact(source.details, 0, prefixLength, 'corrupt-index');
      kernel = new WasmKernel(source.hot);
      const details = new WasmDetailStore(prefix, source.details.byteLength);
      return new Analyzer(kernel, details, source.details, {
        memory: wasm.memory,
        openMs: now() - started,
        transientBytes: wasm.memory.buffer.byteLength + source.hot.byteLength + prefix.byteLength
      });
    } catch (error) {
      kernel?.free();
      throw analyzerError(error);
    }
  }

  async analyze(text: string, options: AnalyzeOptions = {}): Promise<AnalysisResult> {
    this.#assertOpen();
    try {
      const validated = validatePortableAnalyzeRequest(text, options);
      return call(() => json<AnalysisResult>(
        this.#kernel.analyze_utf16_options(utf16(validated.input), optionsJson(validated.options))
      ));
    } catch (error) {
      throw analyzerError(error);
    }
  }

  async romanize(text: string, options: RomanizeOptions = {}): Promise<string> {
    this.#assertOpen();
    try {
      const validated = validatePortableAnalyzeRequest(text, options as PortableAnalyzeOptions);
      const topPathOptions = { ...validated.options, limit: 1 };
      return call(() => fromUtf16(this.#kernel.romanize_utf16_options(
        utf16(validated.input),
        optionsJson(topPathOptions),
        romanizationScheme(options.method)
      )));
    } catch (error) {
      throw analyzerError(error);
    }
  }

  async entry(entryIndex: number): Promise<DictionaryEntry> {
    this.#assertOpen();
    if (!Number.isSafeInteger(entryIndex) || entryIndex < 0) {
      throw new AnalyzerError('invalid-input', 'entryIndex must be a non-negative integer');
    }
    try {
      const rangeBytes = call(() => this.#details.range_json(entryIndex), 'not-found');
      const range = call(() => json<DetailRange>(rangeBytes));
      const compressed = await readExact(
        this.#detailSource,
        range.offset,
        range.byteLength,
        'corrupt-block'
      );
      return call(
        () => json<DictionaryEntry>(this.#details.entry_json(entryIndex, compressed)),
        'invalid-pack'
      );
    } catch (error) {
      throw analyzerError(error);
    }
  }

  dispose(): void {
    if (this.#disposed) return;
    this.#disposed = true;
    runtimeStates.delete(this);
    try {
      this.#details.free();
    } finally {
      try {
        this.#kernel.free();
      } finally {
        this.#detailSource.dispose?.();
      }
    }
  }

  #assertOpen(): void {
    if (this.#disposed) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  }
}

/** Qualification-only diagnostics; exported only by the qualification/runtime subpath. */
export function readAnalyzerDiagnostics(analyzer: Analyzer): AnalyzerDiagnostics {
  const state = runtimeStates.get(analyzer);
  if (!state) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  const memory = performance as Performance & {
    readonly memory?: { readonly usedJSHeapSize: number };
  };
  return {
    openMs: state.openMs,
    transientBytes: state.transientBytes,
    wasmLinearMemoryBytes: state.memory.buffer.byteLength,
    kernelPayloadBytes: state.kernel.resident_payload_bytes(),
    detailResidentBytes: state.details.resident_bytes(),
    workerHeapBytes: memory.memory?.usedJSHeapSize ?? null
  };
}

/** Internal state bridge used only by explicit qualification modules. */
export function analyzerQualificationState(analyzer: Analyzer): RuntimeState {
  const state = runtimeStates.get(analyzer);
  if (!state) throw new AnalyzerError('internal', 'Analyzer has been disposed');
  return state;
}
