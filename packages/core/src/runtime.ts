import {
  type PortableAnalyzeOptions,
  type PortableAnalysisResult
} from './analyzer.js';
import {
  DetailStoreError,
  type DetailEntry,
  type DetailRandomAccessSource,
  type DetailStoreErrorCode
} from './details.js';
import type { RomanizationName } from './romanization.js';
import { PORTABLE_LEGACY_INFO } from './analyzer-legacy.js';
import init, {
  detail_prefix_length,
  WasmDetailStore,
  WasmKernel,
  type InitOutput
} from './rust-kernel/generated/ichiran_kernel.js';
import { validatePortableAnalyzeRequest } from './analyzer-options.js';

interface DetailRange {
  readonly offset: number;
  readonly byteLength: number;
}

type LegacyStep =
  | {
      readonly state: 'ready';
      readonly value: unknown;
      readonly metadata: LegacyWireMetadata;
    }
  | {
      readonly state: 'missing-detail';
      readonly entryIndex: number;
      readonly range: DetailRange;
    };

interface LegacyWireMetadata {
  readonly words: readonly (Record<string, unknown> | null)[];
  readonly conjugations: readonly (Record<string, unknown> | null)[];
}

export interface RustKernelMetrics {
  readonly openMs: number;
  readonly transientBytes: number;
  readonly wasmLinearMemoryBytes: number;
  readonly kernelPayloadBytes: number;
  readonly detailResidentBytes: number;
  readonly workerHeapBytes: number | null;
}

export interface IchiranRuntimeSource {
  /** Installed, uncompressed hot pack bytes. */
  readonly hot: Uint8Array;
  /** Installed, uncompressed random-access detail store. */
  readonly details: DetailRandomAccessSource;
  /** Node supplies the same emitted WASM bytes because file-URL fetch is unavailable. */
  readonly wasm?: Uint8Array;
}

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });
let initialized: Promise<InitOutput> | null = null;

export const RUST_KERNEL_WASM_URL = new URL(
  './rust-kernel/generated/ichiran_kernel_bg.wasm',
  import.meta.url
);

function initialize(wasm?: Uint8Array): Promise<InitOutput> {
  initialized ??= wasm === undefined
    ? init()
    : init({ module_or_path: wasm });
  return initialized;
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

function reviveLegacyInfo(value: unknown, metadata: LegacyWireMetadata): unknown {
  let wordIndex = 0;
  let conjugationIndex = 0;

  const attach = (target: Record<string | symbol, unknown>, facts: unknown): void => {
    if (typeof facts === 'object' && facts !== null) {
      Object.defineProperty(target, PORTABLE_LEGACY_INFO, { value: facts });
    }
  };
  const object = (target: unknown, label: string): Record<string | symbol, unknown> => {
    if (typeof target !== 'object' || target === null || Array.isArray(target)) {
      throw new Error(`Invalid Rust legacy ${label} value`);
    }
    return target as Record<string | symbol, unknown>;
  };
  const visitConjugation = (target: unknown): void => {
    const conjugation = object(target, 'conjugation');
    attach(conjugation, metadata.conjugations[conjugationIndex++]);
    if (Array.isArray(conjugation.via)) {
      for (const child of conjugation.via) visitConjugation(child);
    }
  };
  const visitWord = (target: unknown): void => {
    const word = object(target, 'word');
    attach(word, metadata.words[wordIndex++]);
    if (Array.isArray(word.components)) {
      for (const component of word.components) visitWord(component);
    }
    if (Array.isArray(word.alternative)) {
      for (const alternative of word.alternative) visitWord(alternative);
    }
    if (Array.isArray(word.conj)) {
      for (const conjugation of word.conj) visitConjugation(conjugation);
    }
  };

  if (!Array.isArray(value)) throw new Error('Invalid Rust legacy output');
  for (const chunk of value) {
    if (typeof chunk === 'string') continue;
    if (!Array.isArray(chunk)) throw new Error('Invalid Rust legacy chunk');
    for (const path of chunk) {
      if (!Array.isArray(path) || !Array.isArray(path[0])) {
        throw new Error('Invalid Rust legacy path');
      }
      for (const token of path[0]) {
        if (!Array.isArray(token)) throw new Error('Invalid Rust legacy token');
        visitWord(token[1]);
      }
    }
  }
  if (wordIndex !== metadata.words.length || conjugationIndex !== metadata.conjugations.length) {
    throw new Error('Rust legacy metadata shape does not match the serialized result');
  }
  return value;
}

function optionsJson(options: ReturnType<typeof validatePortableAnalyzeRequest>['options']): Uint8Array {
  return encoder.encode(JSON.stringify(options));
}

async function readExact(
  source: DetailRandomAccessSource,
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

/**
 * Shared Rust/WASM analyzer runtime over one immutable installed pack.
 *
 * Hosts own verification, persistence, and random-access reads. Each analyzer
 * operation crosses into WASM once; retained details retry by exact block range.
 */
export class IchiranRuntime {
  readonly #kernel: WasmKernel;
  readonly #details: WasmDetailStore;
  readonly #detailSource: DetailRandomAccessSource;
  readonly #memory: WebAssembly.Memory;
  readonly #openMs: number;
  readonly #transientBytes: number;

  private constructor(
    kernel: WasmKernel,
    details: WasmDetailStore,
    detailSource: DetailRandomAccessSource,
    memory: WebAssembly.Memory,
    openMs: number,
    transientBytes: number
  ) {
    this.#kernel = kernel;
    this.#details = details;
    this.#detailSource = detailSource;
    this.#memory = memory;
    this.#openMs = openMs;
    this.#transientBytes = transientBytes;
  }

  static async open(source: IchiranRuntimeSource): Promise<IchiranRuntime> {
    const started = now();
    const [wasm, header] = await Promise.all([
      initialize(source.wasm),
      readExact(source.details, 0, 96, 'invalid-header')
    ]);
    const prefixLength = detail_prefix_length(header, source.details.byteLength);
    const prefix = await readExact(source.details, 0, prefixLength, 'corrupt-index');
    const kernel = new WasmKernel(source.hot);
    try {
      const details = new WasmDetailStore(prefix, source.details.byteLength);
      return new IchiranRuntime(
        kernel,
        details,
        source.details,
        wasm.memory,
        now() - started,
        wasm.memory.buffer.byteLength + source.hot.byteLength + prefix.byteLength
      );
    } catch (error) {
      kernel.free();
      throw error;
    }
  }

  entryIndexForSequence(sequence: number): number {
    return this.#kernel.entry_index_for_sequence(sequence);
  }

  analyze(text: string, options: PortableAnalyzeOptions = {}): Promise<PortableAnalysisResult> {
    const validated = validatePortableAnalyzeRequest(text, options);
    return Promise.resolve(json<PortableAnalysisResult>(this.#kernel.analyze_utf16_options(
      utf16(validated.input),
      optionsJson(validated.options)
    )));
  }

  romanize(
    text: string,
    options: PortableAnalyzeOptions & { readonly method?: RomanizationName } = {}
  ): Promise<string> {
    const validated = validatePortableAnalyzeRequest(text, options);
    return Promise.resolve(fromUtf16(this.#kernel.romanize_utf16_options(
      utf16(validated.input),
      optionsJson(validated.options),
      options.method ?? ''
    )));
  }

  async legacy(
    text: string,
    options: PortableAnalyzeOptions & { readonly method?: RomanizationName } = {}
  ): Promise<unknown> {
    const validated = validatePortableAnalyzeRequest(text, options);
    const operation = this.#kernel.legacy_begin_utf16(
      utf16(validated.input),
      optionsJson(validated.options),
      options.method ?? ''
    );
    try {
      const loaded = new Set<string>();
      for (;;) {
        const step = json<LegacyStep>(operation.legacy_step(this.#kernel, this.#details));
        if (step.state === 'ready') return reviveLegacyInfo(step.value, step.metadata);
        const key = `${step.entryIndex}:${step.range.offset}:${step.range.byteLength}`;
        if (loaded.has(key)) {
          throw new Error(`Detail range ${key} remained unavailable after preload`);
        }
        loaded.add(key);
        const compressed = await readExact(
          this.#detailSource,
          step.range.offset,
          step.range.byteLength,
          'corrupt-block'
        );
        this.#details.entry_json(step.entryIndex, compressed);
      }
    } finally {
      operation.free();
    }
  }

  async describe(entryIndex: number): Promise<DetailEntry> {
    const range = json<DetailRange>(this.#details.range_json(entryIndex));
    const compressed = await readExact(
      this.#detailSource,
      range.offset,
      range.byteLength,
      'corrupt-block'
    );
    return json<DetailEntry>(this.#details.entry_json(entryIndex, compressed));
  }

  metrics(): RustKernelMetrics {
    const memory = performance as Performance & {
      readonly memory?: { readonly usedJSHeapSize: number };
    };
    return {
      openMs: this.#openMs,
      transientBytes: this.#transientBytes,
      wasmLinearMemoryBytes: this.#memory.buffer.byteLength,
      kernelPayloadBytes: this.#kernel.resident_payload_bytes(),
      detailResidentBytes: this.#details.resident_bytes(),
      workerHeapBytes: memory.memory?.usedJSHeapSize ?? null
    };
  }

  dispose(): void {
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
}
