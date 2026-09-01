import type {
  PortableAnalysisResult,
  PortableAnalyzeOptions
} from '@ichiran/core';
import { validatePortableAnalyzeRequest } from '@ichiran/core';
import init, {
  detail_prefix_length,
  WasmDetailStore,
  WasmKernel
} from '../rust-kernel/generated/ichiran_kernel.js';
import type { InstalledFiles } from './install.js';

interface DetailRange {
  readonly offset: number;
  readonly byteLength: number;
}

const decoder = new TextDecoder('utf-8', { fatal: true });

function utf16(text: string): Uint16Array {
  const units = new Uint16Array(text.length);
  for (let index = 0; index < text.length; index++) units[index] = text.charCodeAt(index);
  return units;
}

function json<T>(bytes: Uint8Array): T {
  return JSON.parse(decoder.decode(bytes)) as T;
}

/** Experimental M1 adapter. TypeScript retains OPFS and all install/lifecycle ownership. */
export class RustM1Runtime {
  readonly #kernel: WasmKernel;
  readonly #details: WasmDetailStore;
  readonly #detailFile: File;
  readonly #memory: WebAssembly.Memory;
  readonly #openMs: number;
  readonly #transientBytes: number;

  private constructor(
    kernel: WasmKernel,
    details: WasmDetailStore,
    detailFile: File,
    memory: WebAssembly.Memory,
    openMs: number,
    transientBytes: number
  ) {
    this.#kernel = kernel;
    this.#details = details;
    this.#detailFile = detailFile;
    this.#memory = memory;
    this.#openMs = openMs;
    this.#transientBytes = transientBytes;
  }

  static async open(files: InstalledFiles): Promise<RustM1Runtime> {
    const started = performance.now();
    const initialized = await init();
    const [hotFile, detailFile] = await Promise.all([
      files.hot.getFile(),
      files.details.getFile()
    ]);
    const header = new Uint8Array(await detailFile.slice(0, 96).arrayBuffer());
    const prefixLength = detail_prefix_length(header, detailFile.size);
    const prefix = new Uint8Array(await detailFile.slice(0, prefixLength).arrayBuffer());
    const hot = new Uint8Array(await hotFile.arrayBuffer());
    const kernel = new WasmKernel(hot);
    const details = new WasmDetailStore(prefix, detailFile.size);
    return new RustM1Runtime(
      kernel,
      details,
      detailFile,
      initialized.memory,
      performance.now() - started,
      initialized.memory.buffer.byteLength + hot.byteLength + prefix.byteLength
    );
  }

  analyze(text: string, options: PortableAnalyzeOptions = {}): Promise<PortableAnalysisResult> {
    const validated = validatePortableAnalyzeRequest(text, options);
    if (validated.options.entities.length > 0) {
      return Promise.reject(new Error('Rust M1 does not implement entity hints'));
    }
    if (validated.options.normalizePunctuation) {
      return Promise.reject(new Error('Rust M1 does not implement punctuation normalization'));
    }
    return Promise.resolve(json<PortableAnalysisResult>(
      this.#kernel.analyze_utf16(utf16(validated.input), validated.options.limit)
    ));
  }

  async describe(entryIndex: number): Promise<unknown> {
    const range = json<DetailRange>(this.#details.range_json(entryIndex));
    const compressed = new Uint8Array(await this.#detailFile
      .slice(range.offset, range.offset + range.byteLength)
      .arrayBuffer());
    return json(this.#details.entry_json(entryIndex, compressed));
  }

  legacy(): Promise<never> {
    return Promise.reject(new Error('Rust M1 does not implement retained legacy serialization'));
  }

  romanize(): Promise<never> {
    return Promise.reject(new Error('Rust M1 does not implement standalone romanization'));
  }

  dispose(): void {
    this.#details.free();
    this.#kernel.free();
  }

  metrics(): {
    readonly openMs: number;
    readonly transientBytes: number;
    readonly wasmLinearMemoryBytes: number;
    readonly kernelPayloadBytes: number;
    readonly detailResidentBytes: number;
    readonly workerHeapBytes: number | null;
  } {
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
}

export async function openRustM1Runtime(files: InstalledFiles): Promise<RustM1Runtime> {
  return RustM1Runtime.open(files);
}
