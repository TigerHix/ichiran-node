import {
  IchiranRuntime,
  RUST_KERNEL_WASM_URL,
  type DetailRandomAccessSource
} from '@ichiran/core';
import type { InstalledFiles } from './install.js';

async function gunzip(compressed: Uint8Array): Promise<Uint8Array> {
  const owned = new Uint8Array(compressed.byteLength);
  owned.set(compressed);
  const stream = new Blob([owned.buffer])
    .stream()
    .pipeThrough(new DecompressionStream('gzip'));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

export async function decodeGzip(
  compressed: Uint8Array,
  expectedByteLength: number
): Promise<Uint8Array> {
  const bytes = await gunzip(compressed);
  if (bytes.byteLength !== expectedByteLength) {
    throw new Error(
      `Decoded detail block has ${bytes.byteLength} bytes; expected ${expectedByteLength}`
    );
  }
  return bytes;
}

async function rustKernelWasm(): Promise<Uint8Array> {
  // The final `.bin` keeps the gzip body opaque across static hosts; the
  // Worker, not HTTP Content-Encoding, owns its one decompression boundary.
  const compressed = await fetch(`${RUST_KERNEL_WASM_URL.href}.gz.bin`);
  if (compressed.ok) {
    if (!compressed.body) throw new Error('Rust kernel shell asset has no response body');
    const stream = compressed.body.pipeThrough(new DecompressionStream('gzip'));
    return new Uint8Array(await new Response(stream).arrayBuffer());
  }
  // Vite's development server exposes the uncompressed generated asset. The
  // finalized production shell removes it after emitting the gzip sibling.
  const raw = await fetch(RUST_KERNEL_WASM_URL);
  if (raw.ok) return new Uint8Array(await raw.arrayBuffer());
  throw new Error(`Rust kernel shell asset returned HTTP ${compressed.status}`);
}

export async function detailSource(
  handle: FileSystemFileHandle
): Promise<DetailRandomAccessSource> {
  const byteLength = (await handle.getFile()).size;
  return {
    byteLength,
    async read(offset, byteLength) {
      const file = await handle.getFile();
      return new Uint8Array(await file.slice(offset, offset + byteLength).arrayBuffer());
    }
  };
}

/** Open the shared analyzer runtime over the browser's verified OPFS files. */
export async function openAnalyzerRuntime(files: InstalledFiles): Promise<IchiranRuntime> {
  const [hot, details, wasm] = await Promise.all([
    files.hot.getFile().then(file => file.arrayBuffer()).then(bytes => new Uint8Array(bytes)),
    detailSource(files.details),
    rustKernelWasm()
  ]);
  return IchiranRuntime.open({
    hot,
    details,
    wasm
  });
}
