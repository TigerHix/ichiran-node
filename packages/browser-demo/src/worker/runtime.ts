import {
  IchiranRuntime,
  type DetailRandomAccessSource
} from '@ichiran/core';
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

/** Open the shared analyzer runtime over the browser's verified OPFS files. */
export async function openAnalyzerRuntime(files: InstalledFiles): Promise<IchiranRuntime> {
  return IchiranRuntime.open({
    hot: new Uint8Array(await (await files.hot.getFile()).arrayBuffer()),
    details: await detailSource(files.details),
    decodeGzip
  });
}
