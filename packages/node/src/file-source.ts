import { createHash } from 'node:crypto';
import { createReadStream, createWriteStream, rmSync } from 'node:fs';
import { mkdtemp, open, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { Transform, type TransformCallback } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { createGunzip } from 'node:zlib';

import type { RandomAccessSource } from '@ichiran/core';
import type { AnalyzerReleaseAsset } from '@ichiran/core/release';

class DigestMeter extends Transform {
  readonly #hash = createHash('sha256');
  readonly #maximumBytes: number;
  readonly #tooLargeMessage: string;
  #bytes = 0;

  constructor(maximumBytes: number, tooLargeMessage: string) {
    super();
    this.#maximumBytes = maximumBytes;
    this.#tooLargeMessage = tooLargeMessage;
  }

  get bytes(): number {
    return this.#bytes;
  }

  digest(): string {
    return this.#hash.digest('hex');
  }

  override _transform(
    chunk: Buffer,
    _encoding: BufferEncoding,
    callback: TransformCallback
  ): void {
    this.#bytes += chunk.byteLength;
    if (this.#bytes > this.#maximumBytes) {
      callback(new Error(this.#tooLargeMessage));
      return;
    }
    this.#hash.update(chunk);
    callback(null, chunk);
  }
}

/** Node file source with no descriptor held between exact positional reads. */
export class FileRandomAccessSource implements RandomAccessSource {
  readonly byteLength: number;
  readonly path: string;
  readonly #ownedDirectory: string | null;
  #disposed = false;

  constructor(path: string, byteLength: number, ownedDirectory: string | null) {
    this.path = path;
    this.byteLength = byteLength;
    this.#ownedDirectory = ownedDirectory;
  }

  async read(offset: number, byteLength: number): Promise<Uint8Array> {
    const end = offset + byteLength;
    if (
      this.#disposed
      || !Number.isSafeInteger(offset)
      || !Number.isSafeInteger(byteLength)
      || offset < 0
      || byteLength < 0
      || !Number.isSafeInteger(end)
      || end > this.byteLength
    ) {
      throw new RangeError('Release asset read is outside the available source');
    }
    const handle = await open(this.path, 'r');
    try {
      const bytes = Buffer.allocUnsafe(byteLength);
      let received = 0;
      while (received < byteLength) {
        const result = await handle.read(
          bytes,
          received,
          byteLength - received,
          offset + received
        );
        if (result.bytesRead === 0) break;
        received += result.bytesRead;
      }
      return new Uint8Array(bytes.buffer, bytes.byteOffset, received);
    } finally {
      await handle.close();
    }
  }

  dispose(): void {
    if (this.#disposed) return;
    this.#disposed = true;
    if (this.#ownedDirectory !== null) {
      rmSync(this.#ownedDirectory, { recursive: true, force: true });
    }
  }
}

async function verifyIdentityFile(
  path: string,
  asset: AnalyzerReleaseAsset
): Promise<void> {
  const hash = createHash('sha256');
  let bytes = 0;
  for await (const chunk of createReadStream(path)) {
    const buffer = chunk as Buffer;
    bytes += buffer.byteLength;
    if (bytes > asset.downloadBytes) {
      throw new Error(`${asset.file} exceeds the analyzer manifest byte length`);
    }
    hash.update(buffer);
  }
  const digest = hash.digest('hex');
  if (bytes !== asset.downloadBytes || digest !== asset.downloadSha256) {
    throw new Error(`${asset.file} does not match the analyzer manifest`);
  }
  if (bytes !== asset.installedBytes || digest !== asset.installedSha256) {
    throw new Error(`${asset.file} decoded bytes do not match the analyzer manifest`);
  }
}

/** Verify a release asset and expose only its installed random-access bytes. */
export async function openVerifiedAssetSource(
  directory: string,
  asset: AnalyzerReleaseAsset,
  temporaryRoot = tmpdir()
): Promise<FileRandomAccessSource> {
  const input = resolve(directory, asset.file);
  if (asset.encoding === 'identity') {
    await verifyIdentityFile(input, asset);
    return new FileRandomAccessSource(input, asset.installedBytes, null);
  }

  const ownedDirectory = await mkdtemp(join(temporaryRoot, 'ichiran-node-asset-'));
  const output = join(ownedDirectory, asset.file.replace(/\.gz$/, ''));
  const downloaded = new DigestMeter(
    asset.downloadBytes,
    `${asset.file} exceeds the analyzer manifest byte length`
  );
  const installed = new DigestMeter(
    asset.installedBytes,
    `${asset.file} decoded bytes exceed the analyzer manifest byte length`
  );
  try {
    await pipeline(
      createReadStream(input),
      downloaded,
      createGunzip(),
      installed,
      createWriteStream(output, { flags: 'wx', mode: 0o600 })
    );
    if (
      downloaded.bytes !== asset.downloadBytes
      || downloaded.digest() !== asset.downloadSha256
    ) {
      throw new Error(`${asset.file} does not match the analyzer manifest`);
    }
    if (
      installed.bytes !== asset.installedBytes
      || installed.digest() !== asset.installedSha256
    ) {
      throw new Error(`${asset.file} decoded bytes do not match the analyzer manifest`);
    }
    return new FileRandomAccessSource(output, asset.installedBytes, ownedDirectory);
  } catch (error) {
    await rm(ownedDirectory, { recursive: true, force: true });
    throw error;
  }
}
