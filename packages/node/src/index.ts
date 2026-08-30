import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  IchiranRuntime,
  memoryDetailSource
} from '@ichiran/core';

export { romanizeWithInfo } from './compat.js';
export type { AnalyzerEntityHint } from '@ichiran/core';

interface ReleaseAsset {
  readonly file: string;
  readonly encoding: 'gzip';
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

interface ReleaseManifest {
  readonly formatVersion: 1;
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly hot: ReleaseAsset;
  readonly details: ReleaseAsset;
  readonly manifestSha256: string;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function releaseAsset(value: unknown, name: 'hot' | 'details'): ReleaseAsset {
  if (typeof value !== 'object' || value === null) {
    throw new Error(`Analyzer manifest is missing ${name}`);
  }
  const asset = value as Partial<ReleaseAsset>;
  const expectedFile = `${name}.bin.gz`;
  if (
    asset.file !== expectedFile
    || asset.encoding !== 'gzip'
    || !Number.isSafeInteger(asset.downloadBytes)
    || !Number.isSafeInteger(asset.installedBytes)
    || typeof asset.downloadSha256 !== 'string'
    || typeof asset.installedSha256 !== 'string'
  ) {
    throw new Error(`Analyzer manifest has an invalid ${name} asset`);
  }
  return asset as ReleaseAsset;
}

async function loadAsset(directory: string, asset: ReleaseAsset): Promise<Uint8Array> {
  const downloaded = new Uint8Array(await readFile(resolve(directory, asset.file)));
  if (
    downloaded.byteLength !== asset.downloadBytes
    || sha256(downloaded) !== asset.downloadSha256
  ) {
    throw new Error(`${asset.file} does not match the analyzer manifest`);
  }
  const installed = new Uint8Array(gunzipSync(downloaded));
  if (
    installed.byteLength !== asset.installedBytes
    || sha256(installed) !== asset.installedSha256
  ) {
    throw new Error(`${asset.file} decoded bytes do not match the analyzer manifest`);
  }
  return installed;
}

export function analyzerDataDirectory(environment: NodeJS.ProcessEnv = process.env): string {
  const directory = environment.ICHIRAN_PACK_DIR;
  if (!directory) {
    throw new Error('ICHIRAN_PACK_DIR must point to an installed analyzer release');
  }
  return resolve(directory);
}

/** Read and verify one complete release, then open the shared packed runtime. */
export async function openNodeRuntime(directory = analyzerDataDirectory()): Promise<IchiranRuntime> {
  const manifestBytes = await readFile(resolve(directory, 'manifest.json'));
  let parsed: unknown;
  try {
    parsed = JSON.parse(manifestBytes.toString('utf8'));
  } catch {
    throw new Error('Analyzer manifest is not valid JSON');
  }
  if (typeof parsed !== 'object' || parsed === null) {
    throw new Error('Analyzer manifest must be an object');
  }
  const manifest = parsed as Partial<ReleaseManifest>;
  if (
    manifest.formatVersion !== 1
    || typeof manifest.packVersion !== 'string'
    || typeof manifest.sourceCommit !== 'string'
    || typeof manifest.sourcesLockSha256 !== 'string'
    || typeof manifest.manifestSha256 !== 'string'
  ) {
    throw new Error('Analyzer manifest has an unsupported format');
  }
  const hotAsset = releaseAsset(manifest.hot, 'hot');
  const detailAsset = releaseAsset(manifest.details, 'details');
  const digestInput = JSON.stringify({
    formatVersion: manifest.formatVersion,
    packVersion: manifest.packVersion,
    sourceCommit: manifest.sourceCommit,
    sourcesLockSha256: manifest.sourcesLockSha256,
    hot: hotAsset,
    details: detailAsset
  });
  if (sha256(new TextEncoder().encode(digestInput)) !== manifest.manifestSha256) {
    throw new Error('Analyzer manifest checksum does not match');
  }
  const [hot, details] = await Promise.all([
    loadAsset(directory, hotAsset),
    loadAsset(directory, detailAsset)
  ]);
  const decodeGzip = async (compressed: Uint8Array, expectedByteLength: number) => {
    const decoded = new Uint8Array(gunzipSync(compressed));
    if (decoded.byteLength !== expectedByteLength) {
      throw new Error(`Decoded block has ${decoded.byteLength} bytes; expected ${expectedByteLength}`);
    }
    return decoded;
  };
  return IchiranRuntime.open({
    hot,
    details: memoryDetailSource(details),
    decodeGzip
  });
}
