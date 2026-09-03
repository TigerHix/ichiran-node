import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  ANALYZER_WASM_URL,
  Analyzer,
  AnalyzerError
} from '@ichiran/core';
import {
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseManifest
} from '@ichiran/core/release';
import { openVerifiedDetailSource, type FileDetailSource } from './file-details.js';

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function loadAsset(directory: string, asset: AnalyzerReleaseAsset): Promise<Uint8Array> {
  const downloaded = new Uint8Array(await readFile(resolve(directory, asset.file)));
  if (
    downloaded.byteLength !== asset.downloadBytes
    || sha256(downloaded) !== asset.downloadSha256
  ) {
    throw new Error(`${asset.file} does not match the analyzer manifest`);
  }
  const installed = asset.encoding === 'gzip'
    ? new Uint8Array(gunzipSync(downloaded))
    : downloaded.slice();
  if (
    installed.byteLength !== asset.installedBytes
    || sha256(installed) !== asset.installedSha256
  ) {
    throw new Error(`${asset.file} decoded bytes do not match the analyzer manifest`);
  }
  return installed;
}

function dataDirectory(directory: string | undefined): string {
  const selected = directory ?? process.env.ICHIRAN_PACK_DIR;
  if (!selected) {
    throw new AnalyzerError(
      'invalid-input',
      'Pass a pack directory or set ICHIRAN_PACK_DIR'
    );
  }
  return resolve(selected);
}

/** Open and verify one complete packed analyzer release from disk. */
export async function openAnalyzer(directory?: string): Promise<Analyzer> {
  const resolvedDirectory = dataDirectory(directory);
  let details: FileDetailSource | null = null;
  let detailsPromise: Promise<FileDetailSource> | null = null;
  try {
    const manifestBytes = await readFile(resolve(resolvedDirectory, 'manifest.json'));
    let parsed: unknown;
    try {
      parsed = JSON.parse(manifestBytes.toString('utf8'));
    } catch {
      throw new Error('Analyzer manifest is not valid JSON');
    }
    const manifest: AnalyzerReleaseManifest = parseAnalyzerReleaseManifest(
      parsed,
      text => createHash('sha256').update(text).digest('hex')
    );
    const expectedSourceCommit = process.env.ICHIRAN_SOURCE_COMMIT;
    if (expectedSourceCommit !== undefined && manifest.sourceCommit !== expectedSourceCommit) {
      throw new Error(
        `Analyzer release sourceCommit ${manifest.sourceCommit} does not match runtime ${expectedSourceCommit}`
      );
    }
    detailsPromise = openVerifiedDetailSource(resolvedDirectory, manifest.details).then(source => {
      details = source;
      return source;
    });
    const [hot, detailSource, wasm] = await Promise.all([
      loadAsset(resolvedDirectory, manifest.hot),
      detailsPromise,
      readFile(ANALYZER_WASM_URL).then(bytes => new Uint8Array(bytes))
    ]);
    return await Analyzer.open({ hot, details: detailSource, wasm });
  } catch (error) {
    if (details === null && detailsPromise !== null) {
      try {
        details = await detailsPromise;
      } catch {
        // The detail loader closes files when it fails before returning a source.
      }
    }
    details?.dispose();
    if (error instanceof AnalyzerError) throw error;
    throw new AnalyzerError(
      'invalid-pack',
      error instanceof Error ? error.message : String(error)
    );
  }
}
