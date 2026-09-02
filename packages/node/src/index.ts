import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  parseAnalyzerReleaseManifest,
  IchiranRuntime,
  RUST_KERNEL_WASM_URL,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseManifest
} from '@ichiran/core';
import { openVerifiedDetailSource, type FileDetailSource } from './file-details.js';

export { romanizeWithInfo } from './compat.js';
export type { AnalyzerEntityHint } from '@ichiran/core';

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

export function analyzerDataDirectory(environment: NodeJS.ProcessEnv = process.env): string {
  const directory = environment.ICHIRAN_PACK_DIR;
  if (!directory) {
    throw new Error('ICHIRAN_PACK_DIR must point to an installed analyzer release');
  }
  return resolve(directory);
}

/** Read and verify one complete release, then open the shared packed runtime. */
export interface OpenNodeRuntimeOptions {
  /** Optional code identity gate. Docker supplies this automatically. */
  readonly expectedSourceCommit?: string;
}

export async function openNodeRuntime(
  directory = analyzerDataDirectory(),
  options: OpenNodeRuntimeOptions = {}
): Promise<IchiranRuntime> {
  const manifestBytes = await readFile(resolve(directory, 'manifest.json'));
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
  const expectedSourceCommit = options.expectedSourceCommit ?? process.env.ICHIRAN_SOURCE_COMMIT;
  if (expectedSourceCommit !== undefined && manifest.sourceCommit !== expectedSourceCommit) {
    throw new Error(
      `Analyzer release sourceCommit ${manifest.sourceCommit} does not match runtime ${expectedSourceCommit}`
    );
  }
  let details: FileDetailSource | null = null;
  const detailsPromise = openVerifiedDetailSource(directory, manifest.details).then(source => {
    details = source;
    return source;
  });
  try {
    const [hot, detailSource, wasm] = await Promise.all([
      loadAsset(directory, manifest.hot),
      detailsPromise,
      readFile(RUST_KERNEL_WASM_URL).then(bytes => new Uint8Array(bytes))
    ]);
    return await IchiranRuntime.open({
      hot,
      details: detailSource,
      wasm
    });
  } catch (error) {
    if (details === null) {
      try {
        details = await detailsPromise;
      } catch {
        // The detail loader owns cleanup for failures before it returns a source.
      }
    }
    details?.dispose();
    throw error;
  }
}
