import { createHash } from 'node:crypto';
import { execFile as execFileCallback } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import { basename, join, resolve } from 'node:path';
import { promisify } from 'node:util';
import { gunzipSync } from 'node:zlib';

interface ReleaseAsset {
  readonly file: string;
  readonly encoding: 'identity' | 'gzip';
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

export interface ReleaseManifest {
  readonly formatVersion: 1;
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly manifestSha256: string;
  readonly hot: ReleaseAsset;
  readonly details: ReleaseAsset;
}

export interface VerifiedRelease {
  readonly directory: string;
  readonly manifest: ReleaseManifest;
  readonly manifestBytes: Uint8Array;
  readonly hotBytes: Uint8Array;
  readonly detailsBytes: Uint8Array;
}

const execFile = promisify(execFileCallback);

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parseAsset(value: unknown, label: string): ReleaseAsset {
  if (!isObject(value)) throw new Error(`${label} manifest field is not an object`);
  if (
    typeof value.file !== 'string'
    || value.file !== basename(value.file)
    || !value.file.endsWith('.bin.gz')
  ) {
    throw new Error(`${label} manifest field has an unsafe analyzer filename`);
  }
  if (value.encoding !== 'gzip') throw new Error(`${label} analyzer asset must use gzip`);
  for (const key of ['downloadBytes', 'installedBytes'] as const) {
    if (!Number.isSafeInteger(value[key]) || (value[key] as number) <= 0) {
      throw new Error(`${label}.${key} must be a positive integer`);
    }
  }
  for (const key of ['downloadSha256', 'installedSha256'] as const) {
    if (typeof value[key] !== 'string' || !/^[0-9a-f]{64}$/.test(value[key] as string)) {
      throw new Error(`${label}.${key} must be a lowercase SHA-256`);
    }
  }
  return value as unknown as ReleaseAsset;
}

function parseManifest(value: unknown): ReleaseManifest {
  if (!isObject(value) || value.formatVersion !== 1) {
    throw new Error('Analyzer release manifest format is unsupported');
  }
  if (typeof value.packVersion !== 'string' || value.packVersion.length === 0) {
    throw new Error('Analyzer release manifest packVersion is missing');
  }
  if (typeof value.sourceCommit !== 'string' || !/^[0-9a-f]{40}$/.test(value.sourceCommit)) {
    throw new Error('Analyzer release manifest sourceCommit is invalid');
  }
  for (const key of ['sourcesLockSha256', 'manifestSha256'] as const) {
    if (typeof value[key] !== 'string' || !/^[0-9a-f]{64}$/.test(value[key] as string)) {
      throw new Error(`Analyzer release manifest ${key} is invalid`);
    }
  }
  return {
    formatVersion: 1,
    packVersion: value.packVersion,
    sourceCommit: value.sourceCommit,
    sourcesLockSha256: value.sourcesLockSha256 as string,
    manifestSha256: value.manifestSha256 as string,
    hot: parseAsset(value.hot, 'hot'),
    details: parseAsset(value.details, 'details')
  };
}

function manifestDigestInput(manifest: ReleaseManifest): string {
  return JSON.stringify({
    formatVersion: manifest.formatVersion,
    packVersion: manifest.packVersion,
    sourceCommit: manifest.sourceCommit,
    sourcesLockSha256: manifest.sourcesLockSha256,
    hot: {
      file: manifest.hot.file,
      encoding: manifest.hot.encoding,
      downloadBytes: manifest.hot.downloadBytes,
      downloadSha256: manifest.hot.downloadSha256,
      installedBytes: manifest.hot.installedBytes,
      installedSha256: manifest.hot.installedSha256
    },
    details: {
      file: manifest.details.file,
      encoding: manifest.details.encoding,
      downloadBytes: manifest.details.downloadBytes,
      downloadSha256: manifest.details.downloadSha256,
      installedBytes: manifest.details.installedBytes,
      installedSha256: manifest.details.installedSha256
    }
  });
}

export async function currentSourceIdentity(repositoryRoot: string): Promise<{
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
}> {
  const { stdout } = await execFile('git', ['-C', repositoryRoot, 'rev-parse', 'HEAD'], {
    encoding: 'utf8'
  });
  const sourceCommit = stdout.trim();
  if (!/^[0-9a-f]{40}$/.test(sourceCommit)) throw new Error('Current Git HEAD is invalid');
  const lock = await readFile(join(repositoryRoot, 'browser-alpha', 'sources.lock.json'));
  return { sourceCommit, sourcesLockSha256: sha256(lock) };
}

export async function verifyAnalyzerRelease(
  directory: string,
  repositoryRoot: string
): Promise<VerifiedRelease> {
  const resolved = resolve(directory);
  const manifestBytes = await readFile(join(resolved, 'manifest.json'));
  const manifest = parseManifest(JSON.parse(new TextDecoder().decode(manifestBytes)));
  const expectedManifestSha256 = sha256(new TextEncoder().encode(manifestDigestInput(manifest)));
  if (manifest.manifestSha256 !== expectedManifestSha256) {
    throw new Error('Analyzer release manifest checksum does not match its contents');
  }

  const identity = await currentSourceIdentity(repositoryRoot);
  if (manifest.sourceCommit !== identity.sourceCommit) {
    throw new Error(
      `Analyzer release is stale: sourceCommit ${manifest.sourceCommit} != current ${identity.sourceCommit}`
    );
  }
  if (manifest.sourcesLockSha256 !== identity.sourcesLockSha256) {
    throw new Error(
      `Analyzer release is stale: sourcesLockSha256 ${manifest.sourcesLockSha256} != current ${identity.sourcesLockSha256}`
    );
  }

  const verifyAsset = async (asset: ReleaseAsset, label: string): Promise<Uint8Array> => {
    const bytes = await readFile(join(resolved, asset.file));
    if (bytes.byteLength !== asset.downloadBytes) {
      throw new Error(`${label} download size ${bytes.byteLength} != manifest ${asset.downloadBytes}`);
    }
    const digest = sha256(bytes);
    if (digest !== asset.downloadSha256) {
      throw new Error(`${label} download checksum ${digest} != manifest ${asset.downloadSha256}`);
    }
    const installed = new Uint8Array(gunzipSync(bytes));
    if (installed.byteLength !== asset.installedBytes) {
      throw new Error(
        `${label} installed size ${installed.byteLength} != manifest ${asset.installedBytes}`
      );
    }
    const installedDigest = sha256(installed);
    if (installedDigest !== asset.installedSha256) {
      throw new Error(
        `${label} installed checksum ${installedDigest} != manifest ${asset.installedSha256}`
      );
    }
    return bytes;
  };

  return {
    directory: resolved,
    manifest,
    manifestBytes,
    hotBytes: await verifyAsset(manifest.hot, 'hot'),
    detailsBytes: await verifyAsset(manifest.details, 'details')
  };
}

export function assertSameRelease(staged: VerifiedRelease, source: VerifiedRelease): void {
  for (const [label, left, right] of [
    ['manifest.json', staged.manifestBytes, source.manifestBytes],
    [source.manifest.hot.file, staged.hotBytes, source.hotBytes],
    [source.manifest.details.file, staged.detailsBytes, source.detailsBytes]
  ] as const) {
    if (!Buffer.from(left).equals(Buffer.from(right))) {
      throw new Error(`Staged analyzer ${label} is not byte-identical to the qualified release`);
    }
  }
}
