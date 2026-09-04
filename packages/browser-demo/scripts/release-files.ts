import { createHash } from 'node:crypto';
import { execFile as execFileCallback } from 'node:child_process';
import { readFile, readdir } from 'node:fs/promises';
import { join, relative, resolve, sep } from 'node:path';
import { promisify } from 'node:util';
import { gunzipSync } from 'node:zlib';
import {
  ANALYZER_PERSISTED_MAX_BYTES,
  analyzerReadyStateSize,
  parseAnalyzerReleaseManifest,
  type AnalyzerReadyStateSize,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseManifest
} from '@ichiran/core/release';

export type ReleaseManifest = AnalyzerReleaseManifest;

export interface VerifiedRelease {
  readonly directory: string;
  readonly manifest: ReleaseManifest;
  readonly manifestBytes: Uint8Array;
  readonly hotBytes: Uint8Array;
  readonly lexiconBytes: Uint8Array;
  readonly localeBytes: Readonly<Record<string, Uint8Array>>;
}

const execFile = promisify(execFileCallback);
export const QUALIFIED_BASELINE_ARTIFACT = 'portable-core-260118-baseline';

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

export async function currentSourceIdentity(
  repositoryRoot: string,
  sourceLock = 'data/source-compiler-sources.lock.json'
): Promise<{
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
}> {
  const { stdout } = await execFile('git', ['-C', repositoryRoot, 'rev-parse', 'HEAD'], {
    encoding: 'utf8'
  });
  const sourceCommit = stdout.trim();
  if (!/^[0-9a-f]{40}$/.test(sourceCommit)) throw new Error('Current Git HEAD is invalid');
  const resolvedRepository = resolve(repositoryRoot);
  const lockPath = resolve(resolvedRepository, sourceLock);
  const within = relative(resolvedRepository, lockPath);
  if (within === '..' || within.startsWith(`..${sep}`) || within.length === 0) {
    throw new Error('Source lock must be a repository-relative file');
  }
  const lock = await readFile(lockPath);
  return { sourceCommit, sourcesLockSha256: sha256(lock) };
}

export interface ReleaseVerificationOptions {
  readonly qualifiedArtifact?: string | undefined;
  readonly sourceLock?: string | undefined;
}

export function assertAnalyzerReadyStateSize(
  manifest: ReleaseManifest
): AnalyzerReadyStateSize {
  const size = analyzerReadyStateSize(manifest);
  if (size.persistedBytes > ANALYZER_PERSISTED_MAX_BYTES) {
    throw new Error(
      `Persisted release is ${size.persistedBytes} bytes; limit is ${ANALYZER_PERSISTED_MAX_BYTES}`
    );
  }
  return size;
}

export async function verifyAnalyzerRelease(
  directory: string,
  repositoryRoot: string,
  options: ReleaseVerificationOptions = {}
): Promise<VerifiedRelease> {
  const resolved = resolve(directory);
  const manifestBytes = await readFile(join(resolved, 'manifest.json'));
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(new TextDecoder().decode(manifestBytes)),
    text => createHash('sha256').update(text).digest('hex')
  );

  if (options.qualifiedArtifact !== undefined) {
    const qualifiedArtifact = options.qualifiedArtifact;
    if (qualifiedArtifact !== QUALIFIED_BASELINE_ARTIFACT) {
      throw new Error(`Unknown qualified analyzer artifact ${qualifiedArtifact}`);
    }
    throw new Error(
      `Qualified artifact ${qualifiedArtifact} predates analyzer manifest format 2`
    );
  } else {
    const identity = await currentSourceIdentity(repositoryRoot, options.sourceLock);
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
  }

  const verifyAsset = async (asset: AnalyzerReleaseAsset, label: string): Promise<Uint8Array> => {
    const bytes = await readFile(join(resolved, asset.file));
    if (bytes.byteLength !== asset.downloadBytes) {
      throw new Error(`${label} download size ${bytes.byteLength} != manifest ${asset.downloadBytes}`);
    }
    const digest = sha256(bytes);
    if (digest !== asset.downloadSha256) {
      throw new Error(`${label} download checksum ${digest} != manifest ${asset.downloadSha256}`);
    }
    const installed = asset.encoding === 'gzip'
      ? new Uint8Array(gunzipSync(bytes))
      : bytes.slice();
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

  const names = (await readdir(resolved)).sort();
  const expectedNames = [
    'manifest.json',
    manifest.hot.file,
    manifest.lexicon.file,
    ...Object.values(manifest.locales).map(asset => asset.file)
  ];
  if (names.includes('stats.json')) expectedNames.push('stats.json');
  expectedNames.sort();
  if (names.join('\n') !== expectedNames.join('\n')) {
    throw new Error(`Analyzer release has unexpected files: ${names.join(', ')}`);
  }

  const localeBytes = Object.fromEntries(await Promise.all(
    Object.entries(manifest.locales).map(async ([locale, asset]) => (
      [locale, await verifyAsset(asset, `locale ${locale}`)] as const
    ))
  ));
  return {
    directory: resolved,
    manifest,
    manifestBytes,
    hotBytes: await verifyAsset(manifest.hot, 'hot'),
    lexiconBytes: await verifyAsset(manifest.lexicon, 'lexicon'),
    localeBytes
  };
}

export function assertSameRelease(staged: VerifiedRelease, source: VerifiedRelease): void {
  const assets: Array<readonly [string, Uint8Array, Uint8Array]> = [
    ['manifest.json', staged.manifestBytes, source.manifestBytes],
    [source.manifest.hot.file, staged.hotBytes, source.hotBytes],
    [source.manifest.lexicon.file, staged.lexiconBytes, source.lexiconBytes],
    ...Object.keys(source.manifest.locales).sort().map(locale => [
      source.manifest.locales[locale]!.file,
      staged.localeBytes[locale]!,
      source.localeBytes[locale]!
    ] as const)
  ];
  for (const [label, left, right] of assets) {
    if (!Buffer.from(left).equals(Buffer.from(right))) {
      throw new Error(`Staged analyzer ${label} is not byte-identical to the qualified release`);
    }
  }
}
