import { createHash } from 'node:crypto';
import { execFile as execFileCallback } from 'node:child_process';
import { readFile, readdir } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { promisify } from 'node:util';
import { gunzipSync } from 'node:zlib';
import {
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseManifest
} from '@ichiran/core/release';

export type ReleaseManifest = AnalyzerReleaseManifest;

export interface VerifiedRelease {
  readonly directory: string;
  readonly manifest: ReleaseManifest;
  readonly manifestBytes: Uint8Array;
  readonly hotBytes: Uint8Array;
  readonly detailsBytes: Uint8Array;
}

const execFile = promisify(execFileCallback);
export const QUALIFIED_BASELINE_ARTIFACT = 'portable-core-260118-baseline';

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
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
  const lock = await readFile(join(repositoryRoot, 'data', 'source-compiler-sources.lock.json'));
  return { sourceCommit, sourcesLockSha256: sha256(lock) };
}

export async function verifyAnalyzerRelease(
  directory: string,
  repositoryRoot: string,
  qualifiedArtifact?: string
): Promise<VerifiedRelease> {
  const resolved = resolve(directory);
  const manifestBytes = await readFile(join(resolved, 'manifest.json'));
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(new TextDecoder().decode(manifestBytes)),
    text => createHash('sha256').update(text).digest('hex')
  );

  if (qualifiedArtifact !== undefined) {
    if (qualifiedArtifact !== QUALIFIED_BASELINE_ARTIFACT) {
      throw new Error(`Unknown qualified analyzer artifact ${qualifiedArtifact}`);
    }
    if (
      manifest.packVersion !== 'ichiran-260118'
      || manifest.sourceCommit !== '29ec534ede2b4c90dcddb18f87a84089c24df9de'
      || manifest.sourcesLockSha256 !== '80dc7c907d688a5ecb0bbd8b23b889f47cb3a28f8484f80e8dc4737bb090c070'
      || manifest.manifestSha256 !== 'e245cde362ade8b7e6f30f063ea93f42e551168f8c28a7d9fd0b13c48085b258'
      || manifest.hot.downloadSha256 !== '35d02c84d4cc531d299d7d5530994351b75bdba429d5276c20bc2f67cdc8d6d7'
      || manifest.hot.installedSha256 !== '61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0'
      || manifest.details.downloadSha256 !== 'ad10bc4876d9a05224f62f5b438080ea1ff4e6a88ab3090be0f871035e95918a'
      || manifest.details.installedSha256 !== '0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151'
    ) {
      throw new Error(`Analyzer release does not match qualified artifact ${qualifiedArtifact}`);
    }
  } else {
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
  const expectedNames = ['manifest.json', manifest.hot.file, manifest.details.file];
  if (names.includes('stats.json')) expectedNames.push('stats.json');
  expectedNames.sort();
  if (names.join('\n') !== expectedNames.join('\n')) {
    throw new Error(`Analyzer release has unexpected files: ${names.join(', ')}`);
  }

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
