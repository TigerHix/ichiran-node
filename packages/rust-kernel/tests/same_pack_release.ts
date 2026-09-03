import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';

import {
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseManifest
} from '../../core/src/release-manifest.js';

function digest(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function currentSourceIdentity(repository: string): Promise<{
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
}> {
  const git = Bun.spawnSync(['git', 'rev-parse', 'HEAD'], { cwd: repository });
  if (git.exitCode !== 0) throw new Error(new TextDecoder().decode(git.stderr));
  return {
    sourceCommit: new TextDecoder().decode(git.stdout).trim(),
    sourcesLockSha256: digest(await readFile(join(
      repository,
      'data/source-compiler-sources.lock.json'
    )))
  };
}

/** Bind a same-pack oracle corpus to an installed release from the current commit. */
export async function assertSamePackRelease(
  repository: string,
  release: string,
  hot: Uint8Array,
  details?: Uint8Array
): Promise<AnalyzerReleaseManifest> {
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(await readFile(join(release, 'manifest.json'), 'utf8')),
    text => digest(new TextEncoder().encode(text))
  );
  const identity = await currentSourceIdentity(repository);
  if (
    manifest.sourceCommit !== identity.sourceCommit
    || manifest.sourcesLockSha256 !== identity.sourcesLockSha256
  ) {
    throw new Error('same-pack release does not belong to the current source commit and lock');
  }
  if (
    hot.byteLength !== manifest.hot.installedBytes
    || digest(hot) !== manifest.hot.installedSha256
  ) {
    throw new Error('same-pack hot.bin does not match its authenticated installed identity');
  }
  if (details !== undefined && (
    details.byteLength !== manifest.details.installedBytes
    || digest(details) !== manifest.details.installedSha256
  )) {
    throw new Error('same-pack details.bin does not match its authenticated installed identity');
  }
  return manifest;
}
