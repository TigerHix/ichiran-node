import { createHash } from 'node:crypto';
import { readFile, stat } from 'node:fs/promises';
import { relative, resolve, sep } from 'node:path';

interface LockedFile {
  readonly path: string;
  readonly bytes: number;
  readonly sha256: string;
}

type LockedSource =
  | { readonly id: string; readonly kind: 'authority'; }
  | { readonly id: string; readonly kind: 'file'; readonly file: LockedFile; }
  | { readonly id: string; readonly kind: 'files'; readonly files: readonly LockedFile[]; };

export interface SourceCompilerLock {
  readonly formatVersion: 1;
  readonly baseline: {
    readonly qualifiedArtifactTag: string;
    readonly upstreamIchiranCommit: string;
    readonly upstreamDataReleaseTag: string;
  };
  readonly sources: readonly LockedSource[];
}

export interface VerifiedSourceCompilerLock {
  readonly lock: SourceCompilerLock;
  readonly bytes: Uint8Array;
  readonly sha256: string;
  readonly files: readonly {
    readonly id: string;
    readonly path: string;
    readonly bytes: number;
    readonly sha256: string;
  }[];
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${label} must be non-empty text`);
  return value;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return Number(value);
}

function digest(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^[0-9a-f]{64}$/.test(result)) throw new Error(`${label} must be a lowercase SHA-256`);
  return result;
}

function strings(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => text(item, `${label}[${index}]`));
}

function integers(value: unknown, label: string): readonly number[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => integer(item, `${label}[${index}]`));
}

function digests(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => digest(item, `${label}[${index}]`));
}

function source(value: unknown, index: number): LockedSource {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`Source ${index} must be an object`);
  }
  const row = value as Record<string, unknown>;
  const id = text(row.id, `Source ${index} id`);
  if (row.pinnedPath !== undefined) {
    return {
      id,
      kind: 'file',
      file: {
        path: text(row.pinnedPath, `Source ${id} pinnedPath`),
        bytes: integer(row.pinnedBytes, `Source ${id} pinnedBytes`),
        sha256: digest(row.pinnedSha256, `Source ${id} pinnedSha256`)
      }
    };
  }
  if (row.pinnedPaths !== undefined) {
    const pinnedPaths = strings(row.pinnedPaths, `Source ${id} pinnedPaths`);
    const pinnedBytesList = integers(row.pinnedBytes, `Source ${id} pinnedBytes`);
    const pinnedSha256s = digests(row.pinnedSha256, `Source ${id} pinnedSha256`);
    if (pinnedPaths.length !== pinnedBytesList.length || pinnedPaths.length !== pinnedSha256s.length) {
      throw new Error(`Source ${id} pinned path identities have different lengths`);
    }
    return {
      id,
      kind: 'files',
      files: pinnedPaths.map((path, itemIndex) => {
        const bytes = pinnedBytesList[itemIndex];
        const sha256 = pinnedSha256s[itemIndex];
        if (bytes === undefined || sha256 === undefined) {
          throw new Error(`Source ${id} pinned identity ${itemIndex} is missing`);
        }
        return { path, bytes, sha256 };
      })
    };
  }
  return { id, kind: 'authority' };
}

export function parseSourceCompilerLock(value: unknown): SourceCompilerLock {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error('Source compiler lock must be an object');
  }
  const row = value as Record<string, unknown>;
  if (row.formatVersion !== 1) throw new Error('Unsupported source compiler lock format');
  if (!row.baseline || typeof row.baseline !== 'object' || Array.isArray(row.baseline)) {
    throw new Error('Source compiler lock is missing its baseline');
  }
  const baseline = row.baseline as Record<string, unknown>;
  if (!Array.isArray(row.sources)) throw new Error('Source compiler lock is missing sources');
  const sources = row.sources.map(source);
  if (new Set(sources.map(item => item.id)).size !== sources.length) {
    throw new Error('Source compiler lock contains duplicate source ids');
  }
  return {
    formatVersion: 1,
    baseline: {
      qualifiedArtifactTag: text(baseline.qualifiedArtifactTag, 'Qualified artifact tag'),
      upstreamIchiranCommit: text(baseline.upstreamIchiranCommit, 'Upstream Ichiran commit'),
      upstreamDataReleaseTag: text(baseline.upstreamDataReleaseTag, 'Upstream data release tag')
    },
    sources
  };
}

function lockedPath(repository: string, path: string): string {
  if (path.startsWith('/') || path.includes('\\') || path.split('/').includes('..')) {
    throw new Error(`Locked source path is not portable: ${path}`);
  }
  const resolvedRepository = resolve(repository);
  const candidate = resolve(resolvedRepository, path);
  const within = relative(resolvedRepository, candidate);
  if (within === '..' || within.startsWith(`..${sep}`) || candidate === resolvedRepository) {
    throw new Error(`Locked source escapes the repository: ${path}`);
  }
  return candidate;
}

export async function verifySourceCompilerLock(
  repository: string,
  lockPath = 'data/source-compiler-sources.lock.json'
): Promise<VerifiedSourceCompilerLock> {
  const bytes = new Uint8Array(await readFile(lockedPath(repository, lockPath)));
  const lock = parseSourceCompilerLock(JSON.parse(new TextDecoder().decode(bytes)));
  const files: VerifiedSourceCompilerLock['files'][number][] = [];
  for (const item of lock.sources) {
    const identities = item.kind === 'authority' ? []
      : item.kind === 'file' ? [item.file]
      : item.files;
    for (const expected of identities) {
      const path = lockedPath(repository, expected.path);
      if (!(await stat(path)).isFile()) throw new Error(`Locked source is not a file: ${expected.path}`);
      const actual = new Uint8Array(await readFile(path));
      const actualSha256 = sha256(actual);
      if (actual.byteLength !== expected.bytes || actualSha256 !== expected.sha256) {
        throw new Error(
          `Locked source ${expected.path} is ${actual.byteLength} bytes ${actualSha256}; `
          + `expected ${expected.bytes} bytes ${expected.sha256}`
        );
      }
      files.push({ id: item.id, path: expected.path, bytes: actual.byteLength, sha256: actualSha256 });
    }
  }
  return { lock, bytes, sha256: sha256(bytes), files };
}
