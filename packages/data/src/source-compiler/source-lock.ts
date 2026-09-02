import { createHash } from 'node:crypto';
import { readFile, stat } from 'node:fs/promises';
import { relative, resolve, sep } from 'node:path';

interface LockedFile {
  readonly role: SourceCompilerInputRole;
  readonly path: string;
  readonly bytes: number;
  readonly sha256: string;
}

export const QUALIFIED_BASELINE_JMDICT_SHA256 =
  '92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7';

type LockedSource =
  | { readonly id: string; readonly kind: 'authority'; }
  | { readonly id: string; readonly kind: 'file'; readonly file: LockedFile; }
  | { readonly id: string; readonly kind: 'files'; readonly files: readonly LockedFile[]; };

export const SOURCE_COMPILER_INPUT_ROLES = [
  'jmdict',
  'kanjidic',
  'extra',
  'municipality',
  'ward',
  'chronologicalErrata',
  'compatibility',
  'kwpos',
  'conjo'
] as const;

export type SourceCompilerInputRole = typeof SOURCE_COMPILER_INPUT_ROLES[number];

const SOURCE_COMPILER_INPUT_ROLE_SET = new Set<string>(SOURCE_COMPILER_INPUT_ROLES);

const LOCK_KEYS = new Set(['formatVersion', 'baseline', 'archive', 'transition', 'sources']);
const BASELINE_KEYS = new Set([
  'repository', 'startingCommit', 'qualifiedArtifactTag',
  'upstreamIchiranCommit', 'upstreamDataReleaseTag'
]);
const ARCHIVE_KEYS = new Set(['repository', 'commit', 'date', 'acquisitionScript']);
const TRANSITION_KEYS = new Set(['date', 'scope', 'acquisitionScript']);
const SOURCE_KEYS = new Set([
  'id', 'kind', 'role', 'roles', 'authoritativeUrl', 'historicalCaptureUrl',
  'archivePath', 'archiveRepository', 'archiveCommit', 'archivePatch',
  'archivePatchBytes', 'archivePatchSha256', 'upstreamIdentity', 'upstreamPaths',
  'upstreamBytes', 'upstreamSha256', 'uncompressedBytes', 'uncompressedSha256',
  'pinnedPath', 'pinnedPaths', 'pinnedBytes', 'pinnedSha256', 'license', 'licenseUrl',
  'attribution', 'generatedBy', 'authority', 'rows'
]);

export interface SourceCompilerLock {
  readonly formatVersion: 1;
  readonly baseline: {
    readonly repository: string;
    readonly startingCommit: string;
    readonly qualifiedArtifactTag: string;
    readonly upstreamIchiranCommit: string;
    readonly upstreamDataReleaseTag: string;
  };
  readonly archive?: {
    readonly repository: string;
    readonly commit: string;
    readonly date: string;
    readonly acquisitionScript: string;
  };
  readonly transition?: {
    readonly date: string;
    readonly scope: string;
    readonly acquisitionScript: string;
  };
  readonly sources: readonly LockedSource[];
}

export interface VerifiedSourceCompilerLock {
  readonly lock: SourceCompilerLock;
  readonly bytes: Uint8Array;
  readonly sha256: string;
  readonly files: readonly {
    readonly id: string;
    readonly role: SourceCompilerInputRole;
    readonly path: string;
    readonly bytes: number;
    readonly sha256: string;
  }[];
  readonly inputs: Readonly<Record<SourceCompilerInputRole, {
    readonly id: string;
    readonly role: SourceCompilerInputRole;
    readonly path: string;
    readonly absolutePath: string;
    readonly bytes: number;
    readonly sha256: string;
  }>>;
}

export function assertSourceCompilerReleaseMode(
  mode: 'baseline' | 'update',
  jmdict: { readonly sha256: string; }
): void {
  const isQualifiedBaseline = jmdict.sha256 === QUALIFIED_BASELINE_JMDICT_SHA256;
  if (mode === 'baseline' && !isQualifiedBaseline) {
    throw new Error('Baseline mode requires the qualified baseline JMdict identity');
  }
  if (mode === 'update' && isQualifiedBaseline) {
    throw new Error('Update mode cannot use the qualified baseline JMdict identity');
  }
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

function commit(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^[0-9a-f]{40}$/.test(result)) throw new Error(`${label} must be a lowercase Git commit`);
  return result;
}

function integers(value: unknown, label: string): readonly number[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => integer(item, `${label}[${index}]`));
}

function digests(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => digest(item, `${label}[${index}]`));
}

function inputRole(value: unknown, label: string): SourceCompilerInputRole {
  const result = text(value, label);
  if (!SOURCE_COMPILER_INPUT_ROLE_SET.has(result)) {
    throw new Error(`${label} names unexpected compiler input role ${result}`);
  }
  return result as SourceCompilerInputRole;
}

function inputRoles(value: unknown, label: string): readonly SourceCompilerInputRole[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => inputRole(item, `${label}[${index}]`));
}

function portableInputPath(value: unknown, label: string): string {
  const result = text(value, label);
  const segments = result.split('/');
  if (result.startsWith('/') || result.includes('\\')
    || segments.some(segment => segment === '' || segment === '.' || segment === '..')) {
    throw new Error(`${label} must be a normalized repository-relative path`);
  }
  return result;
}

function portableInputPaths(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => portableInputPath(item, `${label}[${index}]`));
}

function assertKnownKeys(row: Record<string, unknown>, keys: ReadonlySet<string>, label: string): void {
  const unknown = Object.keys(row).filter(key => !keys.has(key));
  if (unknown.length > 0) throw new Error(`${label} has unknown fields: ${unknown.join(', ')}`);
}

function requiredRecord(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function assertExactKeys(row: Record<string, unknown>, keys: ReadonlySet<string>, label: string): void {
  assertKnownKeys(row, keys, label);
  const missing = [...keys].filter(key => !(key in row));
  if (missing.length > 0) throw new Error(`${label} is missing fields: ${missing.join(', ')}`);
}

function source(value: unknown, index: number): LockedSource {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`Source ${index} must be an object`);
  }
  const row = value as Record<string, unknown>;
  assertKnownKeys(row, SOURCE_KEYS, `Source ${index}`);
  const id = text(row.id, `Source ${index} id`);
  if (row.pinnedPath !== undefined) {
    if (row.pinnedPaths !== undefined || row.roles !== undefined) {
      throw new Error(`Source ${id} mixes single-file and multi-file role assignments`);
    }
    return {
      id,
      kind: 'file',
      file: {
        role: inputRole(row.role, `Source ${id} role`),
        path: portableInputPath(row.pinnedPath, `Source ${id} pinnedPath`),
        bytes: integer(row.pinnedBytes, `Source ${id} pinnedBytes`),
        sha256: digest(row.pinnedSha256, `Source ${id} pinnedSha256`)
      }
    };
  }
  if (row.pinnedPaths !== undefined) {
    if (row.role !== undefined) {
      throw new Error(`Source ${id} mixes single-file and multi-file role assignments`);
    }
    const pinnedPaths = portableInputPaths(row.pinnedPaths, `Source ${id} pinnedPaths`);
    const pinnedBytesList = integers(row.pinnedBytes, `Source ${id} pinnedBytes`);
    const pinnedSha256s = digests(row.pinnedSha256, `Source ${id} pinnedSha256`);
    const roles = inputRoles(row.roles, `Source ${id} roles`);
    if (pinnedPaths.length !== pinnedBytesList.length
      || pinnedPaths.length !== pinnedSha256s.length
      || pinnedPaths.length !== roles.length) {
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
        const role = roles[itemIndex];
        if (role === undefined) throw new Error(`Source ${id} pinned role ${itemIndex} is missing`);
        return { role, path, bytes, sha256 };
      })
    };
  }
  if (row.role !== undefined || row.roles !== undefined) {
    throw new Error(`Authority source ${id} cannot assign a compiler input role`);
  }
  if (row.pinnedBytes !== undefined || row.pinnedSha256 !== undefined) {
    throw new Error(`Authority source ${id} has an incomplete pinned file identity`);
  }
  return { id, kind: 'authority' };
}

export function parseSourceCompilerLock(value: unknown): SourceCompilerLock {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error('Source compiler lock must be an object');
  }
  const row = value as Record<string, unknown>;
  assertKnownKeys(row, LOCK_KEYS, 'Source compiler lock');
  if (row.formatVersion !== 1) throw new Error('Unsupported source compiler lock format');
  const baseline = requiredRecord(row.baseline, 'Source compiler lock baseline');
  assertExactKeys(baseline, BASELINE_KEYS, 'Source compiler lock baseline');
  const archive = row.archive === undefined
    ? undefined : requiredRecord(row.archive, 'Source compiler lock archive');
  if (archive) assertExactKeys(archive, ARCHIVE_KEYS, 'Source compiler lock archive');
  const transition = row.transition === undefined
    ? undefined : requiredRecord(row.transition, 'Source compiler lock transition');
  if (transition) {
    assertExactKeys(transition, TRANSITION_KEYS, 'Source compiler lock transition');
  }
  if (!Array.isArray(row.sources)) throw new Error('Source compiler lock is missing sources');
  const sources = row.sources.map(source);
  if (new Set(sources.map(item => item.id)).size !== sources.length) {
    throw new Error('Source compiler lock contains duplicate source ids');
  }
  const assignedRoles = sources.flatMap(item => item.kind === 'authority'
    ? []
    : item.kind === 'file' ? [item.file.role] : item.files.map(file => file.role));
  const duplicateRoles = SOURCE_COMPILER_INPUT_ROLES.filter(
    role => assignedRoles.filter(value => value === role).length > 1
  );
  if (duplicateRoles.length > 0) {
    throw new Error(`Source compiler lock assigns duplicate roles: ${duplicateRoles.join(', ')}`);
  }
  const missingRoles = SOURCE_COMPILER_INPUT_ROLES.filter(role => !assignedRoles.includes(role));
  if (missingRoles.length > 0) {
    throw new Error(`Source compiler lock is missing required roles: ${missingRoles.join(', ')}`);
  }
  const assignedPaths = sources.flatMap(item => item.kind === 'authority'
    ? []
    : item.kind === 'file' ? [item.file.path] : item.files.map(file => file.path));
  const duplicatePaths = [...new Set(assignedPaths.filter(
    (path, index) => assignedPaths.indexOf(path) !== index
  ))];
  if (duplicatePaths.length > 0) {
    throw new Error(`Source compiler lock assigns one file to multiple roles: ${duplicatePaths.join(', ')}`);
  }
  return {
    formatVersion: 1,
    baseline: {
      repository: text(baseline.repository, 'Baseline repository'),
      startingCommit: commit(baseline.startingCommit, 'Baseline starting commit'),
      qualifiedArtifactTag: text(baseline.qualifiedArtifactTag, 'Qualified artifact tag'),
      upstreamIchiranCommit: commit(baseline.upstreamIchiranCommit, 'Upstream Ichiran commit'),
      upstreamDataReleaseTag: text(baseline.upstreamDataReleaseTag, 'Upstream data release tag')
    },
    ...(archive ? {
      archive: {
        repository: text(archive.repository, 'Archive repository'),
        commit: commit(archive.commit, 'Archive commit'),
        date: text(archive.date, 'Archive date'),
        acquisitionScript: portableInputPath(
          archive.acquisitionScript,
          'Archive acquisition script'
        )
      }
    } : {}),
    ...(transition ? {
      transition: {
        date: text(transition.date, 'Transition date'),
        scope: text(transition.scope, 'Transition scope'),
        acquisitionScript: portableInputPath(
          transition.acquisitionScript,
          'Transition acquisition script'
        )
      }
    } : {}),
    sources
  };
}

export function sourceCompilerInputPaths(lock: SourceCompilerLock): readonly string[] {
  return lock.sources.flatMap(item => item.kind === 'authority'
    ? []
    : item.kind === 'file' ? [item.file.path] : item.files.map(file => file.path));
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
  const inputs = {} as Record<SourceCompilerInputRole, VerifiedSourceCompilerLock['inputs'][SourceCompilerInputRole]>;
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
      const file = {
        id: item.id,
        role: expected.role,
        path: expected.path,
        absolutePath: path,
        bytes: actual.byteLength,
        sha256: actualSha256
      };
      inputs[expected.role] = file;
      files.push({
        id: file.id,
        role: file.role,
        path: file.path,
        bytes: file.bytes,
        sha256: file.sha256
      });
    }
  }
  for (const role of SOURCE_COMPILER_INPUT_ROLES) {
    if (!inputs[role]) throw new Error(`Verified source lock has no ${role} input`);
  }
  return { lock, bytes, sha256: sha256(bytes), files, inputs };
}
