import { createHash } from 'node:crypto';
import { mkdir, readFile, stat, writeFile } from 'node:fs/promises';
import { basename, join, relative, resolve, sep } from 'node:path';
import { gunzipSync } from 'node:zlib';

import { loadQualifiedErrata } from './chronological-errata-ledger.js';
import { loadSourceCompatibility } from './compatibility.js';

interface LockedFile {
  readonly role: SourceCompilerInputRole;
  readonly path: string;
  readonly bytes: number;
  readonly sha256: string;
}

export const QUALIFIED_BASELINE_JMDICT_SHA256 =
  '92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7';

interface LockedSourceBase<Kind extends string> {
  readonly id: string;
  readonly kind: Kind;
}

type JmdictLockedSource = LockedSourceBase<'jmdict'> & {
  readonly file: LockedFile;
  readonly authoritativeUrl: string;
  readonly upstreamIdentity: string;
  readonly uncompressedBytes: number;
  readonly uncompressedSha256: string;
  readonly license: string;
  readonly licenseUrl: string;
  readonly attribution: string;
} & ({
  readonly archivePath: string;
} | {
  /** Acquisition provenance checked by the transition acquisition command. */
  readonly archiveRepository: string;
  readonly archiveCommit: string;
  readonly archivePatch: string;
  readonly archivePatchBytes: number;
  readonly archivePatchSha256: string;
});

interface KanjidicLockedSource extends LockedSourceBase<'kanjidic2'> {
  readonly file: LockedFile;
  readonly authoritativeUrl: string;
  readonly historicalCaptureUrl: string;
  readonly upstreamIdentity: string;
  readonly uncompressedBytes: number;
  readonly uncompressedSha256: string;
  readonly license: string;
  readonly licenseUrl: string;
  readonly attribution: string;
}

interface CustomEntriesLockedSource extends LockedSourceBase<'custom-entries'> {
  readonly files: readonly LockedFile[];
  readonly authoritativeUrl: string;
  readonly license: string;
}

interface IntendedBehaviorLockedSource extends LockedSourceBase<'intended-behavior'> {
  readonly authoritativeUrl: string;
  readonly upstreamPaths: readonly string[];
  readonly upstreamBytes: readonly number[];
  readonly upstreamSha256: readonly string[];
  readonly license: string;
}

interface SemanticLedgerLockedSource extends LockedSourceBase<'semantic-ledger'> {
  readonly file: LockedFile;
  readonly generatedBy: string;
  readonly authority: string;
  readonly rows: number;
  readonly license: string;
}

interface CompatibilityLedgerLockedSource extends LockedSourceBase<'compatibility-ledger'> {
  readonly file: LockedFile;
  readonly authority: string;
  readonly rows: number;
}

interface ConjugationRulesLockedSource extends LockedSourceBase<'conjugation-rules'> {
  readonly files: readonly LockedFile[];
  readonly authoritativeUrl: string;
  readonly license: string;
}

export type SourceCompilerLockedSource =
  | JmdictLockedSource
  | KanjidicLockedSource
  | CustomEntriesLockedSource
  | IntendedBehaviorLockedSource
  | SemanticLedgerLockedSource
  | CompatibilityLedgerLockedSource
  | ConjugationRulesLockedSource;

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
const JMDICT_ARCHIVE_KEYS = new Set([
  'id', 'kind', 'role', 'authoritativeUrl', 'archivePath', 'upstreamIdentity',
  'uncompressedBytes', 'uncompressedSha256', 'pinnedPath', 'pinnedBytes',
  'pinnedSha256', 'license', 'licenseUrl', 'attribution'
]);
const JMDICT_PATCH_KEYS = new Set([
  'id', 'kind', 'role', 'authoritativeUrl', 'archiveRepository', 'archiveCommit',
  'archivePatch', 'archivePatchBytes', 'archivePatchSha256', 'upstreamIdentity',
  'uncompressedBytes', 'uncompressedSha256', 'pinnedPath', 'pinnedBytes',
  'pinnedSha256', 'license', 'licenseUrl', 'attribution'
]);
const KANJIDIC_KEYS = new Set([
  'id', 'kind', 'role', 'authoritativeUrl', 'historicalCaptureUrl', 'upstreamIdentity',
  'uncompressedBytes', 'uncompressedSha256', 'pinnedPath', 'pinnedBytes',
  'pinnedSha256', 'license', 'licenseUrl', 'attribution'
]);
const CUSTOM_SINGLE_KEYS = new Set([
  'id', 'kind', 'role', 'authoritativeUrl', 'pinnedPath', 'pinnedBytes',
  'pinnedSha256', 'license'
]);
const CUSTOM_MULTIPLE_KEYS = new Set([
  'id', 'kind', 'roles', 'authoritativeUrl', 'pinnedPaths', 'pinnedBytes',
  'pinnedSha256', 'license'
]);
const INTENDED_BEHAVIOR_KEYS = new Set([
  'id', 'kind', 'authoritativeUrl', 'upstreamPaths', 'upstreamBytes',
  'upstreamSha256', 'license'
]);
const SEMANTIC_LEDGER_KEYS = new Set([
  'id', 'kind', 'role', 'pinnedPath', 'pinnedBytes', 'pinnedSha256',
  'generatedBy', 'authority', 'rows', 'license'
]);
const COMPATIBILITY_LEDGER_KEYS = new Set([
  'id', 'kind', 'role', 'pinnedPath', 'pinnedBytes', 'pinnedSha256', 'rows', 'authority'
]);
const CONJUGATION_RULES_KEYS = new Set([
  'id', 'kind', 'roles', 'authoritativeUrl', 'pinnedPaths', 'pinnedBytes',
  'pinnedSha256', 'license'
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
  readonly sources: readonly SourceCompilerLockedSource[];
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
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new Error(`${label} must be non-empty text`);
  }
  return value;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return Number(value);
}

function positiveInteger(value: unknown, label: string): number {
  const result = integer(value, label);
  if (result === 0) throw new Error(`${label} must be positive`);
  return result;
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
  if (value.length === 0) throw new Error(`${label} must not be empty`);
  return value.map((item, index) => positiveInteger(item, `${label}[${index}]`));
}

function digests(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  if (value.length === 0) throw new Error(`${label} must not be empty`);
  return value.map((item, index) => digest(item, `${label}[${index}]`));
}

function webUrl(value: unknown, label: string): string {
  const result = text(value, label);
  let parsed: URL;
  try {
    parsed = new URL(result);
  } catch {
    throw new Error(`${label} must be an absolute HTTP(S) URL`);
  }
  if ((parsed.protocol !== 'http:' && parsed.protocol !== 'https:') || parsed.host.length === 0) {
    throw new Error(`${label} must be an absolute HTTP(S) URL`);
  }
  return result;
}

function isoDate(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(result)) {
    throw new Error(`${label} must be an ISO YYYY-MM-DD date`);
  }
  const parsed = new Date(`${result}T00:00:00.000Z`);
  if (Number.isNaN(parsed.getTime()) || parsed.toISOString().slice(0, 10) !== result) {
    throw new Error(`${label} must be an ISO YYYY-MM-DD date`);
  }
  return result;
}

function jmdictIdentity(value: unknown, label: string): string {
  const result = text(value, label);
  const match = /^JMdict created: (\d{4}-\d{2}-\d{2})$/.exec(result);
  if (!match) throw new Error(`${label} must name the JMdict creation date`);
  isoDate(match[1], `${label} creation date`);
  return result;
}

function kanjidicIdentity(value: unknown, label: string): string {
  const result = text(value, label);
  const match = /^file_version ([^;]+); database_version ([^;]+); date_of_creation (\d{4}-\d{2}-\d{2})$/.exec(result);
  if (!match) throw new Error(`${label} must name the Kanjidic2 header identity`);
  isoDate(match[3], `${label} creation date`);
  return result;
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
  if (value.length === 0) throw new Error(`${label} must not be empty`);
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
  if (value.length === 0) throw new Error(`${label} must not be empty`);
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

function singleFile(
  row: Record<string, unknown>,
  id: string,
  expectedRole: SourceCompilerInputRole
): LockedFile {
  const role = inputRole(row.role, `Source ${id} role`);
  if (role !== expectedRole) {
    throw new Error(`Source ${id} kind requires role ${expectedRole}, got ${role}`);
  }
  return {
    role,
    path: portableInputPath(row.pinnedPath, `Source ${id} pinnedPath`),
    bytes: positiveInteger(row.pinnedBytes, `Source ${id} pinnedBytes`),
    sha256: digest(row.pinnedSha256, `Source ${id} pinnedSha256`)
  };
}

function multipleFiles(
  row: Record<string, unknown>,
  id: string,
  expectedRoles: readonly SourceCompilerInputRole[]
): readonly LockedFile[] {
  const roles = inputRoles(row.roles, `Source ${id} roles`);
  if (roles.length !== expectedRoles.length
    || roles.some((role, index) => role !== expectedRoles[index])) {
    throw new Error(`Source ${id} kind requires roles ${expectedRoles.join(', ')}`);
  }
  const paths = portableInputPaths(row.pinnedPaths, `Source ${id} pinnedPaths`);
  const bytes = integers(row.pinnedBytes, `Source ${id} pinnedBytes`);
  const sha256s = digests(row.pinnedSha256, `Source ${id} pinnedSha256`);
  if (paths.length !== roles.length || bytes.length !== roles.length || sha256s.length !== roles.length) {
    throw new Error(`Source ${id} pinned path identities have different lengths`);
  }
  return paths.map((path, index) => ({
    role: roles[index]!,
    path,
    bytes: bytes[index]!,
    sha256: sha256s[index]!
  }));
}

function pairedUpstreamIdentities(row: Record<string, unknown>, id: string): {
  readonly paths: readonly string[];
  readonly bytes: readonly number[];
  readonly sha256: readonly string[];
} {
  const paths = portableInputPaths(row.upstreamPaths, `Source ${id} upstreamPaths`);
  const bytes = integers(row.upstreamBytes, `Source ${id} upstreamBytes`);
  const sha256 = digests(row.upstreamSha256, `Source ${id} upstreamSha256`);
  if (paths.length !== bytes.length || paths.length !== sha256.length) {
    throw new Error(`Source ${id} upstream identities have different lengths`);
  }
  return { paths, bytes, sha256 };
}

function source(value: unknown, index: number): SourceCompilerLockedSource {
  const row = requiredRecord(value, `Source ${index}`);
  const id = text(row.id, `Source ${index} id`);
  const kind = text(row.kind, `Source ${id} kind`);
  switch (kind) {
    case 'jmdict': {
      const archiveCapture = row.archivePath !== undefined;
      assertExactKeys(
        row,
        archiveCapture ? JMDICT_ARCHIVE_KEYS : JMDICT_PATCH_KEYS,
        `Source ${id}`
      );
      const common = {
        id,
        kind,
        file: singleFile(row, id, 'jmdict'),
        authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
        upstreamIdentity: jmdictIdentity(row.upstreamIdentity, `Source ${id} upstreamIdentity`),
        uncompressedBytes: positiveInteger(
          row.uncompressedBytes,
          `Source ${id} uncompressedBytes`
        ),
        uncompressedSha256: digest(
          row.uncompressedSha256,
          `Source ${id} uncompressedSha256`
        ),
        license: text(row.license, `Source ${id} license`),
        licenseUrl: webUrl(row.licenseUrl, `Source ${id} licenseUrl`),
        attribution: text(row.attribution, `Source ${id} attribution`)
      } as const;
      return archiveCapture ? {
        ...common,
        archivePath: portableInputPath(row.archivePath, `Source ${id} archivePath`)
      } : {
        ...common,
        archiveRepository: webUrl(
          row.archiveRepository,
          `Source ${id} archiveRepository`
        ),
        archiveCommit: commit(row.archiveCommit, `Source ${id} archiveCommit`),
        archivePatch: portableInputPath(row.archivePatch, `Source ${id} archivePatch`),
        archivePatchBytes: positiveInteger(
          row.archivePatchBytes,
          `Source ${id} archivePatchBytes`
        ),
        archivePatchSha256: digest(
          row.archivePatchSha256,
          `Source ${id} archivePatchSha256`
        )
      };
    }
    case 'kanjidic2':
      assertExactKeys(row, KANJIDIC_KEYS, `Source ${id}`);
      return {
        id,
        kind,
        file: singleFile(row, id, 'kanjidic'),
        authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
        historicalCaptureUrl: webUrl(
          row.historicalCaptureUrl,
          `Source ${id} historicalCaptureUrl`
        ),
        upstreamIdentity: kanjidicIdentity(
          row.upstreamIdentity,
          `Source ${id} upstreamIdentity`
        ),
        uncompressedBytes: positiveInteger(
          row.uncompressedBytes,
          `Source ${id} uncompressedBytes`
        ),
        uncompressedSha256: digest(
          row.uncompressedSha256,
          `Source ${id} uncompressedSha256`
        ),
        license: text(row.license, `Source ${id} license`),
        licenseUrl: webUrl(row.licenseUrl, `Source ${id} licenseUrl`),
        attribution: text(row.attribution, `Source ${id} attribution`)
      };
    case 'custom-entries':
      if (row.pinnedPath !== undefined) {
        assertExactKeys(row, CUSTOM_SINGLE_KEYS, `Source ${id}`);
        return {
          id,
          kind,
          files: [singleFile(row, id, 'extra')],
          authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
          license: text(row.license, `Source ${id} license`)
        };
      }
      assertExactKeys(row, CUSTOM_MULTIPLE_KEYS, `Source ${id}`);
      return {
        id,
        kind,
        files: multipleFiles(row, id, ['municipality', 'ward']),
        authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
        license: text(row.license, `Source ${id} license`)
      };
    case 'intended-behavior': {
      assertExactKeys(row, INTENDED_BEHAVIOR_KEYS, `Source ${id}`);
      const upstream = pairedUpstreamIdentities(row, id);
      return {
        id,
        kind,
        authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
        upstreamPaths: upstream.paths,
        upstreamBytes: upstream.bytes,
        upstreamSha256: upstream.sha256,
        license: text(row.license, `Source ${id} license`)
      };
    }
    case 'semantic-ledger':
      assertExactKeys(row, SEMANTIC_LEDGER_KEYS, `Source ${id}`);
      return {
        id,
        kind,
        file: singleFile(row, id, 'chronologicalErrata'),
        generatedBy: portableInputPath(row.generatedBy, `Source ${id} generatedBy`),
        authority: text(row.authority, `Source ${id} authority`),
        rows: positiveInteger(row.rows, `Source ${id} rows`),
        license: text(row.license, `Source ${id} license`)
      };
    case 'compatibility-ledger':
      assertExactKeys(row, COMPATIBILITY_LEDGER_KEYS, `Source ${id}`);
      return {
        id,
        kind,
        file: singleFile(row, id, 'compatibility'),
        authority: text(row.authority, `Source ${id} authority`),
        rows: positiveInteger(row.rows, `Source ${id} rows`)
      };
    case 'conjugation-rules':
      assertExactKeys(row, CONJUGATION_RULES_KEYS, `Source ${id}`);
      return {
        id,
        kind,
        files: multipleFiles(row, id, ['kwpos', 'conjo']),
        authoritativeUrl: webUrl(row.authoritativeUrl, `Source ${id} authoritativeUrl`),
        license: text(row.license, `Source ${id} license`)
      };
    default:
      throw new Error(`Source ${id} kind is unsupported: ${kind}`);
  }
}

function sourceFiles(source: SourceCompilerLockedSource): readonly LockedFile[] {
  switch (source.kind) {
    case 'jmdict':
    case 'kanjidic2':
    case 'semantic-ledger':
    case 'compatibility-ledger':
      return [source.file];
    case 'custom-entries':
    case 'conjugation-rules':
      return source.files;
    case 'intended-behavior':
      return [];
  }
}

function assertJmdictLifecycle(
  source: JmdictLockedSource,
  archive: SourceCompilerLock['archive'],
  transition: SourceCompilerLock['transition']
): void {
  const sourceDate = source.upstreamIdentity.slice('JMdict created: '.length);
  const archiveCapture = 'archivePath' in source;
  if (archiveCapture) {
    if (!archive || transition) {
      throw new Error('Archived JMdict capture requires archive provenance and no transition');
    }
    if (archive.date !== sourceDate
      || !source.archivePath.endsWith(`${archive.date.replaceAll('-', '/')}.patch.br`)) {
      throw new Error('Archived JMdict capture contradicts the archive date');
    }
    return;
  }
  if (!transition || archive) {
    throw new Error('Patched JMdict transition requires transition provenance and no archive');
  }
  if (transition.date !== sourceDate
    || !source.archivePatch.endsWith(`${transition.date.replaceAll('-', '/')}.patch.br`)) {
    throw new Error('Patched JMdict capture contradicts the transition date');
  }
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
  const archiveInput = row.archive === undefined
    ? undefined : requiredRecord(row.archive, 'Source compiler lock archive');
  if (archiveInput) {
    assertExactKeys(archiveInput, ARCHIVE_KEYS, 'Source compiler lock archive');
  }
  const archive = archiveInput ? {
    repository: webUrl(archiveInput.repository, 'Archive repository'),
    commit: commit(archiveInput.commit, 'Archive commit'),
    date: isoDate(archiveInput.date, 'Archive date'),
    acquisitionScript: portableInputPath(
      archiveInput.acquisitionScript,
      'Archive acquisition script'
    )
  } : undefined;
  const transitionInput = row.transition === undefined
    ? undefined : requiredRecord(row.transition, 'Source compiler lock transition');
  if (transitionInput) {
    assertExactKeys(transitionInput, TRANSITION_KEYS, 'Source compiler lock transition');
  }
  const transition = transitionInput ? {
    date: isoDate(transitionInput.date, 'Transition date'),
    scope: text(transitionInput.scope, 'Transition scope'),
    acquisitionScript: portableInputPath(
      transitionInput.acquisitionScript,
      'Transition acquisition script'
    )
  } : undefined;
  if (!Array.isArray(row.sources)) throw new Error('Source compiler lock is missing sources');
  const sources = row.sources.map(source);
  if (new Set(sources.map(item => item.id)).size !== sources.length) {
    throw new Error('Source compiler lock contains duplicate source ids');
  }
  const intendedBehavior = sources.filter(item => item.kind === 'intended-behavior');
  if (intendedBehavior.length !== 1) {
    throw new Error(
      `Source compiler lock requires exactly one intended-behavior authority; found ${intendedBehavior.length}`
    );
  }
  const assignedRoles = sources.flatMap(item => sourceFiles(item).map(file => file.role));
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
  const jmdict = sources.find((item): item is JmdictLockedSource => item.kind === 'jmdict')!;
  assertJmdictLifecycle(jmdict, archive, transition);
  const assignedPaths = sources.flatMap(item => sourceFiles(item).map(file => file.path));
  const duplicatePaths = [...new Set(assignedPaths.filter(
    (path, index) => assignedPaths.indexOf(path) !== index
  ))];
  if (duplicatePaths.length > 0) {
    throw new Error(`Source compiler lock assigns one file to multiple roles: ${duplicatePaths.join(', ')}`);
  }
  return {
    formatVersion: 1,
    baseline: {
      repository: webUrl(baseline.repository, 'Baseline repository'),
      startingCommit: commit(baseline.startingCommit, 'Baseline starting commit'),
      qualifiedArtifactTag: text(baseline.qualifiedArtifactTag, 'Qualified artifact tag'),
      upstreamIchiranCommit: commit(baseline.upstreamIchiranCommit, 'Upstream Ichiran commit'),
      upstreamDataReleaseTag: text(baseline.upstreamDataReleaseTag, 'Upstream data release tag')
    },
    ...(archive ? { archive } : {}),
    ...(transition ? { transition } : {}),
    sources
  };
}

export function sourceCompilerInputPaths(lock: SourceCompilerLock): readonly string[] {
  return lock.sources.flatMap(item => sourceFiles(item).map(file => file.path));
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

function verifyDictionaryProvenance(
  source: JmdictLockedSource | KanjidicLockedSource,
  compressed: Uint8Array
): void {
  let uncompressed: Uint8Array;
  try {
    uncompressed = new Uint8Array(gunzipSync(compressed));
  } catch {
    throw new Error(`Locked source ${source.file.path} is not valid gzip`);
  }
  const actualSha256 = sha256(uncompressed);
  if (uncompressed.byteLength !== source.uncompressedBytes
    || actualSha256 !== source.uncompressedSha256) {
    throw new Error(
      `Locked source ${source.file.path} expands to ${uncompressed.byteLength} bytes ${actualSha256}; `
      + `expected ${source.uncompressedBytes} bytes ${source.uncompressedSha256}`
    );
  }
  const contents = Buffer.from(
    uncompressed.buffer,
    uncompressed.byteOffset,
    uncompressed.byteLength
  );
  if (source.kind === 'jmdict') {
    if (!contents.includes(Buffer.from(`<!-- ${source.upstreamIdentity} -->`))) {
      throw new Error(`Locked source ${source.file.path} lacks ${source.upstreamIdentity}`);
    }
    return;
  }
  const match = /^file_version ([^;]+); database_version ([^;]+); date_of_creation (\d{4}-\d{2}-\d{2})$/.exec(
    source.upstreamIdentity
  )!;
  const tags = [
    `<file_version>${match[1]}</file_version>`,
    `<database_version>${match[2]}</database_version>`,
    `<date_of_creation>${match[3]}</date_of_creation>`
  ];
  if (tags.some(tag => !contents.includes(Buffer.from(tag)))) {
    throw new Error(`Locked source ${source.file.path} lacks ${source.upstreamIdentity}`);
  }
}

async function verifyLedgerRows(
  source: SemanticLedgerLockedSource | CompatibilityLedgerLockedSource,
  path: string
): Promise<void> {
  const rows = source.kind === 'semantic-ledger'
    ? (await loadQualifiedErrata(path)).rows.length
    : (await loadSourceCompatibility(path)).rows.length;
  if (rows !== source.rows) {
    throw new Error(`Locked source ${source.file.path} has ${rows} rows; expected ${source.rows}`);
  }
}

export async function verifySourceCompilerLock(
  repository: string,
  lockPath = 'data/source-compiler-sources.lock.json',
  snapshotDirectory?: string
): Promise<VerifiedSourceCompilerLock> {
  const bytes = new Uint8Array(await readFile(lockedPath(repository, lockPath)));
  const lock = parseSourceCompilerLock(JSON.parse(new TextDecoder().decode(bytes)));
  if (snapshotDirectory) await mkdir(snapshotDirectory);
  const files: VerifiedSourceCompilerLock['files'][number][] = [];
  const inputs = {} as Record<SourceCompilerInputRole, VerifiedSourceCompilerLock['inputs'][SourceCompilerInputRole]>;
  for (const item of lock.sources) {
    for (const expected of sourceFiles(item)) {
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
      const verifiedPath = snapshotDirectory
        ? join(snapshotDirectory, `${expected.role}-${basename(expected.path)}`)
        : path;
      if (snapshotDirectory) await writeFile(verifiedPath, actual, { flag: 'wx' });
      if (item.kind === 'jmdict' || item.kind === 'kanjidic2') {
        verifyDictionaryProvenance(item, actual);
      } else if (item.kind === 'semantic-ledger' || item.kind === 'compatibility-ledger') {
        await verifyLedgerRows(item, verifiedPath);
      }
      const file = {
        id: item.id,
        role: expected.role,
        path: expected.path,
        absolutePath: verifiedPath,
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
