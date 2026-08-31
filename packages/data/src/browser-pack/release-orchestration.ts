import { createHash } from 'node:crypto';
import { execFile as execFileCallback } from 'node:child_process';
import { readFile, stat } from 'node:fs/promises';
import { join, relative, resolve, sep } from 'node:path';
import { promisify } from 'node:util';

const execFile = promisify(execFileCallback);

export const BROWSER_ALPHA_SOURCES_LOCK = 'browser-alpha/sources.lock.json';
export const BROWSER_ALPHA_UPSTREAM_ORACLE = 'browser-alpha/upstream-oracle.json';
export const FROZEN_POSTGRES_REFERENCE_COMMIT = 'd583720572fbf26ee201166ac47034c50380a571';
export const BROWSER_ALPHA_SCHEMA_NORMALIZATION = 'pg-dump-16-schema-v1';

export interface BrowserAlphaSourceLock {
  readonly formatVersion: 2;
  readonly upstreamIchiran: {
    readonly repository: string;
    readonly commit: string;
    readonly dataReleaseTag: string;
  };
  readonly postgresReference: {
    readonly repositoryCommit: string;
  };
  readonly databaseDump: {
    readonly url: string;
    readonly bytes: number;
    readonly sha256: string;
  };
  readonly database: {
    readonly name: string;
    readonly postgresServerVersion: string;
    readonly encoding: string;
    readonly collation: string;
    readonly ctype: string;
    readonly schemaNormalization: typeof BROWSER_ALPHA_SCHEMA_NORMALIZATION;
    readonly schemaSha256: string;
  };
  readonly toolchain: {
    readonly bun: string;
    readonly node: string;
    readonly cargo: string;
    readonly rustc: string;
    readonly pgDump: string;
    readonly packFormat: number;
    readonly detailsFormat: number;
    readonly surfaceIndexFormat: number;
    readonly rootPayloadFormat: number;
    readonly morphologyFormat: number;
    readonly analyzerSupportFormat: number;
    readonly analyzerAnnotationsFormat: number;
  };
  readonly sources: readonly BrowserAlphaLockedSource[];
  readonly artifacts: BrowserAlphaArtifactCounts;
  readonly artifactDigests: BrowserAlphaArtifactDigests;
}

export interface BrowserAlphaLockedSource {
  readonly path: string;
  readonly bytes: number;
  readonly sha256: string;
}

export interface BrowserAlphaArtifactCounts {
  readonly surfaceIndex: {
    readonly input: number;
    readonly accepted: number;
    readonly direct: number;
    readonly morphology: number;
    readonly overlap: number;
    readonly omitted: number;
    readonly states: number;
    readonly edges: number;
  };
  readonly rootPayload: {
    readonly surfaces: number;
    readonly forms: number;
    readonly entries: number;
    readonly restrictions: number;
  };
  readonly morphology: {
    readonly positions: number;
    readonly rules: number;
    readonly templates: number;
    readonly suffixes: number;
    readonly rootKeys: number;
    readonly rootGroups: number;
    readonly patches: number;
    readonly tombstones: number;
  };
  readonly analyzerSupport: {
    readonly suffixKeys: number;
    readonly suffixValues: number;
    readonly suffixClasses: number;
    readonly counterKeys: number;
    readonly counterVariants: number;
    readonly collisions: number;
    readonly generatedRules: number;
    readonly generatedAliases: number;
  };
  readonly annotations: {
    readonly blocks: number;
    readonly splits: number;
    readonly hints: number;
    readonly generatedBlocks: number;
    readonly generatedRoots: number;
    readonly generatedRecords: number;
    readonly lookupOrderRecords: number;
    readonly lookupOrderRoots: number;
    readonly lookupOrderBytes: number;
    readonly lookupOrderExceptionSurfaces: number;
    readonly lookupOrderExceptionClasses: number;
    readonly lookupOrderExceptionLocators: number;
    readonly lookupOrderExceptionBytes: number;
    readonly generatedPhysicalGroups: number;
    readonly generatedFactPairs: number;
    readonly indexBytes: number;
    readonly uncompressedBytes: number;
    readonly compressedBytes: number;
    readonly annotationUncompressedBytes: number;
    readonly annotationCompressedBytes: number;
    readonly generatedUncompressedBytes: number;
    readonly generatedCompressedBytes: number;
    readonly totalBytes: number;
    readonly largestUncompressedBlock: number;
    readonly largestGeneratedBlock: number;
    readonly largestGeneratedCompressedBlock: number;
  };
  readonly details: {
    readonly entries: number;
    readonly forms: number;
    readonly senses: number;
    readonly glosses: number;
    readonly properties: number;
  };
}

export interface BrowserAlphaArtifactDigest {
  readonly bytes: number;
  readonly sha256: string;
}

export interface BrowserAlphaMorphologyAttestation {
  readonly rows: number;
  readonly sha256: string;
  readonly relationRows: number;
  readonly surfaceGroups: number;
  readonly exactSurfaceGroups: number;
  readonly legacyRelationKeys: number;
  readonly alphaRelationKeys: number;
  readonly legacyOnly: number;
  readonly alphaOnly: number;
  readonly duplicateLegacyRows: number;
  readonly duplicateAlphaCandidates: number;
  readonly databaseArtifacts: {
    readonly csrRows: number;
    readonly installedRouteCsrRows: number;
    readonly activeRouteCsrRows: number;
    readonly inactiveRouteCsrRows: number;
    readonly uninstalledCsrRows: number;
    readonly dualRouteCsrRows: number;
    readonly ghostSourceRows: number;
    readonly ghostRootSurfacePairs: number;
    readonly multiPropertyLinks: number;
    readonly staleRawKanaSurfaces: number;
  };
}

export interface BrowserAlphaArtifactDigests {
  readonly surfaceIndex: BrowserAlphaArtifactDigest;
  readonly rootPayload: BrowserAlphaArtifactDigest;
  readonly morphology: BrowserAlphaArtifactDigest;
  readonly analyzerSupport: BrowserAlphaArtifactDigest;
  readonly analyzerAnnotations: BrowserAlphaArtifactDigest;
  readonly details: BrowserAlphaArtifactDigest;
  readonly morphologyRelation: BrowserAlphaMorphologyAttestation;
}

export interface BrowserAlphaActualToolchain {
  readonly bun: string;
  readonly node: string;
  readonly cargo: string;
  readonly rustc: string;
  readonly pgDump: string;
  readonly packFormat: number;
  readonly detailsFormat: number;
  readonly surfaceIndexFormat: number;
  readonly rootPayloadFormat: number;
  readonly morphologyFormat: number;
  readonly analyzerSupportFormat: number;
  readonly analyzerAnnotationsFormat: number;
}

export interface BrowserAlphaSourceVerification {
  readonly lockBytes: Uint8Array;
  readonly lockSha256: string;
  readonly lock: BrowserAlphaSourceLock;
  readonly sources: readonly {
    readonly path: string;
    readonly bytes: number;
    readonly sha256: string;
  }[];
}

export function sha256Bytes(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function expectInteger(value: unknown, label: string): asserts value is number {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative integer`);
  }
}

function expectString(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${label} must be a string`);
}

function expectSha256(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${label} must be a lowercase SHA-256`);
  }
}

const MORPHOLOGY_RELATION_COUNT_FIELDS = [
  'rows',
  'relationRows',
  'surfaceGroups',
  'exactSurfaceGroups',
  'legacyRelationKeys',
  'alphaRelationKeys',
  'legacyOnly',
  'alphaOnly',
  'duplicateLegacyRows',
  'duplicateAlphaCandidates'
] as const;

const MORPHOLOGY_DATABASE_COUNT_FIELDS = [
  'csrRows',
  'installedRouteCsrRows',
  'activeRouteCsrRows',
  'inactiveRouteCsrRows',
  'uninstalledCsrRows',
  'dualRouteCsrRows',
  'ghostSourceRows',
  'ghostRootSurfacePairs',
  'multiPropertyLinks',
  'staleRawKanaSurfaces'
] as const;

function validateMorphologyAttestation(
  value: unknown,
  label: string
): asserts value is BrowserAlphaMorphologyAttestation {
  if (typeof value !== 'object' || value === null) throw new Error(`${label} must be an object`);
  const attestation = value as Partial<BrowserAlphaMorphologyAttestation>;
  for (const field of MORPHOLOGY_RELATION_COUNT_FIELDS) {
    expectInteger(attestation[field], `${label} ${field}`);
  }
  expectSha256(attestation.sha256, `${label} digest`);
  if (typeof attestation.databaseArtifacts !== 'object' || attestation.databaseArtifacts === null) {
    throw new Error(`${label} database artifacts must be an object`);
  }
  for (const field of MORPHOLOGY_DATABASE_COUNT_FIELDS) {
    expectInteger(attestation.databaseArtifacts[field], `${label} databaseArtifacts.${field}`);
  }
  const measured = attestation as BrowserAlphaMorphologyAttestation;
  if (measured.rows !== measured.legacyOnly + measured.alphaOnly) {
    throw new Error(`${label} row count does not equal the emitted relation differences`);
  }
  if (measured.relationRows !== measured.legacyRelationKeys + measured.duplicateLegacyRows) {
    throw new Error(`${label} legacy relation accounting is inconsistent`);
  }
  if (measured.legacyRelationKeys - measured.legacyOnly
    !== measured.alphaRelationKeys - measured.alphaOnly) {
    throw new Error(`${label} relation intersection accounting is inconsistent`);
  }
  if (measured.exactSurfaceGroups > measured.surfaceGroups) {
    throw new Error(`${label} exact surface groups exceed all surface groups`);
  }
  if (measured.alphaOnly !== 0) throw new Error(`${label} contains alpha-only candidates`);
  if (measured.duplicateLegacyRows !== 0) throw new Error(`${label} contains duplicate legacy rows`);
  if (measured.duplicateAlphaCandidates !== 0) {
    throw new Error(`${label} contains duplicate alpha candidates`);
  }
}

export function assertBrowserAlphaMorphologyAttestation(
  actual: BrowserAlphaMorphologyAttestation,
  expected: BrowserAlphaMorphologyAttestation
): void {
  validateMorphologyAttestation(actual, 'Measured morphology attestation');
  validateMorphologyAttestation(expected, 'Locked morphology attestation');
  for (const field of MORPHOLOGY_RELATION_COUNT_FIELDS) {
    if (actual[field] !== expected[field]) {
      throw new Error(
        `Morphology attestation ${field} ${actual[field]}; sources lock requires ${expected[field]}`
      );
    }
  }
  if (actual.sha256 !== expected.sha256) {
    throw new Error(
      `Morphology attestation digest ${actual.sha256}; sources lock requires ${expected.sha256}`
    );
  }
  for (const field of MORPHOLOGY_DATABASE_COUNT_FIELDS) {
    if (actual.databaseArtifacts[field] !== expected.databaseArtifacts[field]) {
      throw new Error(
        `Morphology attestation databaseArtifacts.${field} ${actual.databaseArtifacts[field]}; `
        + `sources lock requires ${expected.databaseArtifacts[field]}`
      );
    }
  }
}

const ARTIFACT_COUNT_FIELDS = {
  surfaceIndex: ['input', 'accepted', 'direct', 'morphology', 'overlap', 'omitted', 'states', 'edges'],
  rootPayload: ['surfaces', 'forms', 'entries', 'restrictions'],
  morphology: ['positions', 'rules', 'templates', 'suffixes', 'rootKeys', 'rootGroups', 'patches', 'tombstones'],
  analyzerSupport: [
    'suffixKeys', 'suffixValues', 'suffixClasses', 'counterKeys', 'counterVariants',
    'collisions', 'generatedRules', 'generatedAliases'
  ],
  annotations: [
    'blocks', 'splits', 'hints', 'generatedBlocks', 'generatedRoots', 'generatedRecords',
    'lookupOrderRecords', 'lookupOrderRoots', 'lookupOrderBytes',
    'lookupOrderExceptionSurfaces', 'lookupOrderExceptionClasses',
    'lookupOrderExceptionLocators', 'lookupOrderExceptionBytes', 'generatedPhysicalGroups',
    'generatedFactPairs', 'indexBytes', 'uncompressedBytes', 'compressedBytes',
    'annotationUncompressedBytes', 'annotationCompressedBytes', 'generatedUncompressedBytes',
    'generatedCompressedBytes', 'totalBytes', 'largestUncompressedBlock',
    'largestGeneratedBlock', 'largestGeneratedCompressedBlock'
  ],
  details: ['entries', 'forms', 'senses', 'glosses', 'properties']
} as const;

function expectCommit(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !/^[0-9a-f]{40}$/.test(value)) {
    throw new Error(`${label} must be a full lowercase Git object ID`);
  }
}

/** Strict enough to reject a malformed lock before it can authorize a build. */
export function parseBrowserAlphaSourceLock(text: string): BrowserAlphaSourceLock {
  const parsed: unknown = JSON.parse(text);
  if (typeof parsed !== 'object' || parsed === null) throw new Error('Sources lock must be an object');
  const lock = parsed as Partial<BrowserAlphaSourceLock>;
  if (lock.formatVersion !== 2) throw new Error('Unsupported sources lock format');
  if (
    !lock.upstreamIchiran || !lock.postgresReference || !lock.databaseDump
    || !lock.database || !lock.toolchain || !Array.isArray(lock.sources)
    || !lock.artifacts || !lock.artifactDigests
  ) {
    throw new Error('Sources lock is missing provenance, database, toolchain, sources, or artifacts');
  }
  expectString(lock.upstreamIchiran.repository, 'Upstream Ichiran repository');
  expectCommit(lock.upstreamIchiran.commit, 'Upstream Ichiran commit');
  expectString(lock.upstreamIchiran.dataReleaseTag, 'Upstream Ichiran data release tag');
  expectCommit(lock.postgresReference.repositoryCommit, 'PostgreSQL reference commit');
  expectString(lock.databaseDump.url, 'Database dump URL');
  let dumpUrl: URL;
  try {
    dumpUrl = new URL(lock.databaseDump.url);
  } catch {
    throw new Error('Database dump URL must be an absolute URL');
  }
  if (dumpUrl.protocol !== 'https:') throw new Error('Database dump URL must use HTTPS');
  expectInteger(lock.databaseDump.bytes, 'Database dump bytes');
  expectSha256(lock.databaseDump.sha256, 'Database dump digest');
  expectString(lock.database.name, 'Database name');
  expectString(lock.database.postgresServerVersion, 'PostgreSQL server version');
  expectString(lock.database.encoding, 'Database encoding');
  expectString(lock.database.collation, 'Database collation');
  expectString(lock.database.ctype, 'Database character classification');
  if (lock.database.schemaNormalization !== BROWSER_ALPHA_SCHEMA_NORMALIZATION) {
    throw new Error(`Database schema normalization must be ${BROWSER_ALPHA_SCHEMA_NORMALIZATION}`);
  }
  expectSha256(lock.database.schemaSha256, 'Database schema digest');
  expectString(lock.toolchain.bun, 'Bun version');
  expectString(lock.toolchain.node, 'Node version');
  expectString(lock.toolchain.cargo, 'Cargo version');
  expectString(lock.toolchain.rustc, 'Rust version');
  expectString(lock.toolchain.pgDump, 'pg_dump version');
  for (const field of [
    'packFormat', 'detailsFormat', 'surfaceIndexFormat', 'rootPayloadFormat',
    'morphologyFormat', 'analyzerSupportFormat'
  ] as const) expectInteger(lock.toolchain[field], `Toolchain ${field}`);
  expectInteger(lock.toolchain.analyzerAnnotationsFormat, 'Toolchain analyzerAnnotationsFormat');

  const paths = new Set<string>();
  for (const [index, source] of lock.sources.entries()) {
    expectString(source.path, `Source ${index} path`);
    if (source.path.startsWith('/') || source.path.includes('\\') || source.path.split('/').includes('..')) {
      throw new Error(`Source ${index} path must be a portable repository-relative path`);
    }
    if (paths.has(source.path)) throw new Error(`Duplicate locked source ${source.path}`);
    paths.add(source.path);
    expectInteger(source.bytes, `Source ${source.path} bytes`);
    expectSha256(source.sha256, `Source ${source.path} digest`);
  }

  const artifactCounts = lock.artifacts as unknown as Record<string, Record<string, unknown>>;
  for (const [artifactName, fields] of Object.entries(ARTIFACT_COUNT_FIELDS)) {
    const counts = artifactCounts[artifactName];
    if (!counts || typeof counts !== 'object') {
      throw new Error(`Sources lock is missing ${artifactName} artifact counts`);
    }
    for (const field of fields) expectInteger(counts[field], `${artifactName}.${field}`);
  }
  for (const name of [
    'surfaceIndex', 'rootPayload', 'morphology', 'analyzerSupport',
    'analyzerAnnotations', 'details'
  ] as const) {
    const artifact = lock.artifactDigests[name];
    if (!artifact) throw new Error(`Sources lock is missing ${name} artifact digest`);
    expectInteger(artifact.bytes, `${name} artifact bytes`);
    expectSha256(artifact.sha256, `${name} artifact digest`);
  }
  validateMorphologyAttestation(
    lock.artifactDigests.morphologyRelation,
    'Morphology relation attestation'
  );
  return lock as BrowserAlphaSourceLock;
}

function lockedPath(repositoryRoot: string, sourcePath: string): string {
  const root = resolve(repositoryRoot);
  const candidate = resolve(root, ...sourcePath.split('/'));
  const within = relative(root, candidate);
  if (within === '..' || within.startsWith(`..${sep}`) || resolve(candidate) === root) {
    throw new Error(`Locked source escapes the repository: ${sourcePath}`);
  }
  return candidate;
}

export async function verifyBrowserAlphaSources(
  repositoryRoot: string,
  lockPath = BROWSER_ALPHA_SOURCES_LOCK
): Promise<BrowserAlphaSourceVerification> {
  const normalizedLockPath = lockedPath(repositoryRoot, lockPath);
  const lockBytes = new Uint8Array(await readFile(normalizedLockPath));
  const lock = parseBrowserAlphaSourceLock(new TextDecoder().decode(lockBytes));
  const sources = [];
  for (const expected of lock.sources) {
    const path = lockedPath(repositoryRoot, expected.path);
    const info = await stat(path);
    if (!info.isFile()) throw new Error(`Locked source is not a file: ${expected.path}`);
    const bytes = new Uint8Array(await readFile(path));
    const actual = { path: expected.path, bytes: bytes.byteLength, sha256: sha256Bytes(bytes) };
    if (actual.bytes !== expected.bytes) {
      throw new Error(`${expected.path} is ${actual.bytes} bytes; lock requires ${expected.bytes}`);
    }
    if (actual.sha256 !== expected.sha256) {
      throw new Error(`${expected.path} digest ${actual.sha256}; lock requires ${expected.sha256}`);
    }
    sources.push(actual);
  }
  return {
    lockBytes,
    lockSha256: sha256Bytes(lockBytes),
    lock,
    sources
  };
}

/**
 * Prove that the frozen PostgreSQL implementation is byte-for-byte identical
 * to packages/core/src at the named checkpoint. Package metadata and tests may
 * change as the reference is made private; analyzer source may not.
 */
export async function verifyBrowserAlphaOracleCore(
  repositoryRoot: string,
  oracleRepositoryCommit: string
): Promise<void> {
  if (!/^[0-9a-f]{40}$/.test(oracleRepositoryCommit)) {
    throw new Error('Oracle repository commit must be a full lowercase Git object ID');
  }

  const git = async (arguments_: readonly string[]): Promise<string> => {
    const { stdout } = await execFile(
      'git',
      ['-C', resolve(repositoryRoot), ...arguments_],
      { encoding: 'utf8' }
    );
    return stdout;
  };

  try {
    await git(['cat-file', '-e', `${oracleRepositoryCommit}^{commit}`]);
  } catch {
    throw new Error(`Oracle repository commit does not exist: ${oracleRepositoryCommit}`);
  }
  try {
    await git(['merge-base', '--is-ancestor', oracleRepositoryCommit, 'HEAD']);
  } catch {
    throw new Error(`Oracle repository commit is not an ancestor of HEAD: ${oracleRepositoryCommit}`);
  }

  const oldPrefix = 'packages/core/src/';
  const referencePrefix = 'packages/reference-postgres/src/';
  const expected = (await git([
    'ls-tree', '-r', '--name-only', oracleRepositoryCommit, '--', 'packages/core/src'
  ])).trim().split('\n').filter(Boolean);
  const current = (await git([
    'ls-files', '--', 'packages/reference-postgres/src'
  ])).trim().split('\n').filter(Boolean);
  const mapped = expected.map(path => `${referencePrefix}${path.slice(oldPrefix.length)}`);
  if (mapped.join('\n') !== current.join('\n')) {
    throw new Error(`Legacy oracle core file inventory differs from ${oracleRepositoryCommit}`);
  }
  for (let index = 0; index < expected.length; index++) {
    const expectedBlob = (await git([
      'rev-parse', `${oracleRepositoryCommit}:${expected[index]}`
    ])).trim();
    const actualBlob = (await git(['hash-object', mapped[index]!])).trim();
    if (expectedBlob !== actualBlob) {
      throw new Error(`Legacy oracle core differs from ${oracleRepositoryCommit}: ${mapped[index]}`);
    }
  }

  const untracked = (await git([
    'ls-files', '--others', '--exclude-standard', '--', 'packages/reference-postgres/src'
  ])).trim();
  if (untracked.length !== 0) {
    throw new Error(`Legacy oracle core contains untracked files: ${untracked}`);
  }
}

export function verifyBrowserAlphaToolchain(
  expected: BrowserAlphaSourceLock['toolchain'],
  actual: BrowserAlphaActualToolchain
): void {
  const values: readonly [string, string | number, string | number][] = [
    ['Bun', expected.bun, actual.bun],
    ['Node', expected.node, actual.node],
    ['Cargo', expected.cargo, actual.cargo],
    ['Rust', expected.rustc, actual.rustc],
    ['pg_dump', expected.pgDump, actual.pgDump],
    ['pack format', expected.packFormat, actual.packFormat],
    ['details format', expected.detailsFormat, actual.detailsFormat],
    ['surface-index format', expected.surfaceIndexFormat, actual.surfaceIndexFormat],
    ['root-payload format', expected.rootPayloadFormat, actual.rootPayloadFormat],
    ['morphology format', expected.morphologyFormat, actual.morphologyFormat],
    ['analyzer-support format', expected.analyzerSupportFormat, actual.analyzerSupportFormat],
    ['analyzer-annotations format', expected.analyzerAnnotationsFormat, actual.analyzerAnnotationsFormat]
  ];
  for (const [label, wanted, found] of values) {
    if (wanted !== found) throw new Error(`${label} ${found}; sources lock requires ${wanted}`);
  }
}

export function assertExactCount(actual: number, expected: number, label: string): void {
  expectInteger(actual, `${label} count`);
  if (actual !== expected) throw new Error(`${label} count ${actual}; sources lock requires ${expected}`);
}

/** Canonical JSON used by reports and fixture tests. */
export function deterministicJson(value: unknown): Uint8Array {
  return new TextEncoder().encode(`${JSON.stringify(value, null, 2)}\n`);
}

export function assertBytesEqual(left: Uint8Array, right: Uint8Array, label: string): void {
  if (left.byteLength !== right.byteLength) {
    throw new Error(`${label} rebuild changed length (${left.byteLength} != ${right.byteLength})`);
  }
  for (let index = 0; index < left.byteLength; index++) {
    if (left[index] !== right[index]) throw new Error(`${label} rebuild differs at byte ${index}`);
  }
}

export function repositoryPath(repositoryRoot: string, ...parts: string[]): string {
  return join(repositoryRoot, ...parts);
}
