#!/usr/bin/env bun

import { execFile as execFileCallback, spawn } from 'node:child_process';
import { closeSync, openSync, writeSync } from 'node:fs';
import {
  mkdir,
  mkdtemp,
  readFile,
  rename,
  rm,
  stat,
  writeFile
} from 'node:fs/promises';
import { dirname, join, relative, resolve, sep } from 'node:path';
import { tmpdir } from 'node:os';
import { promisify } from 'node:util';
import { gunzipSync } from 'node:zlib';
import postgres, { type Sql } from 'postgres';

import { buildAnalyzerAnnotations } from '../packages/data/src/browser-pack/analyzer-annotations.js';
import {
  buildAnalyzerSupportCore,
  loadAnalyzerSupportSource
} from '../packages/data/src/browser-pack/analyzer-support.js';
import { buildDetailStore, loadDetailEntries } from '../packages/data/src/browser-pack/details.js';
import { compileMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import { encodeMorphologyArtifact } from '../packages/data/src/browser-pack/morphology-format.js';
import {
  morphologyRelationAttestation,
  verifyMorphologyRelation
} from '../packages/data/src/browser-pack/morphology-verifier.js';
import {
  ANALYZER_RELEASE_FORMAT_VERSION,
  analyzerManifestDigestInput,
  assertAnalyzerReleaseSize,
  buildAnalyzerRelease,
  type AnalyzerReleaseBuild,
  type AnalyzerReleaseManifest
} from '../packages/data/src/browser-pack/release-manifest.js';
import {
  assertBrowserAlphaMorphologyAttestation,
  assertBytesEqual,
  assertExactCount,
  BROWSER_ALPHA_SOURCES_LOCK,
  BROWSER_ALPHA_UPSTREAM_ORACLE,
  deterministicJson,
  FROZEN_POSTGRES_REFERENCE_COMMIT,
  parseBrowserAlphaSourceLock,
  sha256Bytes,
  verifyBrowserAlphaOracleCore,
  verifyBrowserAlphaSources,
  verifyBrowserAlphaToolchain,
  type BrowserAlphaArtifactCounts,
  type BrowserAlphaArtifactDigests,
  type BrowserAlphaMorphologyAttestation,
  type BrowserAlphaSourceLock
} from '../packages/data/src/browser-pack/release-orchestration.js';
import { buildRootPayload, loadRootPayloadSource } from '../packages/data/src/browser-pack/root-payload.js';
import { SURFACE_INDEX_COPY_QUERY } from '../packages/data/src/browser-pack/surface-index.js';
import {
  ANALYZER_ANNOTATIONS_FORMAT_VERSION,
  ANALYZER_ANNOTATIONS_SECTION_ID,
  ANALYZER_GENERATED_CACHE_BLOCKS,
  ANALYZER_LOOKUP_ORDER_RECORD_BYTES,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../packages/core/src/analyzer-annotations.js';
import {
  ANALYZER_SUPPORT_FORMAT_VERSION,
  ANALYZER_SUPPORT_SECTION_ID,
  openAnalyzerSupport
} from '../packages/core/src/analyzer-support.js';
import {
  DETAILS_FORMAT_VERSION,
  memoryDetailSource,
  openDetailStore
} from '../packages/core/src/details.js';
import { PACK_FORMAT_VERSION } from '../packages/core/src/format.js';
import { MORPHOLOGY_SECTION_ID, openMorphology } from '../packages/core/src/morphology.js';
import { encodePack, openPack } from '../packages/core/src/pack.js';
import {
  ROOT_PAYLOAD_FORMAT_VERSION,
  ROOT_PAYLOAD_SECTION_ID,
  openRootPayload
} from '../packages/core/src/root-payload.js';
import {
  SURFACE_INDEX_FORMAT_VERSION,
  SURFACE_INDEX_SECTION_ID,
  openSurfaceIndex
} from '../packages/core/src/surface-index.js';

const execFile = promisify(execFileCallback);
const MORPHOLOGY_FORMAT_VERSION = 1;
const RELEASE_STATS_FORMAT_VERSION = 1;
const RELEASE_FILENAMES = ['hot.bin.gz', 'details.bin.gz', 'manifest.json', 'stats.json'] as const;
const UPSTREAM_ICHIRAN_COMMIT = 'ea9583368e67cad22d94abae8dbcc8df96d99bcd';
const UPSTREAM_DATA_RELEASE_TAG = 'ichiran-260118';
const RELEASE_SOURCE_PATHS = [
  'data/conj.csv',
  'data/conjo.csv',
  'data/kwpos.csv',
  'data/sources/extra.xml',
  'data/sources/gyoseiku.csv',
  'data/sources/jichitai.csv',
  'packages/data/JMdict_e.gz'
] as const;

interface UpstreamOracle {
  readonly scope: string;
  readonly grammarIncluded: boolean;
  readonly ichiran: {
    readonly repository: string;
    readonly commit: string;
    readonly dataReleaseTag: string;
  };
  readonly databaseDump: {
    readonly url: string;
    readonly bytes: number;
    readonly sha256: string;
  };
  readonly qualifiedOracle: {
    readonly normalizedPgDump16SchemaSha256: string;
  };
}

interface CliOptions {
  readonly command: 'build' | 'verify' | 'refresh-lock';
  readonly database?: string;
  readonly out?: string;
  readonly packVersion?: string;
  readonly shellBytes?: number;
  readonly allowDirty: boolean;
}

interface SurfaceCompilerStats {
  readonly input: number;
  readonly accepted: number;
  readonly direct: number;
  readonly morphology: number;
  readonly overlap: number;
  readonly omitted: number;
  readonly states: number;
  readonly edges: number;
  readonly bytes: number;
}

interface DatabaseIdentity {
  readonly name: string;
  readonly postgresServerVersion: string;
  readonly encoding: string;
  readonly collation: string;
  readonly ctype: string;
  readonly readOnly: boolean;
}

interface ComponentBuilds {
  readonly surface: { readonly bytes: Uint8Array; readonly stats: SurfaceCompilerStats };
  readonly root: ReturnType<typeof buildRootPayload>;
  readonly morphology: Pick<Awaited<ReturnType<typeof compileMorphology>>, 'bytes' | 'stats'>;
  readonly morphologyRelation: BrowserAlphaMorphologyAttestation;
  readonly support: ReturnType<typeof buildAnalyzerSupportCore>;
  readonly annotations: ReturnType<typeof buildAnalyzerAnnotations>;
  readonly details: ReturnType<typeof buildDetailStore>;
  readonly supportIssueCount: number;
  readonly supportIssuesSha256: string;
  readonly database: DatabaseIdentity;
}

interface VerifiedAnalyzerRelease extends AnalyzerReleaseBuild {
  readonly inspection: {
    readonly artifacts: BrowserAlphaArtifactCounts;
    readonly sections: readonly {
      readonly id: number;
      readonly bytes: number;
      readonly sha256: string;
    }[];
    readonly details: { readonly bytes: number; readonly sha256: string };
  };
}

function usage(message?: string): never {
  if (message) console.error(`error: ${message}\n`);
  console.error(`usage:
  bun run alpha:release:build -- --database <url> --out <directory> \\
    --pack-version <version> --shell-bytes <integer> [--allow-dirty]
  bun run alpha:release:verify -- --out <directory> --shell-bytes <integer> [--allow-dirty]
  bun run alpha:release:refresh-lock -- --database <url> [--allow-dirty]`);
  process.exit(2);
}

function parseNonNegativeInteger(text: string, label: string): number {
  if (!/^(0|[1-9][0-9]*)$/.test(text)) usage(`${label} must be a non-negative integer`);
  const value = Number(text);
  if (!Number.isSafeInteger(value)) usage(`${label} is too large`);
  return value;
}

function parseArgs(argv: readonly string[]): CliOptions {
  const command = argv[0];
  if (command !== 'build' && command !== 'verify' && command !== 'refresh-lock') {
    usage('first argument must be build, verify, or refresh-lock');
  }
  let database: string | undefined;
  let out: string | undefined;
  let packVersion: string | undefined;
  let shellBytes: number | undefined;
  let allowDirty = false;
  for (let index = 1; index < argv.length; index++) {
    const argument = argv[index]!;
    const next = (): string => {
      const value = argv[++index];
      if (!value) usage(`${argument} requires a value`);
      return value;
    };
    if (argument === '--database') database = next();
    else if (argument === '--out') out = next();
    else if (argument === '--pack-version') packVersion = next();
    else if (argument === '--shell-bytes') shellBytes = parseNonNegativeInteger(next(), '--shell-bytes');
    else if (argument === '--allow-dirty') allowDirty = true;
    else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (command !== 'refresh-lock' && !out) usage('--out is required');
  if (command !== 'refresh-lock' && shellBytes === undefined) usage('--shell-bytes is required');
  if ((command === 'build' || command === 'refresh-lock') && !database) {
    usage(`--database is required for ${command}`);
  }
  if (command === 'build' && !packVersion) usage('--pack-version is required for build');
  if (command === 'verify' && (database || packVersion)) {
    usage('--database and --pack-version apply only to build');
  }
  if (command === 'refresh-lock' && (out || packVersion || shellBytes !== undefined)) {
    usage('--out, --pack-version, and --shell-bytes do not apply to refresh-lock');
  }
  return { command, database, out, packVersion, shellBytes, allowDirty };
}

async function repositoryRoot(): Promise<string> {
  const { stdout } = await execFile('git', ['rev-parse', '--show-toplevel'], { encoding: 'utf8' });
  return stdout.trim();
}

async function sourceCommit(root: string): Promise<string> {
  const { stdout } = await execFile('git', ['-C', root, 'rev-parse', 'HEAD'], { encoding: 'utf8' });
  const commit = stdout.trim();
  if (!/^[0-9a-f]{40}$/.test(commit)) throw new Error(`Git returned invalid source commit ${commit}`);
  return commit;
}

function fullCommit(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !/^[0-9a-f]{40}$/.test(value)) {
    throw new Error(`${label} must be a full lowercase Git object ID`);
  }
}

function sha256(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${label} must be a lowercase SHA-256`);
  }
}

async function upstreamOracle(root: string): Promise<UpstreamOracle> {
  const bytes = await readFile(join(root, BROWSER_ALPHA_UPSTREAM_ORACLE));
  const parsed: unknown = JSON.parse(bytes.toString('utf8'));
  if (typeof parsed !== 'object' || parsed === null) throw new Error('Upstream oracle must be an object');
  const oracle = parsed as Partial<UpstreamOracle>;
  if (oracle.scope !== 'analyzer-only' || oracle.grammarIncluded !== false) {
    throw new Error('Upstream oracle must be analyzer-only and exclude grammar');
  }
  if (!oracle.ichiran || !oracle.databaseDump || !oracle.qualifiedOracle) {
    throw new Error('Upstream oracle is missing Ichiran, dump, or qualified database provenance');
  }
  if (oracle.ichiran.commit !== UPSTREAM_ICHIRAN_COMMIT) {
    throw new Error(`Upstream oracle commit must be ${UPSTREAM_ICHIRAN_COMMIT}`);
  }
  if (oracle.ichiran.dataReleaseTag !== UPSTREAM_DATA_RELEASE_TAG) {
    throw new Error(`Upstream oracle data release must be ${UPSTREAM_DATA_RELEASE_TAG}`);
  }
  if (typeof oracle.ichiran.repository !== 'string' || oracle.ichiran.repository.length === 0) {
    throw new Error('Upstream oracle repository must be a string');
  }
  fullCommit(oracle.ichiran.commit, 'Upstream Ichiran commit');
  if (typeof oracle.databaseDump.url !== 'string'
    || !Number.isSafeInteger(oracle.databaseDump.bytes)
    || oracle.databaseDump.bytes <= 0) {
    throw new Error('Upstream database dump URL and byte length are invalid');
  }
  sha256(oracle.databaseDump.sha256, 'Upstream database dump digest');
  sha256(
    oracle.qualifiedOracle.normalizedPgDump16SchemaSha256,
    'Qualified database schema digest'
  );
  return oracle as UpstreamOracle;
}

async function measureReleaseSources(root: string) {
  return await Promise.all(RELEASE_SOURCE_PATHS.map(async (path) => {
    const bytes = new Uint8Array(await readFile(join(root, path)));
    return { path, bytes: bytes.byteLength, sha256: sha256Bytes(bytes) };
  }));
}

async function assertCleanSource(root: string, allowDirty: boolean): Promise<void> {
  if (allowDirty) return;
  const { stdout } = await execFile(
    'git', ['-C', root, 'status', '--porcelain=v1', '--untracked-files=all'], { encoding: 'utf8' }
  );
  if (stdout.length !== 0) {
    const count = stdout.trimEnd().split('\n').length;
    throw new Error(
      `Source checkout has ${count} tracked or untracked change(s); commit them or use --allow-dirty for development`
    );
  }
}

function releaseOutputPath(root: string, value: string): string {
  const output = resolve(root, value);
  const within = relative(root, output);
  if (within === '' || within === '..' || within.startsWith(`..${sep}`)) {
    throw new Error('Release output must be a directory below the repository root');
  }
  return output;
}

async function actualToolchain() {
  const [nodeResult, cargoResult, rustcResult, pgDumpResult] = await Promise.all([
    execFile('node', ['--version'], { encoding: 'utf8' }),
    execFile('cargo', ['--version'], { encoding: 'utf8' }),
    execFile('rustc', ['--version'], { encoding: 'utf8' }),
    execFile('pg_dump', ['--version'], { encoding: 'utf8' })
  ]);
  const node = nodeResult.stdout.trim().replace(/^v/, '');
  if (!/^\d+\.\d+\.\d+$/.test(node)) throw new Error(`Node returned invalid version ${node}`);
  return {
    bun: Bun.version,
    node,
    cargo: cargoResult.stdout.trim(),
    rustc: rustcResult.stdout.trim(),
    pgDump: pgDumpResult.stdout.trim(),
    packFormat: PACK_FORMAT_VERSION,
    detailsFormat: DETAILS_FORMAT_VERSION,
    surfaceIndexFormat: SURFACE_INDEX_FORMAT_VERSION,
    rootPayloadFormat: ROOT_PAYLOAD_FORMAT_VERSION,
    morphologyFormat: MORPHOLOGY_FORMAT_VERSION,
    analyzerSupportFormat: ANALYZER_SUPPORT_FORMAT_VERSION,
    analyzerAnnotationsFormat: ANALYZER_ANNOTATIONS_FORMAT_VERSION
  } as const;
}

function parseSurfaceStats(stderr: string): SurfaceCompilerStats {
  const line = stderr.trim().split('\n').find((value) => value.startsWith('surfaces='));
  if (!line) throw new Error(`Surface compiler did not emit its deterministic stats line: ${stderr.trim()}`);
  const values = new Map<string, number>();
  for (const field of line.split(' ')) {
    const match = /^([a-z_]+)=([0-9]+)$/.exec(field);
    if (match && match[1] !== 'elapsed_ms') values.set(match[1], Number(match[2]));
  }
  const take = (name: string): number => {
    const value = values.get(name);
    if (value === undefined || !Number.isSafeInteger(value)) {
      throw new Error(`Surface compiler omitted ${name}`);
    }
    return value;
  };
  return {
    input: take('surfaces'),
    accepted: take('accepted'),
    direct: take('direct'),
    morphology: take('morphology'),
    overlap: take('overlap'),
    omitted: take('omitted'),
    states: take('states'),
    edges: take('edges'),
    bytes: take('bytes')
  };
}

async function command(
  executable: string,
  args: readonly string[],
  options: { readonly cwd: string; readonly stdout?: 'ignore' | 'pipe' }
): Promise<{ readonly stdout: string; readonly stderr: string }> {
  return await new Promise((resolvePromise, reject) => {
    const child = spawn(executable, args, {
      cwd: options.cwd,
      stdio: ['ignore', options.stdout ?? 'pipe', 'pipe']
    });
    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    child.stdout?.on('data', (chunk: Buffer) => stdout.push(chunk));
    child.stderr?.on('data', (chunk: Buffer) => stderr.push(chunk));
    child.once('error', reject);
    child.once('close', (code, signal) => {
      const output = Buffer.concat(stdout).toString('utf8');
      const errors = Buffer.concat(stderr).toString('utf8');
      if (code === 0) resolvePromise({ stdout: output, stderr: errors });
      else reject(new Error(`${executable} failed (${signal ?? code}): ${errors || output}`));
    });
  });
}

async function buildSurfaceCompiler(root: string): Promise<string> {
  const manifest = join(root, 'packages/data/tools/surface-index/Cargo.toml');
  await command('cargo', ['build', '--locked', '--release', '--manifest-path', manifest], {
    cwd: root,
    stdout: 'ignore'
  });
  return join(root, 'packages/data/tools/surface-index/target/release/ichiran-surface-index');
}

async function runSurfaceCompiler(
  executable: string,
  input: string,
  output: string,
  root: string
): Promise<SurfaceCompilerStats> {
  const result = await command(executable, ['--input', input, '--output', output], {
    cwd: root,
    stdout: 'ignore'
  });
  return parseSurfaceStats(result.stderr);
}

async function databaseIdentity(sql: Sql): Promise<DatabaseIdentity> {
  const rows = await sql.unsafe<Array<{
    name: string;
    postgresServerVersion: string;
    encoding: string;
    collation: string;
    ctype: string;
    readOnly: boolean;
  }>>(`
    SELECT current_database() AS name,
           current_setting('server_version') AS "postgresServerVersion",
           pg_encoding_to_char(d.encoding) AS encoding,
           d.datcollate AS collation,
           d.datctype AS ctype,
           current_setting('transaction_read_only') = 'on' AS "readOnly"
    FROM pg_database d
    WHERE d.datname = current_database()
  `);
  const identity = rows[0];
  if (!identity || rows.length !== 1) throw new Error('Could not read database identity');
  if (!identity.readOnly) throw new Error('Release compiler database transaction is not read-only');
  return identity;
}

function assertDatabaseIdentity(actual: DatabaseIdentity, lock: BrowserAlphaSourceLock): void {
  const expected = lock.database;
  for (const [label, wanted, found] of [
    ['name', expected.name, actual.name],
    ['server version', expected.postgresServerVersion, actual.postgresServerVersion],
    ['encoding', expected.encoding, actual.encoding],
    ['collation', expected.collation, actual.collation],
    ['character classification', expected.ctype, actual.ctype]
  ] as const) {
    if (wanted !== found) throw new Error(`Database ${label} ${found}; sources lock requires ${wanted}`);
  }
}

async function exportSurfaceTsv(sql: Sql, destination: string): Promise<void> {
  const readable = await sql.unsafe(SURFACE_INDEX_COPY_QUERY).readable();
  const descriptor = openSync(destination, 'wx');
  try {
    // Bun's Node-stream pipeline can leave postgres.js' underlying socket
    // paused when a large COPY ends under writable backpressure. Consuming in
    // flowing mode with synchronous build-only writes keeps the readable
    // drained and makes the following query on this transaction reliable.
    await new Promise<void>((resolvePromise, reject) => {
      readable.on('data', (chunk: Buffer) => {
        try {
          writeSync(descriptor, chunk);
        } catch (error) {
          readable.destroy(error instanceof Error ? error : new Error(String(error)));
        }
      });
      readable.once('end', resolvePromise);
      readable.once('error', reject);
    });
  } finally {
    closeSync(descriptor);
  }
}

function issueDigest(issues: readonly unknown[]): string {
  const canonical = [...issues].map((value) => JSON.stringify(value)).sort();
  return sha256Bytes(new TextEncoder().encode(canonical.join('\n')));
}

async function deterministicRoot(sql: Sql): Promise<ReturnType<typeof buildRootPayload>> {
  const source = await loadRootPayloadSource(sql);
  const build = buildRootPayload(source);
  assertBytesEqual(build.bytes, buildRootPayload(source).bytes, 'Root payload');
  return build;
}

async function deterministicDetails(sql: Sql): Promise<ReturnType<typeof buildDetailStore>> {
  const source = await loadDetailEntries(sql);
  const build = buildDetailStore(source);
  assertBytesEqual(build.bytes, buildDetailStore(source).bytes, 'Detail store');
  return build;
}

async function deterministicMorphology(
  sql: Sql,
  dataPath: string
): Promise<Pick<Awaited<ReturnType<typeof compileMorphology>>, 'bytes' | 'stats'>> {
  const build = await compileMorphology({ sql, dataPath });
  assertBytesEqual(build.bytes, encodeMorphologyArtifact(build.artifact), 'Morphology section');
  return { bytes: build.bytes, stats: build.stats };
}

async function deterministicSupport(sql: Sql): Promise<{
  readonly support: ReturnType<typeof buildAnalyzerSupportCore>;
  readonly annotations: ReturnType<typeof buildAnalyzerAnnotations>;
  readonly supportIssueCount: number;
  readonly supportIssuesSha256: string;
}> {
  // The pinned generated CTE is badly underestimated by PostgreSQL 16, which
  // otherwise chooses a quadratic nested loop for the physical-member join.
  // Keep the planner override inside support projection only; root/details
  // retain their normal plans.
  await sql.unsafe('SET LOCAL enable_nestloop = off');
  let source: Awaited<ReturnType<typeof loadAnalyzerSupportSource>>;
  try {
    source = await loadAnalyzerSupportSource(sql);
  } finally {
    await sql.unsafe('SET LOCAL enable_nestloop TO DEFAULT');
  }
  const issues = source.issues ?? [];
  if (issues.length !== 0) {
    throw new Error(`Analyzer-support compiler reported ${issues.length} unresolved issue(s)`);
  }
  const support = buildAnalyzerSupportCore(source);
  assertBytesEqual(support.bytes, buildAnalyzerSupportCore(source).bytes, 'Analyzer-support section');
  if (!source.generated) throw new Error('Analyzer support source omitted generated-entry facts');
  const annotations = buildAnalyzerAnnotations(source.splits, source.hints, source.generated);
  assertBytesEqual(
    annotations.bytes,
    buildAnalyzerAnnotations(source.splits, source.hints, source.generated).bytes,
    'Analyzer-annotations section'
  );
  return {
    support,
    annotations,
    supportIssueCount: issues.length,
    supportIssuesSha256: issueDigest(issues)
  };
}

async function loadComponents(
  root: string,
  database: string,
  temporary: string,
  lock?: BrowserAlphaSourceLock
): Promise<Omit<ComponentBuilds, 'surface'>> {
  const connectionOptions = {
    max: 1,
    prepare: false,
    // Legacy analyzer-only freezers run through withConnectionOverride() and
    // therefore require the same snake_case-to-camelCase row contract as the
    // core connection they replace for this transaction.
    transform: postgres.camel,
    connection: {
      application_name: 'ichiran-browser-alpha-release',
      default_transaction_read_only: true
    }
  } as const;
  let sql: Sql;
  if (/^postgres(?:ql)?:\/\//.test(database)) {
    const url = new URL(database.replace(/^postgresql:/, 'postgres:'));
    const databaseName = decodeURIComponent(url.pathname.replace(/^\//, ''));
    if (!databaseName) throw new Error('Database URL is missing its database name');
    const queryHost = url.searchParams.get('host');
    const host = queryHost ? decodeURIComponent(queryHost) : url.hostname || undefined;
    const portText = url.port || url.searchParams.get('port');
    const port = portText ? Number(portText) : undefined;
    if (port !== undefined && (!Number.isSafeInteger(port) || port < 1 || port > 65_535)) {
      throw new Error('Database URL has an invalid port');
    }
    const user = url.username ? decodeURIComponent(url.username) : undefined;
    const password = url.password ? decodeURIComponent(url.password) : undefined;
    const sslMode = url.searchParams.get('sslmode');
    const ssl = sslMode === 'disable' ? false
      : sslMode === 'require' || sslMode === 'verify-ca' || sslMode === 'verify-full' ? 'require'
      : undefined;
    sql = postgres({
      ...connectionOptions,
      database: databaseName,
      ...(host === undefined ? {} : { host }),
      ...(port === undefined ? {} : { port }),
      ...(user === undefined ? {} : { user }),
      ...(password === undefined ? {} : { password }),
      ...(ssl === undefined ? {} : { ssl })
    });
  } else {
    sql = postgres(database, connectionOptions);
  }
  try {
    return await sql.begin('isolation level repeatable read read only', async (transaction) => {
      const tx = transaction as unknown as Sql;
      const identity = await databaseIdentity(tx);
      if (lock) assertDatabaseIdentity(identity, lock);
      // The generated projection performs large deterministic DISTINCT/ORDER BY
      // passes. PostgreSQL's 4 MiB default spills them heavily on the pinned
      // snapshot; this remains transaction-local and does not change output.
      await tx.unsafe("SET LOCAL work_mem = '256MB'");
      await exportSurfaceTsv(tx, join(temporary, 'surface.tsv'));
      const rootBuild = await deterministicRoot(tx);
      const detailBuild = await deterministicDetails(tx);
      const morphologyBuild = await deterministicMorphology(tx, join(root, 'data'));
      if (lock) {
        const lockedMorphology = lock.artifactDigests.morphology;
        if (morphologyBuild.bytes.byteLength !== lockedMorphology.bytes
          || sha256Bytes(morphologyBuild.bytes) !== lockedMorphology.sha256) {
          throw new Error('Compiled morphology section does not match the sources lock');
        }
      }
      // Keep the exhaustive gate on these exact bytes and in this transaction;
      // otherwise the relation digest would not attest the artifact we publish.
      const measuredMorphology = morphologyRelationAttestation(await verifyMorphologyRelation({
        lookup: openMorphology(morphologyBuild.bytes),
        sql: tx as unknown as Parameters<typeof verifyMorphologyRelation>[0]['sql'],
        exampleLimit: 0,
        onProgress: (groups, rows) => {
          console.error(`verified morphology ${groups.toLocaleString()} surfaces / ${rows.toLocaleString()} rows`);
        }
      }));
      if (lock) {
        assertBrowserAlphaMorphologyAttestation(
          measuredMorphology,
          lock.artifactDigests.morphologyRelation
        );
      }
      // This must remain sequential: the legacy cache connection override used
      // by the support freezer is process-global for the duration of this call.
      const supportBuild = await deterministicSupport(tx);
      return {
        root: rootBuild,
        details: detailBuild,
        morphology: morphologyBuild,
        morphologyRelation: measuredMorphology,
        ...supportBuild,
        database: identity
      };
    });
  } finally {
    await sql.end({ timeout: 5 });
  }
}

function exactObjectCounts(actual: object, expected: object, label: string): void {
  const values = actual as Record<string, unknown>;
  for (const [name, count] of Object.entries(expected)) {
    const value = values[name];
    assertExactCount(typeof value === 'number' ? value : -1, count, `${label}.${name}`);
  }
}

function assertArtifactCounts(builds: ComponentBuilds, lock: BrowserAlphaSourceLock): void {
  const expected = lock.artifacts;
  const actual = artifactCounts(builds);
  for (const name of [
    'surfaceIndex', 'rootPayload', 'morphology', 'analyzerSupport', 'annotations', 'details'
  ] as const) exactObjectCounts(actual[name], expected[name], name);
}

function componentDigests(builds: ComponentBuilds): Omit<BrowserAlphaArtifactDigests, 'morphologyRelation'> {
  const digest = (bytes: Uint8Array) => ({ bytes: bytes.byteLength, sha256: sha256Bytes(bytes) });
  return {
    surfaceIndex: digest(builds.surface.bytes),
    rootPayload: digest(builds.root.bytes),
    morphology: digest(builds.morphology.bytes),
    analyzerSupport: digest(builds.support.bytes),
    analyzerAnnotations: digest(builds.annotations.bytes),
    details: digest(builds.details.bytes)
  };
}

function assertArtifactDigests(builds: ComponentBuilds, lock: BrowserAlphaSourceLock): void {
  const expected = lock.artifactDigests;
  const actual = componentDigests(builds);
  for (const name of [
    'surfaceIndex', 'rootPayload', 'morphology', 'analyzerSupport',
    'analyzerAnnotations', 'details'
  ] as const) {
    if (actual[name].bytes !== expected[name].bytes) {
      throw new Error(
        `${name} is ${actual[name].bytes} bytes; sources lock requires ${expected[name].bytes}`
      );
    }
    if (actual[name].sha256 !== expected[name].sha256) {
      throw new Error(
        `${name} digest ${actual[name].sha256}; sources lock requires ${expected[name].sha256}`
      );
    }
  }
}

function artifactCounts(builds: ComponentBuilds): BrowserAlphaArtifactCounts {
  return {
    surfaceIndex: {
      input: builds.surface.stats.input,
      accepted: builds.surface.stats.accepted,
      direct: builds.surface.stats.direct,
      morphology: builds.surface.stats.morphology,
      overlap: builds.surface.stats.overlap,
      omitted: builds.surface.stats.omitted,
      states: builds.surface.stats.states,
      edges: builds.surface.stats.edges
    },
    rootPayload: {
      surfaces: builds.root.stats.counts.surfaces,
      forms: builds.root.stats.counts.forms,
      entries: builds.root.stats.counts.entries,
      restrictions: builds.root.stats.counts.restrictions
    },
    morphology: {
      positions: builds.morphology.stats.positions,
      rules: builds.morphology.stats.rules,
      templates: builds.morphology.stats.templates,
      suffixes: builds.morphology.stats.suffixes,
      rootKeys: builds.morphology.stats.rootKeys,
      rootGroups: builds.morphology.stats.rootGroups,
      patches: builds.morphology.stats.patches,
      tombstones: builds.morphology.stats.tombstones
    },
    analyzerSupport: {
      suffixKeys: builds.support.stats.counts.suffixKeys,
      suffixValues: builds.support.stats.counts.suffixValues,
      suffixClasses: builds.support.stats.counts.suffixClasses,
      counterKeys: builds.support.stats.counts.counterKeys,
      counterVariants: builds.support.stats.counts.counterVariants,
      collisions: builds.support.stats.counts.collisions,
      generatedRules: builds.support.stats.counts.generatedRules,
      generatedAliases: builds.support.stats.counts.generatedAliases
    },
    annotations: {
      blocks: builds.annotations.stats.blocks,
      splits: builds.annotations.stats.splits,
      hints: builds.annotations.stats.hints,
      generatedBlocks: builds.annotations.stats.generatedBlocks,
      generatedRoots: builds.annotations.stats.generatedRoots,
      generatedRecords: builds.annotations.stats.generatedRecords,
      lookupOrderRecords: builds.annotations.stats.lookupOrderRecords,
      lookupOrderRoots: builds.annotations.stats.lookupOrderRoots,
      lookupOrderBytes: builds.annotations.stats.lookupOrderBytes,
      lookupOrderExceptionSurfaces: builds.annotations.stats.lookupOrderExceptionSurfaces,
      lookupOrderExceptionClasses: builds.annotations.stats.lookupOrderExceptionClasses,
      lookupOrderExceptionLocators: builds.annotations.stats.lookupOrderExceptionLocators,
      lookupOrderExceptionBytes: builds.annotations.stats.lookupOrderExceptionBytes,
      generatedPhysicalGroups: builds.annotations.stats.generatedPhysicalGroups,
      generatedFactPairs: builds.annotations.stats.generatedFactPairs,
      indexBytes: builds.annotations.stats.indexBytes,
      uncompressedBytes: builds.annotations.stats.uncompressedBytes,
      compressedBytes: builds.annotations.stats.compressedBytes,
      annotationUncompressedBytes: builds.annotations.stats.annotationUncompressedBytes,
      annotationCompressedBytes: builds.annotations.stats.annotationCompressedBytes,
      generatedUncompressedBytes: builds.annotations.stats.generatedUncompressedBytes,
      generatedCompressedBytes: builds.annotations.stats.generatedCompressedBytes,
      totalBytes: builds.annotations.stats.totalBytes,
      largestUncompressedBlock: builds.annotations.stats.largestUncompressedBlock,
      largestGeneratedBlock: builds.annotations.stats.largestGeneratedBlock,
      largestGeneratedCompressedBlock: builds.annotations.stats.largestGeneratedCompressedBlock
    },
    details: {
      entries: builds.details.stats.entryCount,
      forms: builds.details.stats.formCount,
      senses: builds.details.stats.senseCount,
      glosses: builds.details.stats.glossCount,
      properties: builds.details.stats.propertyCount
    }
  };
}

function makeHotPack(builds: ComponentBuilds): Uint8Array {
  return encodePack([
    { id: SURFACE_INDEX_SECTION_ID, bytes: builds.surface.bytes },
    { id: ROOT_PAYLOAD_SECTION_ID, bytes: builds.root.bytes },
    { id: MORPHOLOGY_SECTION_ID, bytes: builds.morphology.bytes },
    { id: ANALYZER_SUPPORT_SECTION_ID, bytes: builds.support.bytes },
    { id: ANALYZER_ANNOTATIONS_SECTION_ID, bytes: builds.annotations.bytes }
  ]);
}

function statsReport(
  builds: ComponentBuilds,
  release: AnalyzerReleaseBuild,
  source: Awaited<ReturnType<typeof verifyBrowserAlphaSources>>,
  sourceCommitValue: string,
  shellBytes: number
): unknown {
  return {
    formatVersion: RELEASE_STATS_FORMAT_VERSION,
    packVersion: release.manifest.packVersion,
    sourceCommit: sourceCommitValue,
    sourcesLockSha256: source.lockSha256,
    database: {
      name: builds.database.name,
      postgresServerVersion: builds.database.postgresServerVersion,
      encoding: builds.database.encoding,
      collation: builds.database.collation,
      schemaSha256: source.lock.database.schemaSha256
    },
    artifacts: artifactCounts(builds),
    supportIssues: {
      count: builds.supportIssueCount,
      sha256: builds.supportIssuesSha256
    },
    morphologyRelation: builds.morphologyRelation,
    sections: [
      { id: 1, name: 'surface-index', bytes: builds.surface.bytes.byteLength, sha256: sha256Bytes(builds.surface.bytes) },
      { id: 2, name: 'root-payload', bytes: builds.root.bytes.byteLength, sha256: sha256Bytes(builds.root.bytes) },
      { id: 3, name: 'morphology', bytes: builds.morphology.bytes.byteLength, sha256: sha256Bytes(builds.morphology.bytes) },
      { id: 4, name: 'analyzer-support', bytes: builds.support.bytes.byteLength, sha256: sha256Bytes(builds.support.bytes) },
      { id: 5, name: 'analyzer-annotations', bytes: builds.annotations.bytes.byteLength, sha256: sha256Bytes(builds.annotations.bytes) }
    ],
    details: {
      bytes: builds.details.bytes.byteLength,
      sha256: sha256Bytes(builds.details.bytes)
    },
    section5: {
      rawBytes: builds.annotations.bytes.byteLength,
      internalCompressedBytes: builds.annotations.stats.compressedBytes,
      residentIndexBytes: builds.annotations.stats.indexBytes,
      annotationBlocks: builds.annotations.stats.blocks,
      generatedBlocks: builds.annotations.stats.generatedBlocks,
      largestGeneratedCompressedBlock: builds.annotations.stats.largestGeneratedCompressedBlock,
      largestGeneratedDecodedBlock: builds.annotations.stats.largestGeneratedBlock,
      decodedCacheBlocks: ANALYZER_GENERATED_CACHE_BLOCKS,
      decodedCacheUpperBound: builds.annotations.stats.largestGeneratedBlock
        * ANALYZER_GENERATED_CACHE_BLOCKS
    },
    sizes: assertAnalyzerReleaseSize(release, shellBytes)
  };
}

async function gzipDecode(bytes: Uint8Array): Promise<Uint8Array> {
  return new Uint8Array(gunzipSync(bytes));
}

function parseManifest(bytes: Uint8Array): AnalyzerReleaseManifest {
  const parsed: unknown = JSON.parse(new TextDecoder().decode(bytes));
  if (typeof parsed !== 'object' || parsed === null) throw new Error('Release manifest must be an object');
  return parsed as AnalyzerReleaseManifest;
}

async function verifyRelease(out: string, shellBytes: number): Promise<VerifiedAnalyzerRelease> {
  const manifestBytes = new Uint8Array(await readFile(join(out, 'manifest.json')));
  const manifest = parseManifest(manifestBytes);
  if (manifest.formatVersion !== ANALYZER_RELEASE_FORMAT_VERSION) {
    throw new Error(`Unsupported release manifest format ${String(manifest.formatVersion)}`);
  }
  if (manifest.hot.file !== 'hot.bin.gz' || manifest.hot.encoding !== 'gzip') {
    throw new Error('Release hot asset must be hot.bin.gz with gzip encoding');
  }
  if (manifest.details.file !== 'details.bin.gz' || manifest.details.encoding !== 'gzip') {
    throw new Error('Release details asset must be details.bin.gz with gzip encoding');
  }
  const hotDownload = new Uint8Array(await readFile(join(out, manifest.hot.file)));
  const detailsDownload = new Uint8Array(await readFile(join(out, manifest.details.file)));
  for (const [label, asset, download] of [
    ['hot', manifest.hot, hotDownload],
    ['details', manifest.details, detailsDownload]
  ] as const) {
    assertExactCount(download.byteLength, asset.downloadBytes, `${label} download bytes`);
    const digest = sha256Bytes(download);
    if (digest !== asset.downloadSha256) throw new Error(`${label} download digest mismatch`);
  }
  const hot = await gzipDecode(hotDownload);
  const details = await gzipDecode(detailsDownload);
  for (const [label, asset, installed] of [
    ['hot', manifest.hot, hot], ['details', manifest.details, details]
  ] as const) {
    assertExactCount(installed.byteLength, asset.installedBytes, `${label} installed bytes`);
    const digest = sha256Bytes(installed);
    if (digest !== asset.installedSha256) throw new Error(`${label} installed digest mismatch`);
  }
  const { manifestSha256: _digest, ...unsigned } = manifest;
  const expectedManifestDigest = sha256Bytes(
    new TextEncoder().encode(analyzerManifestDigestInput(unsigned))
  );
  if (manifest.manifestSha256 !== expectedManifestDigest) throw new Error('Release manifest digest mismatch');

  const pack = openPack(hot);
  pack.verifyAll();
  const ids = pack.manifest.sections.map((section) => section.id);
  if (ids.join(',') !== '1,2,3,4,5') throw new Error(`Hot pack sections must be 1,2,3,4,5; found ${ids}`);
  const surface = openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID));
  const root = openRootPayload(pack.getSection(ROOT_PAYLOAD_SECTION_ID));
  const morphology = openMorphology(pack.getSection(MORPHOLOGY_SECTION_ID));
  const support = openAnalyzerSupport(pack.getSection(ANALYZER_SUPPORT_SECTION_ID));
  const annotations = await AnalyzerAnnotationsReader.open(
    analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
    gzipDecode
  );
  const detailReader = await openDetailStore(memoryDetailSource(details), gzipDecode);
  let detailForms = 0;
  let detailSenses = 0;
  let detailGlosses = 0;
  let detailProperties = 0;
  for (let index = 0; index < detailReader.manifest.entryCount; index++) {
    const entry = await detailReader.entry(index);
    detailForms += entry.forms.length;
    detailSenses += entry.senses.length;
    for (const sense of entry.senses) {
      detailGlosses += sense.glosses.length;
      detailProperties += sense.properties.length;
    }
  }
  const release: VerifiedAnalyzerRelease = {
    manifest,
    manifestBytes,
    hotDownload,
    detailsDownload,
    inspection: {
      artifacts: {
        surfaceIndex: {
          input: surface.manifest.inputCount,
          accepted: surface.manifest.acceptedCount,
          direct: surface.manifest.directCount,
          morphology: surface.manifest.morphologyCount,
          overlap: surface.manifest.overlapCount,
          omitted: surface.manifest.inputCount - surface.manifest.acceptedCount,
          states: surface.manifest.stateCount,
          edges: surface.manifest.edgeCount
        },
        rootPayload: {
          surfaces: root.surfaceCount,
          forms: root.formCount,
          entries: root.entryCount,
          restrictions: root.restrictionCount
        },
        morphology: {
          positions: morphology.stats.positions,
          rules: morphology.stats.rules,
          templates: morphology.stats.templates,
          suffixes: morphology.stats.suffixes,
          rootKeys: morphology.stats.rootKeys,
          rootGroups: morphology.stats.rootGroups,
          patches: morphology.stats.patches,
          tombstones: morphology.stats.tombstones
        },
        analyzerSupport: {
          suffixKeys: support.stats.suffixKeys,
          suffixValues: support.stats.suffixValues,
          suffixClasses: support.stats.suffixClasses,
          counterKeys: support.stats.counterKeys,
          counterVariants: support.stats.counterVariants,
          collisions: support.stats.collisions,
          generatedRules: support.stats.generatedRules,
          generatedAliases: support.stats.generatedAliases
        },
        annotations: {
          blocks: annotations.manifest.blocks,
          splits: annotations.manifest.splits,
          hints: annotations.manifest.hints,
          generatedBlocks: annotations.manifest.generatedBlocks,
          generatedRoots: annotations.manifest.generatedRoots,
          generatedRecords: annotations.manifest.generatedRecords,
          lookupOrderRecords: annotations.manifest.lookupOrderRecords,
          lookupOrderRoots: annotations.manifest.lookupOrderRoots,
          lookupOrderBytes: annotations.manifest.lookupOrderRecords
            * ANALYZER_LOOKUP_ORDER_RECORD_BYTES,
          lookupOrderExceptionSurfaces: annotations.manifest.lookupOrderExceptionSurfaces,
          lookupOrderExceptionClasses: annotations.manifest.lookupOrderExceptionClasses,
          lookupOrderExceptionLocators: annotations.manifest.lookupOrderExceptionLocators,
          lookupOrderExceptionBytes: annotations.manifest.lookupOrderExceptionBytes,
          generatedPhysicalGroups: annotations.manifest.generatedPhysicalGroups,
          generatedFactPairs: annotations.manifest.generatedFactPairs,
          indexBytes: annotations.manifest.residentIndexBytes,
          uncompressedBytes: annotations.manifest.uncompressedBytes,
          compressedBytes: annotations.manifest.compressedBytes,
          annotationUncompressedBytes: annotations.manifest.uncompressedBytes
            - annotations.manifest.generatedUncompressedBytes,
          annotationCompressedBytes: annotations.manifest.compressedBytes
            - annotations.manifest.generatedCompressedBytes,
          generatedUncompressedBytes: annotations.manifest.generatedUncompressedBytes,
          generatedCompressedBytes: annotations.manifest.generatedCompressedBytes,
          totalBytes: annotations.manifest.byteLength,
          largestUncompressedBlock: annotations.manifest.largestUncompressedBlock,
          largestGeneratedBlock: annotations.manifest.largestGeneratedBlock,
          largestGeneratedCompressedBlock: annotations.manifest.largestGeneratedCompressedBlock
        },
        details: {
          entries: detailReader.manifest.entryCount,
          forms: detailForms,
          senses: detailSenses,
          glosses: detailGlosses,
          properties: detailProperties
        }
      },
      sections: pack.manifest.sections.map((section) => {
        const bytes = pack.getSection(section.id);
        return { id: section.id, bytes: bytes.byteLength, sha256: sha256Bytes(bytes) };
      }),
      details: { bytes: details.byteLength, sha256: sha256Bytes(details) }
    }
  };
  assertAnalyzerReleaseSize(release, shellBytes);
  return release;
}

async function publish(
  out: string,
  release: AnalyzerReleaseBuild,
  reportBytes: Uint8Array
): Promise<void> {
  const parent = dirname(out);
  await mkdir(parent, { recursive: true });
  const stage = await mkdtemp(join(parent, '.browser-alpha-release-'));
  try {
    const files = new Map<string, Uint8Array>([
      ['hot.bin.gz', release.hotDownload],
      ['details.bin.gz', release.detailsDownload],
      ['manifest.json', release.manifestBytes],
      ['stats.json', reportBytes]
    ]);
    for (const [name, bytes] of files) await writeFile(join(stage, name), bytes, { flag: 'wx' });
    await verifyRelease(stage, (JSON.parse(new TextDecoder().decode(reportBytes)) as { sizes: { shellBytes: number } }).sizes.shellBytes);
    await mkdir(out, { recursive: true });
    for (const name of RELEASE_FILENAMES) await rename(join(stage, name), join(out, name));
  } finally {
    await rm(stage, { recursive: true, force: true });
  }
}

async function compileComponents(
  root: string,
  database: string,
  temporary: string,
  lock?: BrowserAlphaSourceLock
): Promise<ComponentBuilds> {
  const compiler = await buildSurfaceCompiler(root);
  const partial = await loadComponents(root, database, temporary, lock);
  const firstSurfacePath = join(temporary, 'surface-first.bin');
  const secondSurfacePath = join(temporary, 'surface-second.bin');
  const firstStats = await runSurfaceCompiler(
    compiler, join(temporary, 'surface.tsv'), firstSurfacePath, root
  );
  const secondStats = await runSurfaceCompiler(
    compiler, join(temporary, 'surface.tsv'), secondSurfacePath, root
  );
  if (JSON.stringify(firstStats) !== JSON.stringify(secondStats)) {
    throw new Error('Surface-index rebuild changed compiler counts');
  }
  const surfaceBytes = new Uint8Array(await readFile(firstSurfacePath));
  assertBytesEqual(surfaceBytes, new Uint8Array(await readFile(secondSurfacePath)), 'Surface index');
  return { ...partial, surface: { bytes: surfaceBytes, stats: firstStats } };
}

async function build(options: CliOptions, root: string): Promise<void> {
  const source = await verifyBrowserAlphaSources(root);
  await verifyBrowserAlphaOracleCore(root, source.lock.postgresReference.repositoryCommit);
  verifyBrowserAlphaToolchain(source.lock.toolchain, await actualToolchain());
  const commit = await sourceCommit(root);
  const temporary = await mkdtemp(join(tmpdir(), 'ichiran-browser-alpha-'));
  try {
    const builds = await compileComponents(root, options.database!, temporary, source.lock);
    assertArtifactCounts(builds, source.lock);
    assertArtifactDigests(builds, source.lock);

    const hot = makeHotPack(builds);
    assertBytesEqual(hot, makeHotPack(builds), 'Hot pack');
    const releaseOptions = {
      packVersion: options.packVersion!,
      sourceCommit: commit,
      sourcesLockSha256: source.lockSha256,
      hot,
      details: builds.details.bytes,
      hotEncoding: 'gzip',
      detailsEncoding: 'gzip'
    } as const;
    const release = buildAnalyzerRelease(releaseOptions);
    const rebuilt = buildAnalyzerRelease(releaseOptions);
    assertBytesEqual(release.hotDownload, rebuilt.hotDownload, 'Compressed hot asset');
    assertBytesEqual(release.detailsDownload, rebuilt.detailsDownload, 'Compressed details asset');
    assertBytesEqual(release.manifestBytes, rebuilt.manifestBytes, 'Release manifest');
    assertAnalyzerReleaseSize(release, options.shellBytes!);
    const report = deterministicJson(statsReport(builds, release, source, commit, options.shellBytes!));
    await publish(releaseOutputPath(root, options.out!), release, report);
    console.log(new TextDecoder().decode(report).trimEnd());
  } finally {
    await rm(temporary, { recursive: true, force: true });
  }
}

async function refreshLock(options: CliOptions, root: string): Promise<void> {
  const oracle = await upstreamOracle(root);
  await verifyBrowserAlphaOracleCore(root, FROZEN_POSTGRES_REFERENCE_COMMIT);
  const toolchain = await actualToolchain();
  const sources = await measureReleaseSources(root);
  const temporary = await mkdtemp(join(tmpdir(), 'ichiran-browser-alpha-lock-'));
  try {
    // Deliberately omit a lock: this is the only command allowed to establish
    // new expected artifact identities from a target database.
    const builds = await compileComponents(root, options.database!, temporary);
    const lock: BrowserAlphaSourceLock = {
      formatVersion: 2,
      upstreamIchiran: {
        repository: oracle.ichiran.repository,
        commit: oracle.ichiran.commit,
        dataReleaseTag: oracle.ichiran.dataReleaseTag
      },
      postgresReference: {
        repositoryCommit: FROZEN_POSTGRES_REFERENCE_COMMIT
      },
      databaseDump: {
        url: oracle.databaseDump.url,
        bytes: oracle.databaseDump.bytes,
        sha256: oracle.databaseDump.sha256
      },
      database: {
        name: builds.database.name,
        postgresServerVersion: builds.database.postgresServerVersion,
        encoding: builds.database.encoding,
        collation: builds.database.collation,
        ctype: builds.database.ctype,
        schemaSha256: oracle.qualifiedOracle.normalizedPgDump16SchemaSha256
      },
      toolchain,
      sources,
      artifacts: artifactCounts(builds),
      artifactDigests: {
        ...componentDigests(builds),
        morphologyRelation: builds.morphologyRelation
      }
    };
    const lockBytes = deterministicJson(lock);
    parseBrowserAlphaSourceLock(new TextDecoder().decode(lockBytes));
    const destination = join(root, BROWSER_ALPHA_SOURCES_LOCK);
    const stage = await mkdtemp(join(dirname(destination), '.sources-lock-'));
    try {
      const staged = join(stage, 'sources.lock.json');
      await writeFile(staged, lockBytes, { flag: 'wx' });
      await rename(staged, destination);
    } finally {
      await rm(stage, { recursive: true, force: true });
    }
    console.log(JSON.stringify({
      refreshed: true,
      lock: BROWSER_ALPHA_SOURCES_LOCK,
      sha256: sha256Bytes(lockBytes),
      upstreamIchiranCommit: lock.upstreamIchiran.commit,
      postgresReferenceCommit: lock.postgresReference.repositoryCommit,
      database: lock.database.name,
      artifacts: lock.artifactDigests
    }, null, 2));
  } finally {
    await rm(temporary, { recursive: true, force: true });
  }
}

async function verify(options: CliOptions, root: string): Promise<void> {
  const source = await verifyBrowserAlphaSources(root);
  await verifyBrowserAlphaOracleCore(root, source.lock.postgresReference.repositoryCommit);
  verifyBrowserAlphaToolchain(source.lock.toolchain, await actualToolchain());
  const output = releaseOutputPath(root, options.out!);
  const release = await verifyRelease(output, options.shellBytes!);
  if (release.manifest.sourcesLockSha256 !== source.lockSha256) {
    throw new Error('Release manifest does not point to the current sources lock');
  }
  const reportPath = join(output, 'stats.json');
  const reportBytes = new Uint8Array(await readFile(reportPath));
  const report = JSON.parse(new TextDecoder().decode(reportBytes)) as {
    formatVersion?: number;
    packVersion?: string;
    sourceCommit?: string;
    sourcesLockSha256?: string;
    artifacts?: BrowserAlphaArtifactCounts;
    supportIssues?: { count?: number; sha256?: string };
    morphologyRelation?: BrowserAlphaMorphologyAttestation;
    sections?: readonly { id?: number; name?: string; bytes?: number; sha256?: string }[];
    details?: { bytes?: number; sha256?: string };
    section5?: {
      rawBytes?: number;
      internalCompressedBytes?: number;
      residentIndexBytes?: number;
      annotationBlocks?: number;
      generatedBlocks?: number;
      largestGeneratedCompressedBlock?: number;
      largestGeneratedDecodedBlock?: number;
      decodedCacheBlocks?: number;
      decodedCacheUpperBound?: number;
    };
    sizes?: {
      hotBytes?: number;
      persistedBytes?: number;
      wireBytes?: number;
      shellBytes?: number;
      cachedManifestBytes?: number;
      installedMarkerBytes?: number;
    };
  };
  assertBytesEqual(reportBytes, deterministicJson(report), 'Stats report');
  if (report.formatVersion !== RELEASE_STATS_FORMAT_VERSION) throw new Error('Unsupported stats format');
  if (report.packVersion !== release.manifest.packVersion) throw new Error('Stats pack version mismatch');
  if (report.sourceCommit !== release.manifest.sourceCommit) throw new Error('Stats source commit mismatch');
  if (report.sourcesLockSha256 !== source.lockSha256) throw new Error('Stats sources-lock digest mismatch');
  if (report.sizes?.shellBytes !== options.shellBytes) throw new Error('Stats shell-byte input mismatch');
  if (!report.artifacts || !report.sections || !report.details) {
    throw new Error('Stats report is missing artifact measurements');
  }
  for (const name of [
    'surfaceIndex', 'rootPayload', 'morphology', 'analyzerSupport', 'annotations', 'details'
  ] as const) {
    exactObjectCounts(report.artifacts[name], source.lock.artifacts[name], `stats.artifacts.${name}`);
    exactObjectCounts(
      release.inspection.artifacts[name],
      source.lock.artifacts[name],
      `release.artifacts.${name}`
    );
  }
  const componentNames = [
    'surfaceIndex', 'rootPayload', 'morphology', 'analyzerSupport', 'analyzerAnnotations'
  ] as const;
  if (report.sections.length !== componentNames.length) throw new Error('Stats section count mismatch');
  for (let index = 0; index < componentNames.length; index++) {
    const expected = source.lock.artifactDigests[componentNames[index]!];
    const measured = release.inspection.sections[index]!;
    const reported = report.sections[index]!;
    if (measured.id !== index + 1 || reported.id !== measured.id) throw new Error('Stats section ID mismatch');
    if (reported.bytes !== measured.bytes || measured.bytes !== expected.bytes) {
      throw new Error(`Section ${measured.id} byte length mismatch`);
    }
    if (reported.sha256 !== measured.sha256 || measured.sha256 !== expected.sha256) {
      throw new Error(`Section ${measured.id} digest mismatch`);
    }
  }
  const expectedDetails = source.lock.artifactDigests.details;
  if (
    report.details.bytes !== release.inspection.details.bytes
    || release.inspection.details.bytes !== expectedDetails.bytes
    || report.details.sha256 !== release.inspection.details.sha256
    || release.inspection.details.sha256 !== expectedDetails.sha256
  ) throw new Error('Stats details artifact mismatch');
  const annotationInspection = release.inspection.artifacts.annotations;
  const expectedSection5 = {
    rawBytes: annotationInspection.totalBytes,
    internalCompressedBytes: annotationInspection.compressedBytes,
    residentIndexBytes: annotationInspection.indexBytes,
    annotationBlocks: annotationInspection.blocks,
    generatedBlocks: annotationInspection.generatedBlocks,
    largestGeneratedCompressedBlock: annotationInspection.largestGeneratedCompressedBlock,
    largestGeneratedDecodedBlock: annotationInspection.largestGeneratedBlock,
    decodedCacheBlocks: ANALYZER_GENERATED_CACHE_BLOCKS,
    decodedCacheUpperBound: annotationInspection.largestGeneratedBlock
      * ANALYZER_GENERATED_CACHE_BLOCKS
  };
  if (JSON.stringify(report.section5) !== JSON.stringify(expectedSection5)) {
    throw new Error('Stats section-5 measurements mismatch');
  }
  if (!report.morphologyRelation) throw new Error('Stats morphology relation attestation is missing');
  assertBrowserAlphaMorphologyAttestation(
    report.morphologyRelation,
    source.lock.artifactDigests.morphologyRelation
  );
  const emptyDigest = sha256Bytes(new Uint8Array());
  if (report.supportIssues?.count !== 0 || report.supportIssues.sha256 !== emptyDigest) {
    throw new Error('Stats report contains unresolved analyzer-support issues');
  }
  const expectedSizes = assertAnalyzerReleaseSize(release, options.shellBytes!);
  if (JSON.stringify(report.sizes) !== JSON.stringify(expectedSizes)) {
    throw new Error('Stats release-size report mismatch');
  }
  const currentCommit = await sourceCommit(root);
  if (release.manifest.sourceCommit !== currentCommit) {
    throw new Error('Release source commit is not the current checkout commit');
  }
  const info = await stat(reportPath);
  if (!info.isFile()) throw new Error('stats.json is not a file');
  console.log(JSON.stringify({
    verified: true,
    packVersion: release.manifest.packVersion,
    sourceCommit: release.manifest.sourceCommit,
    sourcesLockSha256: source.lockSha256,
    sizes: expectedSizes
  }, null, 2));
}

async function main(): Promise<void> {
  const options = parseArgs(process.argv.slice(2));
  const root = await repositoryRoot();
  // Reject an unsafe/unsupported target before the multi-minute database
  // projection. build() resolves it again at publication as a final guard.
  if (options.out) releaseOutputPath(root, options.out);
  await assertCleanSource(root, options.allowDirty);
  if (options.command === 'build') await build(options, root);
  else if (options.command === 'verify') await verify(options, root);
  else await refreshLock(options, root);
}

await main().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
