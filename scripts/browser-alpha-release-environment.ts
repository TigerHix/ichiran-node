import { execFile as execFileCallback } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import { join, relative, resolve, sep } from 'node:path';
import { promisify } from 'node:util';

import {
  BROWSER_ALPHA_UPSTREAM_ORACLE,
  sha256Bytes
} from '../packages/data/src/browser-pack/release-orchestration.js';

const execFile = promisify(execFileCallback);
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

export interface UpstreamOracle {
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

export interface CliOptions {
  readonly command: 'build' | 'verify' | 'refresh-lock';
  readonly database?: string;
  readonly out?: string;
  readonly packVersion?: string;
  readonly shellDir?: string;
  readonly allowDirty: boolean;
}

function usage(message?: string): never {
  if (message) console.error(`error: ${message}\n`);
  console.error(`usage:
  bun run alpha:release:build -- --database <url> --out <directory> \\
    --pack-version <version> --shell-dir <production-dist> [--allow-dirty]
  bun run alpha:release:verify -- --out <directory> --shell-dir <production-dist> [--allow-dirty]
  bun run alpha:release:refresh-lock -- --database <url> [--allow-dirty]`);
  process.exit(2);
}

export function parseArgs(argv: readonly string[]): CliOptions {
  const command = argv[0];
  if (command !== 'build' && command !== 'verify' && command !== 'refresh-lock') {
    usage('first argument must be build, verify, or refresh-lock');
  }
  let database: string | undefined;
  let out: string | undefined;
  let packVersion: string | undefined;
  let shellDir: string | undefined;
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
    else if (argument === '--shell-dir') shellDir = next();
    else if (argument === '--allow-dirty') allowDirty = true;
    else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (command !== 'refresh-lock' && !out) usage('--out is required');
  if (command !== 'refresh-lock' && shellDir === undefined) usage('--shell-dir is required');
  if ((command === 'build' || command === 'refresh-lock') && !database) {
    usage(`--database is required for ${command}`);
  }
  if (command === 'build' && !packVersion) usage('--pack-version is required for build');
  if (command === 'verify' && (database || packVersion)) {
    usage('--database and --pack-version apply only to build');
  }
  if (command === 'refresh-lock' && (out || packVersion || shellDir !== undefined)) {
    usage('--out, --pack-version, and --shell-dir do not apply to refresh-lock');
  }
  return { command, database, out, packVersion, shellDir, allowDirty };
}

export async function repositoryRoot(): Promise<string> {
  const { stdout } = await execFile('git', ['rev-parse', '--show-toplevel'], { encoding: 'utf8' });
  return stdout.trim();
}

export async function sourceCommit(root: string): Promise<string> {
  const { stdout } = await execFile('git', ['-C', root, 'rev-parse', 'HEAD'], { encoding: 'utf8' });
  const commit = stdout.trim();
  if (!/^[0-9a-f]{40}$/.test(commit)) throw new Error(`Git returned invalid source commit ${commit}`);
  return commit;
}

export async function upstreamOracle(root: string): Promise<UpstreamOracle> {
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
  if (!/^[0-9a-f]{40}$/.test(oracle.ichiran.commit)) throw new Error('Invalid upstream commit');
  if (typeof oracle.databaseDump.url !== 'string'
    || !Number.isSafeInteger(oracle.databaseDump.bytes)
    || oracle.databaseDump.bytes <= 0) {
    throw new Error('Upstream database dump URL and byte length are invalid');
  }
  for (const [value, label] of [
    [oracle.databaseDump.sha256, 'Upstream database dump digest'],
    [oracle.qualifiedOracle.normalizedPgDump16SchemaSha256, 'Qualified database schema digest']
  ] as const) {
    if (!/^[0-9a-f]{64}$/.test(value)) throw new Error(`${label} must be a lowercase SHA-256`);
  }
  return oracle as UpstreamOracle;
}

export async function measureReleaseSources(root: string) {
  return await Promise.all(RELEASE_SOURCE_PATHS.map(async path => {
    const bytes = new Uint8Array(await readFile(join(root, path)));
    return { path, bytes: bytes.byteLength, sha256: sha256Bytes(bytes) };
  }));
}

export async function assertCleanSource(root: string, allowDirty: boolean): Promise<void> {
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

export function releaseOutputPath(root: string, value: string): string {
  const output = resolve(root, value);
  const within = relative(root, output);
  if (within === '' || within === '..' || within.startsWith(`..${sep}`)) {
    throw new Error('Release output must be a directory below the repository root');
  }
  return output;
}
