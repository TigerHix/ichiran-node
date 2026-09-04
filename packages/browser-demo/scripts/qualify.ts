import { spawn } from 'node:child_process';
import { readFile, realpath } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { TypeScriptOracleRuntime } from '@ichiran/core/qualification';
import {
  assertAnalyzerReadyStateSize,
  verifyAnalyzerRelease
} from './release-files.js';

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
function gitOutput(args: readonly string[]): string {
  const result = Bun.spawnSync(['git', '-C', repositoryRoot, ...args]);
  if (result.exitCode !== 0) throw new Error(`git ${args.join(' ')} failed`);
  return result.stdout.toString().trim();
}
function assertCleanCheckout(expectedCommit: string): void {
  const result = Bun.spawnSync([
    'git', '-C', repositoryRoot, 'status', '--porcelain=v1', '--untracked-files=all'
  ]);
  if (result.exitCode !== 0 || result.stdout.byteLength !== 0) {
    throw new Error('Browser qualification requires a clean source checkout');
  }
  if (gitOutput(['rev-parse', 'HEAD']) !== expectedCommit) {
    throw new Error('Browser qualification source commit changed during the run');
  }
}
if (process.env.ICHIRAN_TYPESCRIPT_ORACLE === '1') {
  throw new Error(
    'Production browser qualification requires the Rust kernel; '
    + 'use build:qualification-typescript-oracle only for the frozen transition oracle'
  );
}
if (process.env.ICHIRAN_QUALIFIED_ARTIFACT !== undefined) {
  throw new Error('Source browser qualification does not accept ICHIRAN_QUALIFIED_ARTIFACT');
}
const qualificationCommit = gitOutput(['rev-parse', 'HEAD']);
assertCleanCheckout(qualificationCommit);
const productionEnvironment = { ...process.env };
delete productionEnvironment.ICHIRAN_TYPESCRIPT_ORACLE;
delete productionEnvironment.ICHIRAN_QUALIFIED_ARTIFACT;
let release = resolve(repositoryRoot, 'dist', 'browser-alpha');
let sourceLock: string | undefined;
for (let index = 2; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument === '--release') {
    const value = process.argv[++index];
    if (!value) throw new Error('--release requires a directory');
    release = resolve(repositoryRoot, value);
  } else if (argument === '--source-lock') {
    sourceLock = process.argv[++index];
    if (!sourceLock) throw new Error('--source-lock requires a file');
  } else {
    throw new Error(`Unknown qualification argument: ${argument}`);
  }
}
release = await realpath(release);
if (sourceLock) productionEnvironment.ICHIRAN_SOURCE_LOCK = sourceLock;

async function run(
  command: string,
  args: readonly string[],
  cwd: string,
  capture = false,
  environment = process.env
): Promise<string> {
  const child = spawn(command, args, {
    cwd,
    env: environment,
    stdio: capture ? ['ignore', 'pipe', 'inherit'] : 'inherit'
  });
  let stdout = '';
  if (capture && child.stdout) {
    child.stdout.setEncoding('utf8');
    child.stdout.on('data', chunk => { stdout += chunk; });
  }
  const code = await new Promise<number | null>((resolveExit, reject) => {
    child.once('error', reject);
    child.once('exit', resolveExit);
  });
  if (code !== 0) throw new Error(`${command} ${args.join(' ')} exited ${code}`);
  return stdout.trim();
}

const verifiedRelease = await verifyAnalyzerRelease(
  release,
  repositoryRoot,
  { sourceLock }
);

const installed = (bytes: Uint8Array, encoding: 'identity' | 'gzip'): Uint8Array =>
  encoding === 'gzip' ? new Uint8Array(gunzipSync(bytes)) : bytes.slice();

const hot = installed(verifiedRelease.hotBytes, verifiedRelease.manifest.hot.encoding);
const lexicon = installed(
  verifiedRelease.lexiconBytes,
  verifiedRelease.manifest.lexicon.encoding
);
const locales = Object.fromEntries(Object.entries(verifiedRelease.localeBytes).map(
  ([locale, bytes]) => {
    const decoded = installed(bytes, verifiedRelease.manifest.locales[locale]!.encoding);
    return [locale, {
      byteLength: decoded.byteLength,
      read: async (offset: number, byteLength: number) => decoded.slice(offset, offset + byteLength)
    }] as const;
  }
));
const oracle = await TypeScriptOracleRuntime.open({
  hot,
  lexicon: {
    sha256: verifiedRelease.manifest.lexicon.installedSha256,
    source: {
      byteLength: lexicon.byteLength,
      read: async (offset, byteLength) => lexicon.slice(offset, offset + byteLength)
    }
  },
  locales,
  decodeGzip: async (compressed, expectedByteLength) => {
    const decoded = new Uint8Array(gunzipSync(compressed));
    if (decoded.byteLength !== expectedByteLength) {
      throw new Error(`gzip decoded ${decoded.byteLength}; expected ${expectedByteLength}`);
    }
    return decoded;
  }
});
const witnessInputs = JSON.parse(await readFile(resolve(
  repositoryRoot,
  'packages/rust-kernel/tests/fixtures/m1-oracle.json'
), 'utf8')) as readonly {
  readonly name: string;
  readonly codeUnits: readonly number[];
}[];
const samePackWitnesses = [];
for (const witness of witnessInputs) {
  const text = String.fromCharCode(...witness.codeUnits);
  const result = { ...await oracle.analyzeProduct(text, { limit: 1 }), computeMs: 0 };
  samePackWitnesses.push({ ...witness, serialized: JSON.stringify(result) });
}

await run('bun', [
  'scripts/stage-analyzer.ts', release,
  ...(sourceLock ? ['--source-lock', sourceLock] : [])
], packageRoot, false, productionEnvironment);
await run('bun', ['run', 'build'], packageRoot, false, productionEnvironment);
await run('bun', [
  'scripts/audit-build.ts', '--require-rust', '--require-analyzer', '--release', release,
  ...(sourceLock ? ['--source-lock', sourceLock] : [])
], packageRoot, false, productionEnvironment);
const releaseDownloadBytes = verifiedRelease.manifestBytes.byteLength
  + verifiedRelease.hotBytes.byteLength
  + verifiedRelease.lexiconBytes.byteLength
  + Object.values(verifiedRelease.localeBytes).reduce(
    (total, bytes) => total + bytes.byteLength,
    0
  );
const firstInstallLimit = 36 * 1024 * 1024;
if (releaseDownloadBytes > firstInstallLimit) {
  throw new Error(
    `Analyzer download bytes ${releaseDownloadBytes} exceed the ${firstInstallLimit}-byte limit`
  );
}
const readyState = assertAnalyzerReadyStateSize(verifiedRelease.manifest);
await run('bun', ['run', 'build:qualification-browser'], packageRoot, false, productionEnvironment);
await run('bun', ['run', 'test:e2e'], packageRoot, false, {
  ...productionEnvironment,
  ICHIRAN_BROWSER_QUALIFICATION: '1',
  ICHIRAN_E2E_M1_WITNESSES: JSON.stringify(samePackWitnesses)
});
assertCleanCheckout(qualificationCommit);

console.log(
  `Browser qualification passed for ${release}: ${releaseDownloadBytes} analyzer download bytes; `
  + `${readyState.persistedBytes} ready-state persisted bytes`
);
