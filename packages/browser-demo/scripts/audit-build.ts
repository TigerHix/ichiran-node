import { readdir, readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { assertSameRelease, verifyAnalyzerRelease } from './release-files.js';

let requireAnalyzer = false;
let releaseDirectory: string | null = null;
for (let index = 2; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument === '--require-analyzer') requireAnalyzer = true;
  else if (argument === '--release') {
    const value = process.argv[++index];
    if (!value) throw new Error('--release requires a directory');
    releaseDirectory = value;
  } else {
    throw new Error(`Unknown build-audit argument: ${argument}`);
  }
}

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const output = join(packageRoot, 'dist');
const assetDirectory = join(output, 'assets');
const scripts = (await readdir(assetDirectory)).filter(name => name.endsWith('.js'));
const workerName = scripts.find(name => name.startsWith('analyzer.worker-'));
if (!workerName) throw new Error('Production analyzer Worker chunk is missing');

const worker = await readFile(join(assetDirectory, workerName), 'utf8');
const mainNames = scripts.filter(name => name !== workerName);
const main = (await Promise.all(
  mainNames.map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n');

// The lazily loaded benchmark corpus is inert JSON provenance/data. Its source
// paths may name reference-postgres tests without introducing runtime code.
const runtimeNames = mainNames.filter(name => !name.startsWith('benchmark-corpus-'));
const runtime = `${(await Promise.all(
  runtimeNames.map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n')}\n${worker}`;

const typescriptOracle = process.env.ICHIRAN_TYPESCRIPT_ORACLE === '1';
if (!typescriptOracle) {
  const assets = await readdir(assetDirectory);
  const wasm = assets.filter(name =>
    name.startsWith('ichiran_kernel_bg-') && name.endsWith('.wasm.gz.bin')
  );
  const rawWasm = assets.filter(name =>
    name.startsWith('ichiran_kernel_bg-') && name.endsWith('.wasm')
  );
  if (wasm.length !== 1 || rawWasm.length !== 0 || !worker.includes('ichiran-browser-alpha')) {
    throw new Error('Rust kernel, adapter, or Worker install lifecycle is missing');
  }
  const compressed = await readFile(join(assetDirectory, wasm[0]!));
  const decoded = gunzipSync(compressed);
  if (
    decoded.byteLength - compressed.byteLength < 200 * 1024
    || decoded[0] !== 0
    || decoded[1] !== 0x61
    || decoded[2] !== 0x73
    || decoded[3] !== 0x6d
  ) {
    throw new Error('Rust kernel shell asset is invalid or saves less than 200 KiB');
  }
} else if (!worker.includes('ICHIPACK') || !worker.includes('ichiran-browser-alpha')) {
  throw new Error('Analyzer pack/runtime is not linked into the Worker');
}
for (const forbidden of ['ICHIPACK', 'PortableAnalyzer', 'AnalyzerRuntime']) {
  if (main.includes(forbidden)) throw new Error(`${forbidden} leaked into the main-thread bundle`);
}
for (const forbidden of [
  'postgres', 'node:fs', 'node:path', 'node:async_hooks', 'async_hooks', 'kernel-not-ready'
]) {
  if (runtime.includes(forbidden)) {
    throw new Error(`Browser bundle contains forbidden runtime text ${forbidden}`);
  }
}

const serviceWorker = await readFile(join(output, 'sw.js'), 'utf8');
if (serviceWorker.includes('/analyzer/hot.bin') || serviceWorker.includes('/analyzer/details.bin')) {
  throw new Error('Service Worker must not duplicate analyzer data in Cache Storage');
}
if (!serviceWorker.includes('/analyzer/manifest.json')) {
  throw new Error('Service Worker does not preserve the pinned manifest for offline reopen');
}
if (serviceWorker.includes('cache.put(') || !serviceWorker.includes('CORE_PATHS.has')) {
  throw new Error('Service Worker must cache only the finalized shell allowlist');
}
if (!serviceWorker.includes("key.startsWith('ichiran-shell-')")) {
  throw new Error('Service Worker cache cleanup is not scoped to this app');
}
if (/__CACHE_VERSION__|\/\*__PRECACHE__\*\//.test(serviceWorker)) {
  throw new Error('Service Worker contains an unfinalized cache marker');
}

const stagedDirectory = join(output, 'analyzer');
let staged = null;
try {
  staged = await verifyAnalyzerRelease(
    stagedDirectory,
    repositoryRoot,
    process.env.ICHIRAN_QUALIFIED_ARTIFACT
  );
} catch (error) {
  const missing = error instanceof Error
    && 'code' in error
    && (error as NodeJS.ErrnoException).code === 'ENOENT';
  if (!missing || requireAnalyzer || releaseDirectory !== null) throw error;
}
if (staged && releaseDirectory !== null) {
  const source = await verifyAnalyzerRelease(
    resolve(repositoryRoot, releaseDirectory),
    repositoryRoot,
    process.env.ICHIRAN_QUALIFIED_ARTIFACT
  );
  assertSameRelease(staged, source);
}

console.log(
  `Browser build audit passed: ${workerName}; analyzer code is Worker-only; `
  + (staged ? `release ${staged.manifest.packVersion} is current and verified` : 'no analyzer release staged')
);
