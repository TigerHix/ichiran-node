import { readdir, readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
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
const main = (await Promise.all(
  scripts.filter(name => name !== workerName).map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n');
const all = `${main}\n${worker}`;

if (!worker.includes('ICHIPACK') || !worker.includes('ichiran-browser-alpha')) {
  throw new Error('Analyzer pack/runtime is not linked into the Worker');
}
for (const forbidden of ['ICHIPACK', 'PortableAnalyzer', 'AnalyzerRuntime']) {
  if (main.includes(forbidden)) throw new Error(`${forbidden} leaked into the main-thread bundle`);
}
for (const forbidden of [
  'postgres', 'node:fs', 'node:path', 'node:async_hooks', 'async_hooks', 'kernel-not-ready'
]) {
  if (all.includes(forbidden)) throw new Error(`Browser bundle contains forbidden runtime text ${forbidden}`);
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
  staged = await verifyAnalyzerRelease(stagedDirectory, repositoryRoot);
} catch (error) {
  const missing = error instanceof Error
    && 'code' in error
    && (error as NodeJS.ErrnoException).code === 'ENOENT';
  if (!missing || requireAnalyzer || releaseDirectory !== null) throw error;
}
if (staged && releaseDirectory !== null) {
  const source = await verifyAnalyzerRelease(resolve(repositoryRoot, releaseDirectory), repositoryRoot);
  assertSameRelease(staged, source);
}

console.log(
  `Browser build audit passed: ${workerName}; analyzer code is Worker-only; `
  + (staged ? `release ${staged.manifest.packVersion} is current and verified` : 'no analyzer release staged')
);
