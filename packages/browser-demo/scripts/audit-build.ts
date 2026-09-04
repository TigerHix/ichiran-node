import { readdir, readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { assertSameRelease, verifyAnalyzerRelease } from './release-files.js';
import {
  assertAnalyzerWorkerOnly,
  assertRustRuntimeGraph,
  findTypeScriptOracleWorkerChunks
} from './build-audit-policy.js';

let requireAnalyzer = false;
let requireRust = false;
let typescriptOracle = false;
let qualificationBuild = false;
let releaseDirectory: string | null = null;
let sourceLock = process.env.ICHIRAN_SOURCE_LOCK;
for (let index = 2; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument === '--require-analyzer') requireAnalyzer = true;
  else if (argument === '--require-rust') requireRust = true;
  else if (argument === '--typescript-oracle') typescriptOracle = true;
  else if (argument === '--qualification') qualificationBuild = true;
  else if (argument === '--release') {
    const value = process.argv[++index];
    if (!value) throw new Error('--release requires a directory');
    releaseDirectory = value;
  } else if (argument === '--source-lock') {
    sourceLock = process.argv[++index];
    if (!sourceLock) throw new Error('--source-lock requires a file');
  } else {
    throw new Error(`Unknown build-audit argument: ${argument}`);
  }
}

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const output = join(packageRoot, 'dist');
const assetDirectory = join(output, 'assets');
const outputNames = await readdir(output);
if (outputNames.includes('sw.js')) {
  throw new Error('Browser adapter/demo must not ship a Service Worker');
}
const scripts = (await readdir(assetDirectory)).filter(name => name.endsWith('.js'));
if (scripts.some(name => name.startsWith('qualification-bridge-'))) {
  throw new Error('Production browser build contains the qualification bridge');
}
const workerName = scripts.find(name => name.startsWith('analyzer.worker-'));
if (!workerName) throw new Error('Production analyzer Worker chunk is missing');
const qualificationClients = scripts.filter(name => name.startsWith('qualification-client-'));
const lazyBenchmarkCorpora = scripts.filter(name => name.startsWith('benchmark-corpus-'));
if (qualificationBuild && (qualificationClients.length !== 1 || lazyBenchmarkCorpora.length !== 0)) {
  throw new Error('Qualification build must contain one offline-complete client chunk');
}
if (!qualificationBuild && (qualificationClients.length !== 0 || lazyBenchmarkCorpora.length !== 0)) {
  throw new Error('Product browser build contains qualification-only chunks');
}

const worker = await readFile(join(assetDirectory, workerName), 'utf8');
const oracleWorkerNames = findTypeScriptOracleWorkerChunks(worker, scripts);
if (typescriptOracle && oracleWorkerNames.length !== 1) {
  throw new Error('Explicit TypeScript-oracle build must contain one Worker-only runtime chunk');
}
if (!typescriptOracle && oracleWorkerNames.length !== 0) {
  throw new Error('Production Rust build contains a TypeScript-oracle runtime chunk');
}
const oracleWorkerNameSet = new Set(oracleWorkerNames);
const mainNames = scripts.filter(
  name => name !== workerName && !oracleWorkerNameSet.has(name)
);
const main = (await Promise.all(
  mainNames.map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n');
const workerGraph = `${worker}\n${(await Promise.all(
  oracleWorkerNames.map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n')}`;

if (!qualificationBuild) {
  const product = `${main}\n${worker}`;
  if (product.includes('rust-kernel-metrics') || product.includes('__ichiranQualification')) {
    throw new Error('Product browser build contains qualification-only runtime code');
  }
}

// The qualification client embeds its inert corpus so it is complete before
// the offline benchmark. Its provenance may name reference-postgres tests
// without introducing that code into the product runtime.
const runtimeNames = scripts.filter(
  name => name !== workerName && !name.startsWith('qualification-client-')
);
const runtime = `${(await Promise.all(
  runtimeNames.map(name => readFile(join(assetDirectory, name), 'utf8'))
)).join('\n')}\n${worker}`;

if (requireRust && typescriptOracle) {
  throw new Error('Rust browser audit cannot run in explicit TypeScript-oracle mode');
}
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
} else if (!workerGraph.includes('ICHIPACK') || !worker.includes('ichiran-browser-alpha')) {
  throw new Error('Analyzer pack/runtime is not linked into the Worker');
}
assertAnalyzerWorkerOnly(main);
if (!typescriptOracle) assertRustRuntimeGraph(runtime);
for (const forbidden of [
  'postgres', 'node:fs', 'node:path', 'node:async_hooks', 'async_hooks', 'kernel-not-ready',
]) {
  if (runtime.includes(forbidden)) {
    throw new Error(`Browser bundle contains forbidden runtime text ${forbidden}`);
  }
}

const stagedDirectory = join(output, 'analyzer');
let staged = null;
try {
  staged = await verifyAnalyzerRelease(
    stagedDirectory,
    repositoryRoot,
    {
      qualifiedArtifact: process.env.ICHIRAN_QUALIFIED_ARTIFACT,
      sourceLock
    }
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
    {
      qualifiedArtifact: process.env.ICHIRAN_QUALIFIED_ARTIFACT,
      sourceLock
    }
  );
  assertSameRelease(staged, source);
}

console.log(
  `Browser build audit passed: ${workerName}; analyzer code is Worker-only; `
  + (staged ? `release ${staged.manifest.packVersion} is current and verified` : 'no analyzer release staged')
);
