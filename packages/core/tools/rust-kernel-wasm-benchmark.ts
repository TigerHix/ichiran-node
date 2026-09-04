#!/usr/bin/env bun

import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';

import { Analyzer, type AnalyzeOptions } from '../src/index.js';

interface BenchmarkRequest {
  readonly text: string;
  readonly limit: number;
  readonly entities?: AnalyzeOptions['entities'];
}

interface BenchmarkCorpus {
  readonly groups: Readonly<Record<string, readonly BenchmarkRequest[]>>;
}

interface ReleaseManifest {
  readonly formatVersion: 2;
  readonly lexicon: { readonly installedSha256: string };
  readonly locales: Readonly<Record<string, { readonly file: string }>>;
}

function fileSource(path: string) {
  const file = Bun.file(path);
  return {
    byteLength: file.size,
    read: async (offset: number, byteLength: number) => new Uint8Array(
      await file.slice(offset, offset + byteLength).arrayBuffer()
    )
  };
}

function percentile(values: readonly number[], value: number): number {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.ceil(sorted.length * value) - 1]!;
}

const repository = resolve(import.meta.dir, '../../..');
const release = resolve(process.argv[2] ?? join(repository, 'browser-alpha/release'));
const wasmPath = resolve(
  process.argv[3]
    ?? join(repository, 'packages/core/src/rust-kernel/generated/ichiran_kernel_bg.wasm')
);
const requestedGroups = process.argv.slice(4);
const corpus = JSON.parse(
  await readFile(join(repository, 'browser-alpha/bench/corpus.json'), 'utf8')
) as BenchmarkCorpus;
const manifest = JSON.parse(
  await readFile(join(release, 'manifest.json'), 'utf8')
) as ReleaseManifest;
if (manifest.formatVersion !== 2 || !manifest.locales.en) {
  throw new Error('Benchmark release must be a multilingual format-v2 pack with English');
}
const locales = Object.fromEntries(Object.entries(manifest.locales).map(([locale, asset]) => [
  locale,
  fileSource(join(release, asset.file.replace(/\.gz$/, '')))
]));
const runtime = await Analyzer.open({
  hot: new Uint8Array(await Bun.file(join(release, 'hot.bin')).arrayBuffer()),
  wasm: new Uint8Array(await Bun.file(wasmPath).arrayBuffer()),
  lexicon: {
    source: fileSource(join(release, manifest.lexicon.file.replace(/\.gz$/, ''))),
    sha256: manifest.lexicon.installedSha256
  },
  locales
});

const names = requestedGroups.length > 0
  ? requestedGroups
  : ['ordinary', 'pathological-morphology', 'dense-contiguous-boundary', 'paragraph-scaling'];
const report: Record<string, unknown> = {};
try {
  for (const name of names) {
    const requests = corpus.groups[name];
    if (!requests) throw new Error(`Unknown benchmark group ${name}`);
    for (const request of requests) {
      await runtime.analyze(request.text, {
        limit: name === 'ordinary' || name === 'pathological-morphology' ? 1 : request.limit,
        entities: request.entities
      });
    }
    const samples: number[] = [];
    for (const request of requests) {
      const started = performance.now();
      await runtime.analyze(request.text, {
        limit: name === 'ordinary' || name === 'pathological-morphology' ? 1 : request.limit,
        entities: request.entities
      });
      samples.push(performance.now() - started);
    }
    report[name] = {
      samples: samples.length,
      p50Ms: percentile(samples, 0.5),
      p95Ms: percentile(samples, 0.95),
      maxMs: Math.max(...samples),
      totalMs: samples.reduce((sum, sample) => sum + sample, 0)
    };
  }
  console.log(JSON.stringify({ wasmPath, groups: report }, null, 2));
} finally {
  runtime.dispose();
}
