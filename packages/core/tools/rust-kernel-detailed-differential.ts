#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';

import { compareDetailedAuthority } from './oracle-authority.js';
import {
  fixtureKey,
  loadAnalyzerParityCorpus,
  type AnalyzerEntityFixture,
  type AnalyzerFixtureRequest,
  type AnalyzerParityCorpus
} from './parity-corpus.js';

type SuiteName = 'cli' | 'hard' | 'counters' | 'entities' | 'probes';
type Authority = 'current-lisp' | 'frozen-postgres-reference';

interface BatchEntity {
  readonly start: number;
  readonly end: number;
  readonly boost?: number;
}

interface BatchRequest {
  readonly text: string;
  readonly limit: number;
  readonly entities?: readonly BatchEntity[];
  readonly normalizePunctuation: boolean;
}

interface DetailedCase {
  readonly suite: SuiteName;
  readonly index: number;
  readonly request: BatchRequest;
  readonly authority: Authority;
  readonly expected: unknown;
}

interface FallbackCase {
  readonly request: AnalyzerFixtureRequest;
  readonly entities?: readonly BatchEntity[];
  readonly detailed: unknown;
}

interface SuiteStats {
  operations: number;
  exact: number;
  pathExact: number;
  analyzer: number;
  presentation: number;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function batchRequest(
  request: AnalyzerFixtureRequest,
  entities?: readonly BatchEntity[]
): BatchRequest {
  return {
    text: request.text,
    limit: request.limit,
    ...(entities ? { entities } : {}),
    normalizePunctuation: request.normalizePunctuation ?? true
  };
}

function rawEqual(left: BatchRequest, right: BatchRequest): boolean {
  const leftEntities = left.entities ?? [];
  const rightEntities = right.entities ?? [];
  return left.text === right.text
    && left.limit === right.limit
    && left.normalizePunctuation === right.normalizePunctuation
    && leftEntities.length === rightEntities.length
    && leftEntities.every((entity, index) => {
      const other = rightEntities[index]!;
      return entity.start === other.start
        && entity.end === other.end
        && entity.boost === other.boost;
    });
}

function fallbackSuites(value: unknown): Record<'counters' | 'entities' | 'probes', FallbackCase[]> {
  const fixture = value as {
    readonly formatVersion?: number;
    readonly identityPolicy?: string;
    readonly counts?: Record<string, number>;
    readonly suites?: Record<string, FallbackCase[]>;
  };
  if (
    fixture.formatVersion !== 1
    || fixture.identityPolicy !== 'terminal-root-v1'
    || fixture.counts?.counters !== 200
    || fixture.counts?.entities !== 54
    || fixture.counts?.probes !== 47
    || fixture.counts?.total !== 301
    || !fixture.suites
  ) throw new Error('fallback fixture identity or accounting is invalid');
  const result = {
    counters: fixture.suites.counters,
    entities: fixture.suites.entities,
    probes: fixture.suites.probes
  };
  for (const [suite, count] of [['counters', 200], ['entities', 54], ['probes', 47]] as const) {
    if (!Array.isArray(result[suite]) || result[suite].length !== count) {
      throw new Error(`fallback ${suite} has ${result[suite]?.length ?? 'invalid'} cases; expected ${count}`);
    }
  }
  return result;
}

function currentLispCases(corpus: AnalyzerParityCorpus): DetailedCase[] {
  const result: DetailedCase[] = [];
  const append = (
    suite: 'cli' | 'hard',
    requests: readonly AnalyzerFixtureRequest[],
    outputs: Readonly<Record<string, string>>
  ): void => {
    requests.forEach((request, index) => {
      const key = fixtureKey(request);
      const serialized = outputs[key];
      if (serialized === undefined) throw new Error(`${suite} current-Lisp output is missing ${key}`);
      result.push({
        suite,
        index,
        request: batchRequest(request),
        authority: 'current-lisp',
        expected: JSON.parse(serialized) as unknown
      });
    });
  };
  append('cli', corpus.cli, corpus.currentLispCli);
  append('hard', corpus.hard, corpus.currentLispHard);
  if (result.length !== 401) {
    throw new Error(`current-Lisp detailed corpus has ${result.length} requests; expected 401`);
  }
  return result;
}

function fallbackCases(
  corpus: AnalyzerParityCorpus,
  fallback: Record<'counters' | 'entities' | 'probes', FallbackCase[]>
): DetailedCase[] {
  const requests = {
    counters: corpus.counters.map(request => batchRequest(request)),
    entities: corpus.entities.map(value => batchRequest(
      { text: value.text, limit: 1 },
      value.entities
    )),
    probes: corpus.probes.map(value => batchRequest(value.request))
  };
  const result: DetailedCase[] = [];
  for (const suite of ['counters', 'entities', 'probes'] as const) {
    fallback[suite].forEach((fixture, index) => {
      const expectedRequest = batchRequest(fixture.request, fixture.entities);
      const packedRequest = requests[suite][index];
      if (!packedRequest || !rawEqual(expectedRequest, packedRequest)) {
        throw new Error(`fallback ${suite}[${index}] request disagrees with the packed corpus`);
      }
      result.push({
        suite,
        index,
        request: packedRequest,
        authority: 'frozen-postgres-reference',
        expected: fixture.detailed
      });
    });
  }
  if (result.length !== 301) {
    throw new Error(`frozen fallback detailed corpus has ${result.length} requests; expected 301`);
  }
  return result;
}

async function rustDetailedBatch(
  repository: string,
  release: string,
  lexiconSha256: string,
  requests: readonly BatchRequest[]
): Promise<unknown[]> {
  const child = spawn('cargo', [
    'run', '--quiet',
    '--manifest-path', join(repository, 'packages/rust-kernel/Cargo.toml'),
    '--bin', 'analyzer_detailed_batch', '--',
    join(release, 'hot.bin'),
    join(release, 'lexicon.bin'),
    join(release, 'gloss.en.bin'),
    'en',
    lexiconSha256
  ], {
    cwd: repository,
    stdio: ['pipe', 'pipe', 'pipe']
  });
  const stdout: Buffer[] = [];
  const stderr: Buffer[] = [];
  child.stdout.on('data', value => stdout.push(value as Buffer));
  child.stderr.on('data', value => stderr.push(value as Buffer));
  child.stdin.end(JSON.stringify(requests));
  const status = await new Promise<number | null>((resolveStatus, reject) => {
    child.once('error', reject);
    child.once('close', resolveStatus);
  });
  if (status !== 0) {
    throw new Error(
      `native detailed batch exited ${String(status)}: ${Buffer.concat(stderr).toString('utf8')}`
    );
  }
  const values = JSON.parse(Buffer.concat(stdout).toString('utf8')) as unknown;
  if (!Array.isArray(values) || values.length !== requests.length) {
    throw new Error(
      `native detailed batch returned ${Array.isArray(values) ? values.length : 'non-array'} results; expected ${requests.length}`
    );
  }
  return values;
}

function emptyStats(): SuiteStats {
  return { operations: 0, exact: 0, pathExact: 0, analyzer: 0, presentation: 0 };
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const release = resolve(process.argv[2] ?? join(repository, 'browser-alpha/release'));
  const fixturePath = join(repository, 'packages/rust-kernel/tests/fixtures/m3-fallback.json');
  const [corpus, fixtureBytes, manifestBytes, hotStat, lexiconStat, englishStat] = await Promise.all([
    loadAnalyzerParityCorpus(repository),
    readFile(fixturePath),
    readFile(join(release, 'manifest.json')),
    stat(join(release, 'hot.bin')),
    stat(join(release, 'lexicon.bin')),
    stat(join(release, 'gloss.en.bin'))
  ]);
  const manifest = JSON.parse(manifestBytes.toString('utf8')) as {
    readonly formatVersion?: number;
    readonly hot?: { readonly installedBytes?: number; readonly installedSha256?: string };
    readonly lexicon?: { readonly installedBytes?: number; readonly installedSha256?: string };
    readonly locales?: Readonly<Record<string, {
      readonly installedBytes?: number;
      readonly installedSha256?: string;
    }>>;
  };
  if (
    manifest.formatVersion !== 2
    || manifest.hot?.installedBytes !== hotStat.size
    || manifest.lexicon?.installedBytes !== lexiconStat.size
    || manifest.locales?.en?.installedBytes !== englishStat.size
    || !manifest.hot.installedSha256
    || !manifest.lexicon.installedSha256
    || !manifest.locales.en.installedSha256
  ) throw new Error('release manifest does not identify the installed analyzer artifacts');
  const fallback = fallbackSuites(JSON.parse(fixtureBytes.toString('utf8')) as unknown);
  const cases = [...currentLispCases(corpus), ...fallbackCases(corpus, fallback)];
  if (cases.length !== 702) {
    throw new Error(`detailed corpus has ${cases.length} requests; expected 702`);
  }
  const actual = await rustDetailedBatch(
    repository,
    release,
    manifest.lexicon.installedSha256,
    cases.map(value => value.request)
  );
  const suites: Record<SuiteName, SuiteStats> = {
    cli: emptyStats(),
    hard: emptyStats(),
    counters: emptyStats(),
    entities: emptyStats(),
    probes: emptyStats()
  };
  const authority = {
    'current-lisp': emptyStats(),
    'frozen-postgres-reference': emptyStats()
  } satisfies Record<Authority, SuiteStats>;
  const differences: unknown[] = [];

  cases.forEach((fixture, index) => {
    const comparison = compareDetailedAuthority(
      fixture.authority === 'current-lisp' ? fixture.expected : null,
      fixture.authority === 'frozen-postgres-reference' ? fixture.expected : null,
      actual[index]
    );
    if (comparison.source !== fixture.authority) {
      throw new Error(`${fixture.suite}[${fixture.index}] selected the wrong authority`);
    }
    for (const stats of [suites[fixture.suite], authority[fixture.authority]]) {
      stats.operations++;
      if (!comparison.pathDifference) stats.pathExact++;
      if (!comparison.detailedDifference) stats.exact++;
      else if (comparison.pathDifference) stats.analyzer++;
      else stats.presentation++;
    }
    if (comparison.detailedDifference) {
      differences.push({
        suite: fixture.suite,
        index: fixture.index,
        request: fixture.request,
        authority: fixture.authority,
        pathDifference: comparison.pathDifference,
        detailedDifference: comparison.detailedDifference
      });
    }
  });

  const exact = Object.values(suites).reduce((sum, stats) => sum + stats.exact, 0);
  const report = {
    formatVersion: 2,
    artifacts: {
      manifest: { path: join(release, 'manifest.json'), sha256: sha256(manifestBytes) },
      hot: {
        path: join(release, 'hot.bin'),
        bytes: hotStat.size,
        manifestSha256: manifest.hot.installedSha256
      },
      lexicon: {
        path: join(release, 'lexicon.bin'),
        bytes: lexiconStat.size,
        manifestSha256: manifest.lexicon.installedSha256
      },
      english: {
        path: join(release, 'gloss.en.bin'),
        bytes: englishStat.size,
        manifestSha256: manifest.locales.en.installedSha256
      },
      fallback: { path: fixturePath, sha256: sha256(fixtureBytes) }
    },
    policy: 'Current-Lisp snapshots are authoritative where present; provenance-bound frozen PostgreSQL detailed output is the fallback. Repository canonical equal-score ordering only.',
    operations: cases.length,
    exact,
    divergent: cases.length - exact,
    suites,
    authority,
    differences,
    allowlist: { entries: 0 }
  };
  console.log(JSON.stringify(report, null, 2));
  if (exact !== 702) process.exitCode = 1;
}

await main();
