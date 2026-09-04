#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import { memoryDetailSource, openDetailStore } from '../../core/src/details.js';
import { TypeScriptOracleRuntime } from '../../core/src/runtime-typescript.js';
import {
  fixtureKey,
  loadAnalyzerParityCorpus,
  type AnalyzerEntityFixture,
  type AnalyzerFixtureRequest
} from '../../core/tools/parity-corpus.js';
import { firstCanonicalDifference } from '../../core/tools/parity-canonical.js';
import { assertSamePackRelease } from './same_pack_release.js';
import { ANALYZER_WASM_URL, Analyzer } from '../../core/src/index.js';

interface Request {
  readonly text: string;
  readonly limit: number;
  readonly entities?: readonly AnalyzerEntityFixture['entities'][number][];
  readonly normalizePunctuation: boolean;
}

interface DetailedRequest {
  readonly name: string;
  readonly request: Request;
}

interface AuthorityDetailedCase extends DetailedRequest {
  readonly expected: unknown;
}

interface FallbackCase {
  readonly request: AnalyzerFixtureRequest;
  readonly entities?: AnalyzerEntityFixture['entities'];
  readonly detailed: unknown;
}

interface TokenDetailsOracle {
  readonly formatVersion: 1;
  readonly sourcesLockSha256: string;
  readonly cases: readonly {
    readonly name: string;
    readonly text: string;
    readonly options: {
      readonly limit: number;
      readonly entities?: readonly AnalyzerEntityFixture['entities'][number][];
      readonly normalizePunctuation?: boolean;
    };
    readonly pathIndex: number;
    readonly tokenIndex: number;
    readonly expected: unknown;
  }[];
}

const HOT_SHA256 = '61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0';
const DETAILS_SHA256 = '0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151';
const DESCRIBE_ENTRIES = [0, 33_240, 43_720, 48_688] as const;
const CANONICAL_TIE_NAMES = ['cli:169', 'cli:214', 'hard:10', 'probes:26'] as const;
const TOKEN_DETAILS_INPUTS = ['猫', '食べました', '読んでいました', '三個'] as const;
const SAME_PACK_TOKEN_DETAILS_CASES = 8;

function digest(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function request(
  value: AnalyzerFixtureRequest,
  entities?: AnalyzerEntityFixture['entities']
): Request {
  return {
    text: value.text,
    limit: value.limit,
    ...(entities ? { entities } : {}),
    normalizePunctuation: value.normalizePunctuation ?? true
  };
}

function sameRequest(left: Request, right: Request): boolean {
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

function utf16Hex(value: string): string {
  return Array.from({ length: value.length }, (_, index) =>
    value.charCodeAt(index).toString(16).padStart(4, '0')
  ).join(',');
}

function canonicalObjectOrder(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(canonicalObjectOrder);
  if (typeof value !== 'object' || value === null) return value;
  const source = value as Record<string, unknown>;
  const output: Record<string, unknown> = {};
  for (const key of Object.keys(source).sort()) output[key] = canonicalObjectOrder(source[key]);
  return output;
}

async function main(): Promise<void> {
  const repository = resolve(import.meta.dir, '../../..');
  const samePack = process.argv[2] === '--same-pack';
  const release = resolve(process.argv[samePack ? 3 : 2] ?? join(repository, 'browser-alpha/release'));
  const sourceLock = samePack && process.argv[4] === '--source-lock'
    ? process.argv[5]
    : undefined;
  if (samePack && process.argv.length > 4 && !sourceLock) {
    throw new Error('Expected --source-lock <file> after the same-pack release');
  }
  const [corpus, hot, details] = await Promise.all([
    loadAnalyzerParityCorpus(repository),
    readFile(join(release, 'hot.bin')),
    readFile(join(release, 'details.bin'))
  ]);
  const hotSha256 = digest(hot);
  const detailsSha256 = digest(details);
  const samePackManifest = samePack
    ? await assertSamePackRelease(repository, release, hot, details, sourceLock)
    : null;
  const tokenDetailsOracle = samePack
    ? JSON.parse(await readFile(join(
        repository,
        'packages/rust-kernel/tests/fixtures/token-details-oracle.json'
      ), 'utf8')) as TokenDetailsOracle
    : null;
  if (
    tokenDetailsOracle
    && (
      tokenDetailsOracle.formatVersion !== 1
      || tokenDetailsOracle.cases.length !== SAME_PACK_TOKEN_DETAILS_CASES
      || tokenDetailsOracle.sourcesLockSha256 !== samePackManifest?.sourcesLockSha256
    )
  ) {
    throw new Error('TokenDetails oracle does not match the qualified source release');
  }
  if (!samePack && (hotSha256 !== HOT_SHA256 || detailsSha256 !== DETAILS_SHA256)) {
    throw new Error('C product corpus requires the immutable qualified pack');
  }

  const fallback = samePack ? null : JSON.parse(await readFile(join(
    repository,
    'packages/rust-kernel/tests/fixtures/m3-fallback.json'
  ), 'utf8')) as {
    readonly formatVersion: number;
    readonly counts: { readonly total: number };
    readonly suites: Record<'counters' | 'entities' | 'probes', FallbackCase[]>;
  };
  if (fallback && (fallback.formatVersion !== 1 || fallback.counts.total !== 301)) {
    throw new Error('fallback fixture identity is invalid');
  }

  const detailed: (DetailedRequest | AuthorityDetailedCase)[] = [];
  const packed = {
    counters: corpus.counters.map(value => request(value)),
    entities: corpus.entities.map(value => request({ text: value.text, limit: 1 }, value.entities)),
    probes: corpus.probes.map(value => request(value.request))
  };
  if (samePack) {
    for (const [suite, values] of [
      ['cli', corpus.cli.map(value => request(value))],
      ['hard', corpus.hard.map(value => request(value))],
      ['counters', packed.counters],
      ['entities', packed.entities],
      ['probes', packed.probes]
    ] as const) {
      values.forEach((value, index) => detailed.push({ name: `${suite}:${index}`, request: value }));
    }
  } else {
    for (const [suite, values, outputs] of [
      ['cli', corpus.cli, corpus.currentLispCli],
      ['hard', corpus.hard, corpus.currentLispHard]
    ] as const) {
      values.forEach((value, index) => {
        const serialized = outputs[fixtureKey(value)];
        if (serialized === undefined) throw new Error(`${suite}[${index}] lacks detailed authority`);
        detailed.push({
          name: `${suite}:${index}`,
          request: request(value),
          expected: JSON.parse(serialized)
        });
      });
    }
    for (const suite of ['counters', 'entities', 'probes'] as const) {
      fallback!.suites[suite].forEach((value, index) => {
        const actual = packed[suite][index];
        const expected = request(value.request, value.entities);
        if (!actual || !sameRequest(actual, expected)) {
          throw new Error(`${suite}[${index}] request disagrees with its fallback fixture`);
        }
        detailed.push({ name: `${suite}:${index}`, request: actual, expected: value.detailed });
      });
    }
  }
  if (detailed.length !== 702) throw new Error(`expected 702 detailed cases, got ${detailed.length}`);
  const utf16Requests = [
    { name: 'utf16:astral', request: request({ text: '😀', limit: 1 }) },
    {
      name: 'utf16:lone-high',
      request: request({ text: String.fromCharCode(0xd83d), limit: 1 })
    },
    {
      name: 'utf16:lone-low',
      request: request({ text: String.fromCharCode(0xde00), limit: 1 })
    }
  ] as const;
  detailed.push(...utf16Requests);

  const decodeGzip = async (compressed: Uint8Array, expectedBytes: number): Promise<Uint8Array> => {
    const decoded = new Uint8Array(gunzipSync(compressed));
    if (decoded.byteLength !== expectedBytes) {
      throw new Error(`gzip output has ${decoded.byteLength} bytes; expected ${expectedBytes}`);
    }
    return decoded;
  };
  const oracle = await TypeScriptOracleRuntime.open({
    hot: Uint8Array.from(hot),
    details: memoryDetailSource(Uint8Array.from(details)),
    decodeGzip
  });
  const portable: unknown[] = [];
  let canonicalTies = 0;
  const canonicalTieNames: string[] = [];
  for (let index = 0; index < detailed.length; index++) {
    const value = detailed[index]!;
    const actual = await oracle.legacy(value.request.text, {
      limit: value.request.limit,
      entities: value.request.entities,
      normalizePunctuation: value.request.normalizePunctuation
    });
    if ('expected' in value) {
      const difference = firstCanonicalDifference(value.expected, actual);
      if (difference) {
        throw new Error(`${value.name} portable oracle differs at ${difference.path}`);
      }
      if (
        JSON.stringify(canonicalObjectOrder(value.expected))
        !== JSON.stringify(canonicalObjectOrder(actual))
      ) {
        canonicalTies++;
        canonicalTieNames.push(value.name);
      }
    }
    portable.push(actual);
  }
  if (!samePack && JSON.stringify(canonicalTieNames) !== JSON.stringify(CANONICAL_TIE_NAMES)) {
    throw new Error(
      `portable detailed oracle tie identities ${canonicalTieNames.join(', ')}; expected ${CANONICAL_TIE_NAMES.join(', ')}`
    );
  }

  const metadata = {
    format: 'ichiran-c-product-v1',
    mode: samePack ? 'same-pack' : 'immutable-baseline',
    detailed: samePack
      ? { operations: 705, samePack: 702, utf16: 3, canonicalTies: 0 }
      : {
          operations: 705,
          currentLisp: 401,
          fallback: 301,
          utf16: 3,
          canonicalTies: {
            currentLisp: 3,
            fallback: 1,
            total: canonicalTies,
            names: canonicalTieNames
          }
        },
    romanization: { operations: 8, retained: 5, utf16: 3 },
    describe: DESCRIBE_ENTRIES.length,
    tokenDetails: tokenDetailsOracle?.cases.length ?? TOKEN_DETAILS_INPUTS.length,
    hotSha256,
    detailsSha256,
    pack: samePackManifest ? {
      packVersion: samePackManifest.packVersion,
      sourceCommit: samePackManifest.sourceCommit,
      sourcesLockSha256: samePackManifest.sourcesLockSha256
    } : { tag: 'portable-core-260118-baseline' }
  };
  process.stdout.write(`#${JSON.stringify(metadata)}\n`);
  const analyzer = await Analyzer.open({
    hot: Uint8Array.from(hot),
    details: {
      byteLength: details.byteLength,
      read: async (offset, byteLength) => Uint8Array.from(
        details.subarray(offset, offset + byteLength)
      )
    },
    wasm: new Uint8Array(await readFile(ANALYZER_WASM_URL))
  });
  try {
    const cases = tokenDetailsOracle?.cases ?? TOKEN_DETAILS_INPUTS.map((text, index) => ({
      name: `baseline-${index}`,
      text,
      options: { limit: 3, entities: [], normalizePunctuation: true },
      pathIndex: 0,
      tokenIndex: 0,
      expected: null
    }));
    for (const [index, value] of cases.entries()) {
      const options = {
        limit: value.options.limit,
        entities: value.options.entities ?? [],
        normalizePunctuation: value.options.normalizePunctuation ?? false
      };
      const actual = await analyzer.details(value.text, {
        ...options,
        pathIndex: value.pathIndex,
        tokenIndex: value.tokenIndex
      });
      const expected = value.expected ?? actual;
      if (value.expected !== null && JSON.stringify(actual) !== JSON.stringify(value.expected)) {
        throw new Error(`TokenDetails oracle mismatch for ${value.name}`);
      }
      process.stdout.write(
        `T\ttoken-details:${index}\t${utf16Hex(value.text)}\t${JSON.stringify(options)}`
        + `\t${value.pathIndex}\t${value.tokenIndex}\t${JSON.stringify(expected)}\n`
      );
    }
  } finally {
    analyzer.dispose();
  }
  for (let index = 0; index < detailed.length; index++) {
    const value = detailed[index]!;
    const options = JSON.stringify({
      limit: value.request.limit,
      entities: value.request.entities ?? [],
      normalizePunctuation: value.request.normalizePunctuation
    });
    process.stdout.write(
      `L\t${value.name}\t${utf16Hex(value.request.text)}\t${options}`
      + `\t${JSON.stringify(portable[index])}\n`
    );
  }
  for (const [index, input] of corpus.romanization.entries()) {
    const expected = samePack
      ? await oracle.romanize(input, { limit: 1, normalizePunctuation: true })
      : corpus.currentLispRomanization[input];
    if (expected === undefined) throw new Error(`romanization[${index}] lacks authority`);
    const options = JSON.stringify({ limit: 1, entities: [], normalizePunctuation: true });
    process.stdout.write(
      `R\tromanization:${index}\t${utf16Hex(input)}\t${options}\t\t${JSON.stringify(expected)}\n`
    );
  }
  for (const [index, value] of ([
    { input: '😀', method: 'kunrei-siki' },
    { input: String.fromCharCode(0xd83d), method: 'hepburn-basic' },
    { input: String.fromCharCode(0xde00), method: 'hepburn-modified' }
  ] as const).entries()) {
    const options = { limit: 1, normalizePunctuation: true } as const;
    const expected = await oracle.romanize(value.input, { ...options, method: value.method });
    process.stdout.write(
      `R\tutf16:${index}\t${utf16Hex(value.input)}\t${JSON.stringify({
        ...options,
        entities: []
      })}\t${value.method}\t${JSON.stringify(expected)}\n`
    );
  }

  const store = await openDetailStore(memoryDetailSource(Uint8Array.from(details)), decodeGzip);
  for (const entryIndex of DESCRIBE_ENTRIES) {
    process.stdout.write(
      `D\tdescribe:${entryIndex}\t${entryIndex}`
      + `\t${JSON.stringify(await store.entry(entryIndex))}\n`
    );
  }
}

await main();
