import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { openNodeRuntime } from '@ichiran/node';

import { runCli } from '../src/index.js';
import { firstCanonicalDifference } from '../../core/tools/parity-canonical.js';

const TEST_DIRECTORY = dirname(fileURLToPath(import.meta.url));
const REPOSITORY = join(TEST_DIRECTORY, '..', '..', '..');
const SOURCES_LOCK_PATH = join(REPOSITORY, 'browser-alpha', 'sources.lock.json');
const CANONICAL_IDENTITY_POLICY = 'terminal-root-v1';

export interface TestCases {
  readonly romanization: readonly string[];
  readonly info: readonly string[];
  readonly fullJson: readonly { readonly text: string; readonly limit: number }[];
}

export interface ExpectedOutputs {
  readonly romanization: Readonly<Record<string, string>>;
  readonly info: Readonly<Record<string, string>>;
  readonly fullJson: Readonly<Record<string, string>>;
}

export interface ParityTestData {
  readonly testCases: TestCases;
  readonly expectedOutputs: ExpectedOutputs;
}

export interface CanonicalParityOutputs {
  readonly formatVersion: number;
  readonly identityPolicy: string;
  readonly source: {
    readonly path: string;
    readonly sha256: string;
  };
  readonly oracle: {
    readonly sourcesLockSha256: string;
    readonly upstreamIchiranCommit: string;
    readonly dataReleaseTag: string;
    readonly postgresReferenceCommit: string;
    readonly databaseDumpSha256: string;
    readonly databaseSchemaSha256: string;
  };
  readonly stats: {
    readonly requests: number;
    readonly rewrittenSeqFields: number;
    readonly multipleRootIdentityKeys: number;
    readonly outputsSha256: string;
  };
  readonly fullJson: Readonly<Record<string, string>>;
}

export interface TextParityReport {
  readonly total: number;
  readonly exact: number;
  readonly failures: readonly string[];
}

export interface JsonParityReport {
  readonly total: number;
  /** Parsed JSON is identical with every array in its original order. */
  readonly rawExact: number;
  /** Difference is solely the contract's equal-score path/alternative ordering. */
  readonly canonicalOnly: number;
  readonly mismatched: number;
  readonly failures: readonly string[];
  readonly canonicalOnlyKeys: readonly string[];
}

type PackedRuntime = Awaited<ReturnType<typeof openNodeRuntime>>;

let runtimePromise: Promise<PackedRuntime> | null = null;

/** One verified pack and one immutable runtime are shared by the entire gate. */
export function openPackedParityRuntime(): Promise<PackedRuntime> {
  runtimePromise ??= openNodeRuntime();
  return runtimePromise;
}

export function loadParityTestData(
  testCasesFile: string,
  expectedOutputsFile: string,
  errorMessage: string
): ParityTestData {
  try {
    const testCases = JSON.parse(
      readFileSync(join(TEST_DIRECTORY, 'data', testCasesFile), 'utf8')
    ) as TestCases;
    const expectedOutputs = JSON.parse(
      readFileSync(join(TEST_DIRECTORY, 'data', expectedOutputsFile), 'utf8')
    ) as ExpectedOutputs;
    return { testCases, expectedOutputs };
  } catch (error) {
    throw new Error(errorMessage, { cause: error });
  }
}

function sha256(value: string | Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

function exactKeys(left: object, right: object): boolean {
  return JSON.stringify(Object.keys(left)) === JSON.stringify(Object.keys(right));
}

/**
 * Load the compiler-generated identity-normalized expectation and prove that it
 * was derived from the raw Lisp capture and the currently locked 260118 oracle.
 */
export function loadCanonicalParityOutputs(
  canonicalFile: string,
  rawFile: string,
  expectedRequests: number
): CanonicalParityOutputs {
  const rawPath = join(TEST_DIRECTORY, 'data', rawFile);
  const canonicalPath = join(TEST_DIRECTORY, 'data', canonicalFile);
  const rawBytes = readFileSync(rawPath);
  const lockBytes = readFileSync(SOURCES_LOCK_PATH);
  const raw = JSON.parse(rawBytes.toString('utf8')) as ExpectedOutputs;
  const lock = JSON.parse(lockBytes.toString('utf8')) as {
    readonly upstreamIchiran: { readonly commit: string; readonly dataReleaseTag: string };
    readonly postgresReference: { readonly repositoryCommit: string };
    readonly databaseDump: { readonly sha256: string };
    readonly database: { readonly schemaSha256: string };
  };
  const canonical = JSON.parse(
    readFileSync(canonicalPath, 'utf8')
  ) as CanonicalParityOutputs;
  const expectedSourcePath = `packages/cli/tests/data/${rawFile}`;
  const checks: readonly [label: string, actual: unknown, expected: unknown][] = [
    ['format version', canonical.formatVersion, 1],
    ['identity policy', canonical.identityPolicy, CANONICAL_IDENTITY_POLICY],
    ['raw source path', canonical.source.path, expectedSourcePath],
    ['raw source SHA-256', canonical.source.sha256, sha256(rawBytes)],
    ['sources lock SHA-256', canonical.oracle.sourcesLockSha256, sha256(lockBytes)],
    ['upstream Ichiran commit', canonical.oracle.upstreamIchiranCommit, lock.upstreamIchiran.commit],
    ['data release tag', canonical.oracle.dataReleaseTag, lock.upstreamIchiran.dataReleaseTag],
    [
      'PostgreSQL reference commit',
      canonical.oracle.postgresReferenceCommit,
      lock.postgresReference.repositoryCommit
    ],
    ['database dump SHA-256', canonical.oracle.databaseDumpSha256, lock.databaseDump.sha256],
    ['database schema SHA-256', canonical.oracle.databaseSchemaSha256, lock.database.schemaSha256],
    ['request count', canonical.stats.requests, expectedRequests],
    ['output count', Object.keys(canonical.fullJson).length, expectedRequests],
    ['output SHA-256', canonical.stats.outputsSha256, sha256(JSON.stringify(canonical.fullJson))],
    ['raw/canonical request keys', exactKeys(raw.fullJson, canonical.fullJson), true]
  ];
  for (const [label, actual, expected] of checks) {
    if (actual !== expected) {
      throw new Error(
        `${canonicalFile} ${label} ${JSON.stringify(actual)}; expected ${JSON.stringify(expected)}`
      );
    }
  }
  if (!Number.isSafeInteger(canonical.stats.rewrittenSeqFields)
    || canonical.stats.rewrittenSeqFields <= 0) {
    throw new Error(`${canonicalFile} has no generated sequence identities to normalize`);
  }
  if (!Number.isSafeInteger(canonical.stats.multipleRootIdentityKeys)
    || canonical.stats.multipleRootIdentityKeys < 0) {
    throw new Error(`${canonicalFile} has invalid multiple-root identity statistics`);
  }
  return canonical;
}

/** CLI defaults to normalizePunctuation=true, matching the captured Lisp CLI. */
export async function runTsCli(
  text: string,
  options: { readonly withInfo?: boolean; readonly full?: boolean; readonly limit?: number } = {}
): Promise<string> {
  return runCli(text, { ...options, runtime: await openPackedParityRuntime() });
}

function objectKeysOnly(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(objectKeysOnly);
  if (typeof value !== 'object' || value === null) return value;
  const source = value as Record<string, unknown>;
  return Object.fromEntries(
    Object.keys(source).sort().map(key => [key, objectKeysOnly(source[key])])
  );
}

function rawJsonEqual(expected: unknown, actual: unknown): boolean {
  return JSON.stringify(objectKeysOnly(expected)) === JSON.stringify(objectKeysOnly(actual));
}

function preview(value: unknown): string {
  const rendered = JSON.stringify(value);
  if (rendered === undefined) return String(value);
  return rendered.length <= 160 ? rendered : `${rendered.slice(0, 157)}...`;
}

export async function runTextParity(
  inputs: readonly string[],
  expected: Readonly<Record<string, string>>,
  options: { readonly withInfo?: boolean } = {}
): Promise<TextParityReport> {
  let exact = 0;
  const failures: string[] = [];
  for (const input of inputs) {
    const expectedOutput = expected[input];
    if (expectedOutput === undefined) {
      failures.push(`${JSON.stringify(input)}: fixture output is missing`);
      continue;
    }
    try {
      const actual = await runTsCli(input, options);
      if (actual === expectedOutput) exact++;
      else failures.push(
        `${JSON.stringify(input)}: expected ${preview(expectedOutput)}, actual ${preview(actual)}`
      );
    } catch (error) {
      failures.push(
        `${JSON.stringify(input)}: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }
  return { total: inputs.length, exact, failures };
}

export async function runFullJsonParity(
  cases: readonly { readonly text: string; readonly limit: number }[],
  expected: Readonly<Record<string, string>>
): Promise<JsonParityReport> {
  let rawExact = 0;
  let canonicalOnly = 0;
  const failures: string[] = [];
  const canonicalOnlyKeys: string[] = [];

  for (const testCase of cases) {
    const key = `${testCase.text}|${testCase.limit}`;
    const expectedOutput = expected[key];
    if (expectedOutput === undefined) {
      failures.push(`${JSON.stringify(key)}: fixture output is missing`);
      continue;
    }
    try {
      const actualOutput = await runTsCli(testCase.text, {
        full: true,
        limit: testCase.limit
      });
      const expectedJson = JSON.parse(expectedOutput) as unknown;
      const actualJson = JSON.parse(actualOutput) as unknown;
      if (rawJsonEqual(expectedJson, actualJson)) {
        rawExact++;
        continue;
      }

      // This is the only tolerated normalization: reordering within contiguous
      // equal-score path/alternative runs, using the analyzer's semantic tie key.
      const difference = firstCanonicalDifference(expectedJson, actualJson);
      if (difference === null) {
        canonicalOnly++;
        canonicalOnlyKeys.push(key);
        continue;
      }
      failures.push(
        `${JSON.stringify(key)}: ${difference.kind} at ${difference.path}; `
        + `expected ${preview(difference.expected)}, actual ${preview(difference.actual)}`
      );
    } catch (error) {
      failures.push(`${JSON.stringify(key)}: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  return {
    total: cases.length,
    rawExact,
    canonicalOnly,
    mismatched: failures.length,
    failures,
    canonicalOnlyKeys
  };
}

function cappedFailures(failures: readonly string[]): string {
  const shown = failures.slice(0, 12);
  const omitted = failures.length - shown.length;
  return shown.map(value => `  - ${value}`).join('\n')
    + (omitted > 0 ? `\n  - ... ${omitted} more mismatch(es) omitted` : '');
}

export function assertTextParity(label: string, report: TextParityReport): void {
  const summary = `${label}: ${report.exact}/${report.total} exact; `
    + `${report.failures.length} mismatch(es)`;
  console.info(summary);
  if (report.failures.length > 0) {
    throw new Error(`${summary}\n${cappedFailures(report.failures)}`);
  }
}

export function assertJsonParity(label: string, report: JsonParityReport): void {
  const summary = `${label}: ${report.rawExact}/${report.total} raw exact; `
    + `${report.canonicalOnly} canonical-only; ${report.mismatched} mismatch(es)`;
  console.info(summary);
  if (report.canonicalOnlyKeys.length > 0) {
    console.info(
      `${label} canonical-only keys: ${report.canonicalOnlyKeys.slice(0, 12).map(key => JSON.stringify(key)).join(', ')}`
      + (report.canonicalOnlyKeys.length > 12
        ? `, ... ${report.canonicalOnlyKeys.length - 12} more`
        : '')
    );
  }
  if (report.mismatched > 0) {
    throw new Error(`${summary}\n${cappedFailures(report.failures)}`);
  }
}
