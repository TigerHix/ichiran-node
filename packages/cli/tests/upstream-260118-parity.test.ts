import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, test } from 'bun:test';

import { openPackedParityRuntime } from './cli-parity-helpers.js';

interface SegmentationProbe {
  readonly input: string;
  readonly segments: readonly string[];
  readonly score: number;
}

interface CrashProbe extends SegmentationProbe {
  readonly seq: number;
  readonly conjugation: string | null;
}

interface GataiProbe extends SegmentationProbe {
  readonly compound: readonly string[];
  readonly suffixSeq: number;
  readonly suffixDescription: string;
}

interface UpstreamOracle {
  readonly scope: string;
  readonly grammarIncluded: boolean;
  readonly ichiran: {
    readonly commit: string;
    readonly dataReleaseTag: string;
  };
  readonly qualification: {
    readonly topOneRegressions: readonly SegmentationProbe[];
    readonly jsonCrashRegressions: readonly CrashProbe[];
    readonly gataiProbe: GataiProbe;
  };
}

interface JmdictUpdateBehavior {
  readonly formatVersion: 1;
  readonly sourceLockSha256: string;
  readonly changes: readonly [{
    readonly input: string;
    readonly selectedEntrySeq: number;
    readonly conjugationSourceSeq: number;
    readonly addedSourceForm: string;
    readonly baselineConjugation: null;
    readonly updatedConjugation: string;
    readonly reason: string;
  }];
}

type LegacyWord = Record<string, unknown>;

const RUN_PACKED_PARITY = process.env.RUN_PARITY_TESTS === 'true'
  && Boolean(process.env.ICHIRAN_PACK_DIR);
const TEST_DIRECTORY = dirname(fileURLToPath(import.meta.url));
const ORACLE_PATH = join(TEST_DIRECTORY, '..', '..', '..', 'browser-alpha', 'upstream-oracle.json');
const oracle = JSON.parse(readFileSync(ORACLE_PATH, 'utf8')) as UpstreamOracle;
const updateBehavior = JSON.parse(readFileSync(join(
  TEST_DIRECTORY,
  '..', '..', '..',
  'data/source-compiler-update-2026-09-02-behavior.json'
), 'utf8')) as JmdictUpdateBehavior;
const activeSourceLock = RUN_PACKED_PARITY
  ? (JSON.parse(readFileSync(join(process.env.ICHIRAN_PACK_DIR!, 'manifest.json'), 'utf8')) as {
      readonly sourcesLockSha256: string;
    }).sourcesLockSha256
  : null;
const activeUpdate = activeSourceLock === updateBehavior.sourceLockSha256;

function topLegacyWords(value: unknown): LegacyWord[] {
  if (!Array.isArray(value)) return [];
  const words: LegacyWord[] = [];
  for (const chunk of value) {
    if (!Array.isArray(chunk) || !Array.isArray(chunk[0])) continue;
    const path = chunk[0];
    if (!Array.isArray(path[0])) continue;
    for (const token of path[0]) {
      if (
        Array.isArray(token)
        && typeof token[1] === 'object'
        && token[1] !== null
        && !Array.isArray(token[1])
      ) {
        words.push(token[1] as LegacyWord);
      }
    }
  }
  return words;
}

function conjugationDescription(value: unknown): string | null {
  if (!Array.isArray(value) || value.length === 0) return null;
  const first = value[0];
  if (typeof first !== 'object' || first === null || Array.isArray(first)) return null;
  const conjugation = first as Record<string, unknown>;
  const own = Array.isArray(conjugation.prop)
    ? conjugation.prop.flatMap(property => {
        if (typeof property !== 'object' || property === null || Array.isArray(property)) return [];
        const type = (property as Record<string, unknown>).type;
        return typeof type === 'string' ? [type] : [];
      }).join(' + ')
    : '';
  const via = conjugationDescription(conjugation.via);
  if (!own) return via;
  return via ? `${own} via ${via}` : own;
}

function same(left: unknown, right: unknown): boolean {
  return JSON.stringify(left) === JSON.stringify(right);
}

function check(
  failures: string[],
  input: string,
  field: string,
  expected: unknown,
  actual: unknown
): boolean {
  if (!same(expected, actual)) {
    failures.push(
      `${JSON.stringify(input)} ${field}: expected ${JSON.stringify(expected)}, actual ${JSON.stringify(actual)}`
    );
    return false;
  }
  return true;
}

function assertNoFailures(failures: readonly string[], exact: number, total: number): void {
  const summary = `upstream 260118 regressions: ${exact}/${total} checks exact; `
    + `${failures.length} mismatch(es)`;
  console.info(summary);
  if (failures.length === 0) return;
  const shown = failures.slice(0, 16);
  const omitted = failures.length - shown.length;
  throw new Error(
    `${summary}\n${shown.map(value => `  - ${value}`).join('\n')}`
    + (omitted > 0 ? `\n  - ... ${omitted} more mismatch(es) omitted` : '')
  );
}

describe.skipIf(!RUN_PACKED_PARITY)('packed runtime upstream 260118 regressions', () => {
  test('uses the pinned analyzer-only oracle', () => {
    if (
      oracle.scope !== 'analyzer-only'
      || oracle.grammarIncluded
      || oracle.ichiran.commit !== 'ea9583368e67cad22d94abae8dbcc8df96d99bcd'
      || oracle.ichiran.dataReleaseTag !== 'ichiran-260118'
      || oracle.qualification.topOneRegressions.length !== 7
      || oracle.qualification.jsonCrashRegressions.length !== 2
    ) {
      throw new Error('browser-alpha/upstream-oracle.json is not the pinned analyzer-only 260118 oracle');
    }
    const change = updateBehavior.changes[0];
    if (
      updateBehavior.formatVersion !== 1
      || updateBehavior.changes.length !== 1
      || change.input !== '一本とられた'
      || change.selectedEntrySeq !== 2268020
      || change.conjugationSourceSeq !== 1859020
      || change.addedSourceForm !== '一本とる'
      || change.baselineConjugation !== null
      || change.updatedConjugation !== 'Past (~ta) via Passive'
      || change.reason.length === 0
    ) {
      throw new Error('JMdict 2026-09-02 behavior attestation is invalid');
    }
  });

  test('matches top-one, JSON crash, and gatai probes', async () => {
    const runtime = await openPackedParityRuntime();
    const failures: string[] = [];
    let checks = 0;
    let exact = 0;

    for (const probe of oracle.qualification.topOneRegressions) {
      try {
        const result = await runtime.analyze(probe.input, { limit: 1 });
        const path = result.paths[0];
        exact += Number(check(
          failures,
          probe.input,
          'segments',
          probe.segments,
          path?.tokens.map(token => token.text)
        ));
        exact += Number(check(failures, probe.input, 'score', probe.score, path?.score));
      } catch (error) {
        failures.push(`${JSON.stringify(probe.input)} threw: ${error instanceof Error ? error.message : String(error)}`);
      }
      checks += 2;
    }

    for (const probe of oracle.qualification.jsonCrashRegressions) {
      try {
        const result = await runtime.analyze(probe.input, { limit: 1 });
        const path = result.paths[0];
        const words = topLegacyWords(await runtime.legacy(probe.input, { limit: 1 }));
        exact += Number(check(
          failures,
          probe.input,
          'segments',
          probe.segments,
          path?.tokens.map(token => token.text)
        ));
        exact += Number(check(failures, probe.input, 'score', probe.score, path?.score));
        exact += Number(check(failures, probe.input, 'seq', probe.seq, words[0]?.seq));
        exact += Number(check(
          failures,
          probe.input,
          'conjugation',
          activeUpdate && probe.input === updateBehavior.changes[0].input
            ? updateBehavior.changes[0].updatedConjugation
            : probe.conjugation,
          conjugationDescription(words[0]?.conj)
        ));
      } catch (error) {
        failures.push(`${JSON.stringify(probe.input)} threw: ${error instanceof Error ? error.message : String(error)}`);
      }
      checks += 4;
    }

    const probe = oracle.qualification.gataiProbe;
    try {
      const result = await runtime.analyze(probe.input, { limit: 1 });
      const path = result.paths[0];
      const word = topLegacyWords(await runtime.legacy(probe.input, { limit: 1 }))[0];
      const components = Array.isArray(word?.components) ? word.components : [];
      const suffix = components[1] as LegacyWord | undefined;
      exact += Number(check(
        failures,
        probe.input,
        'segments',
        probe.segments,
        path?.tokens.map(token => token.text)
      ));
      exact += Number(check(failures, probe.input, 'score', probe.score, path?.score));
      exact += Number(check(failures, probe.input, 'compound', probe.compound, word?.compound));
      exact += Number(check(failures, probe.input, 'suffixSeq', probe.suffixSeq, suffix?.seq));
      exact += Number(check(
        failures,
        probe.input,
        'suffixDescription',
        probe.suffixDescription,
        suffix?.suffix
      ));
    } catch (error) {
      failures.push(`${JSON.stringify(probe.input)} threw: ${error instanceof Error ? error.message : String(error)}`);
    }
    checks += 5;

    assertNoFailures(failures, exact, checks);
  }, 120_000);
});
