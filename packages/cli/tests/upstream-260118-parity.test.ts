import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, test } from 'bun:test';
import { legacyAnalysis } from '@ichiran/core/qualification/runtime';
import { openAnalyzer } from '@ichiran/node';

interface SegmentationProbe {
  readonly input: string;
  readonly segments: readonly string[];
  readonly score: number;
}

interface CrashProbe extends SegmentationProbe {
  readonly seq: number;
}

interface GataiProbe extends SegmentationProbe {
  readonly compound: readonly string[];
  readonly suffixSeq: number;
}

interface UpstreamOracle {
  readonly scope: string;
  readonly grammarIncluded: boolean;
  readonly ichiran: { readonly commit: string; readonly dataReleaseTag: string };
  readonly qualification: {
    readonly topOneRegressions: readonly SegmentationProbe[];
    readonly jsonCrashRegressions: readonly CrashProbe[];
    readonly gataiProbe: GataiProbe;
  };
}

interface ProductInflection {
  readonly pos: string;
  readonly type: number;
  readonly negative: boolean | null;
  readonly formal: boolean | null;
  readonly ordinal: number;
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
    readonly productProbe: {
      readonly input: string;
      readonly segments: readonly string[];
      readonly score: number;
      readonly selectedEntrySeq: number;
      readonly inflection: readonly ProductInflection[];
    };
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
): number {
  if (same(expected, actual)) return 1;
  failures.push(
    `${JSON.stringify(input)} ${field}: expected ${JSON.stringify(expected)}, actual ${JSON.stringify(actual)}`
  );
  return 0;
}

function assertNoFailures(failures: readonly string[], exact: number, total: number): void {
  const summary = `upstream and source-update regressions: ${exact}/${total} checks exact; `
    + `${failures.length} mismatch(es)`;
  console.info(summary);
  if (failures.length === 0) return;
  throw new Error(`${summary}\n${failures.slice(0, 16).map(value => `  - ${value}`).join('\n')}`);
}

describe.skipIf(!RUN_PACKED_PARITY)('packed analyzer upstream 260118 regressions', () => {
  test('uses the pinned analyzer-only oracle', () => {
    if (
      oracle.scope !== 'analyzer-only'
      || oracle.grammarIncluded
      || oracle.ichiran.commit !== 'ea9583368e67cad22d94abae8dbcc8df96d99bcd'
      || oracle.ichiran.dataReleaseTag !== 'ichiran-260118'
      || oracle.qualification.topOneRegressions.length !== 7
      || oracle.qualification.jsonCrashRegressions.length !== 2
    ) {
      throw new Error('browser-alpha/upstream-oracle.json is not the pinned analyzer-only oracle');
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
      || change.productProbe.input !== '一本とられる'
      || !same(change.productProbe.segments, ['一本とられる'])
      || change.productProbe.score !== 616
      || change.productProbe.selectedEntrySeq !== 1859020
      || !same(change.productProbe.inflection, [{
        pos: 'v5r',
        type: 6,
        negative: false,
        formal: false,
        ordinal: 1
      }])
      || change.reason.length === 0
    ) {
      throw new Error('JMdict 2026-09-02 behavior attestation is invalid');
    }
  });

  test('matches product regressions and the source-locked qualification witness', async () => {
    const analyzer = await openAnalyzer();
    const failures: string[] = [];
    let checks = 0;
    let exact = 0;
    try {
      for (const probe of oracle.qualification.topOneRegressions) {
        const path = (await analyzer.analyze(probe.input, { limit: 1 })).paths[0];
        exact += check(failures, probe.input, 'segments', probe.segments,
          path?.tokens.map(token => token.text));
        exact += check(failures, probe.input, 'score', probe.score, path?.score);
        checks += 2;
      }

      for (const probe of oracle.qualification.jsonCrashRegressions) {
        const path = (await analyzer.analyze(probe.input, { limit: 1 })).paths[0];
        exact += check(failures, probe.input, 'segments', probe.segments,
          path?.tokens.map(token => token.text));
        exact += check(failures, probe.input, 'score', probe.score, path?.score);
        exact += check(failures, probe.input, 'seq', probe.seq, path?.tokens[0]?.root?.seq);
        checks += 3;
      }

      const probe = oracle.qualification.gataiProbe;
      const path = (await analyzer.analyze(probe.input, { limit: 1 })).paths[0];
      const components = path?.tokens[0]?.components;
      exact += check(failures, probe.input, 'segments', probe.segments,
        path?.tokens.map(token => token.text));
      exact += check(failures, probe.input, 'score', probe.score, path?.score);
      exact += check(failures, probe.input, 'compound', probe.compound,
        components?.map(component => component.text));
      exact += check(failures, probe.input, 'suffixSeq', probe.suffixSeq,
        components?.[1]?.root?.seq);
      checks += 4;

      if (activeUpdate) {
        const change = updateBehavior.changes[0];
        const legacyWords = topLegacyWords(await legacyAnalysis(
          analyzer,
          change.input,
          { limit: 1 }
        ));
        exact += check(failures, change.input, 'qualification conjugation',
          change.updatedConjugation, conjugationDescription(legacyWords[0]?.conj));
        checks += 1;

        const updateProbe = change.productProbe;
        const updatePath = (await analyzer.analyze(updateProbe.input, { limit: 1 })).paths[0];
        exact += check(failures, updateProbe.input, 'segments', updateProbe.segments,
          updatePath?.tokens.map(token => token.text));
        exact += check(failures, updateProbe.input, 'score', updateProbe.score, updatePath?.score);
        exact += check(failures, updateProbe.input, 'seq', updateProbe.selectedEntrySeq,
          updatePath?.tokens[0]?.root?.seq);
        exact += check(failures, updateProbe.input, 'inflection', updateProbe.inflection,
          updatePath?.tokens[0]?.inflection);
        checks += 4;
      }
    } catch (error) {
      failures.push(error instanceof Error ? error.message : String(error));
    } finally {
      analyzer.dispose();
    }
    assertNoFailures(failures, exact, checks);
  }, 120_000);
});
