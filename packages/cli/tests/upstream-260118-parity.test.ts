import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, test } from 'bun:test';
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

const RUN_PACKED_PARITY = process.env.RUN_PARITY_TESTS === 'true'
  && Boolean(process.env.ICHIRAN_PACK_DIR);
const TEST_DIRECTORY = dirname(fileURLToPath(import.meta.url));
const ORACLE_PATH = join(TEST_DIRECTORY, '..', '..', '..', 'browser-alpha', 'upstream-oracle.json');
const oracle = JSON.parse(readFileSync(ORACLE_PATH, 'utf8')) as UpstreamOracle;

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
  const summary = `upstream 260118 product regressions: ${exact}/${total} checks exact; `
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
  });

  test('matches every behavior represented by the product result', async () => {
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
    } catch (error) {
      failures.push(error instanceof Error ? error.message : String(error));
    } finally {
      analyzer.dispose();
    }
    assertNoFailures(failures, exact, checks);
  }, 120_000);
});
