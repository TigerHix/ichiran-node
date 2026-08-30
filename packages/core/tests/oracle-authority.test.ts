import { describe, expect, test } from 'bun:test';

import {
  compareDetailedAuthority,
  normalizeSegmentationExpectation,
  releaseGateFailureCount
} from '../tools/oracle-authority.js';

describe('oracle release authority', () => {
  test('prefers pinned current Lisp and falls back only without a snapshot', () => {
    const currentLisp = { score: 345, alternatives: [1, 2] };
    const frozen = { score: 335, alternatives: [1, 2, 3] };

    const upstream = compareDetailedAuthority(currentLisp, frozen, currentLisp);
    expect(upstream.source).toBe('current-lisp');
    expect(upstream.pathDifference).toBeNull();
    expect(upstream.detailedDifference).toBeNull();

    const fallback = compareDetailedAuthority(null, frozen, frozen);
    expect(fallback.source).toBe('frozen-postgres-reference');
    expect(fallback.pathDifference).toBeNull();
    expect(fallback.detailedDifference).toBeNull();
  });

  test('does not normalize a generated identity emitted by the portable analyzer', () => {
    const comparison = compareDetailedAuthority(
      { text: '食べた', seq: 1358280 },
      { text: '食べた', seq: 1358280 },
      { text: '食べた', seq: 9_999_999 }
    );
    expect(comparison.source).toBe('current-lisp');
    expect(comparison.detailedDifference?.path).toBe('$.seq');
  });

  test('normalizes fixture spans without rewriting the gap sentinel', () => {
    const expected = ['新成人', '１４人', ':gap', '１年'];
    expect(normalizeSegmentationExpectation(expected, value =>
      value.replaceAll('１', '1').replaceAll('４', '4'))).toEqual([
      '新成人', '14人', ':gap', '1年'
    ]);
    expect(expected).toEqual(['新成人', '１４人', ':gap', '１年']);
  });

  test('counts only chosen authorities and both views of fallback suites', () => {
    expect(releaseGateFailureCount({
      currentLisp: [
        { total: 534, exact: 534 },
        { total: 252, exact: 251 }
      ],
      frozenFallback: [{
        detailed: { total: 200, exact: 200 },
        clean: { total: 200, exact: 199 }
      }]
    })).toBe(2);
  });
});
