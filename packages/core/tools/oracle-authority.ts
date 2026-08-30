import {
  firstCanonicalDifference,
  legacyPathSkeleton,
  type CanonicalDifference
} from './parity-canonical.js';

export type DetailedAuthoritySource = 'current-lisp' | 'frozen-postgres-reference';

export interface DetailedAuthorityComparison {
  readonly source: DetailedAuthoritySource;
  readonly value: unknown;
  readonly pathDifference: CanonicalDifference | null;
  readonly detailedDifference: CanonicalDifference | null;
}

/** Current upstream output wins whenever the corpus has a pinned Lisp snapshot. */
export function compareDetailedAuthority(
  currentLisp: unknown | null,
  frozenPostgresReference: unknown | null,
  actual: unknown
): DetailedAuthorityComparison {
  const source: DetailedAuthoritySource = currentLisp === null
    ? 'frozen-postgres-reference'
    : 'current-lisp';
  const value = currentLisp ?? frozenPostgresReference;
  if (value === null) throw new Error('Detailed comparison has no oracle value');
  return {
    source,
    value,
    pathDifference: firstCanonicalDifference(
      legacyPathSkeleton(value),
      legacyPathSkeleton(actual)
    ),
    detailedDifference: firstCanonicalDifference(value, actual)
  };
}

/** Apply the analyzer's public normalization boundary to every expected span. */
export function normalizeSegmentationExpectation(
  expected: readonly string[],
  normalize: (value: string) => string
): string[] {
  return expected.map(value => value === ':gap' ? value : normalize(value));
}

export interface ExactStats {
  readonly total: number;
  readonly exact: number;
}

/**
 * Snapshot-covered suites use current Lisp. Suites without snapshots require
 * both frozen-reference legacy and clean projections. Diagnostics are absent
 * from this input by construction and therefore cannot weaken or fail the gate.
 */
export function releaseGateFailureCount(input: {
  readonly currentLisp: readonly ExactStats[];
  readonly frozenFallback: readonly {
    readonly detailed: ExactStats;
    readonly clean: ExactStats;
  }[];
}): number {
  const failed = (stats: ExactStats): number => stats.total - stats.exact;
  return input.currentLisp.reduce((total, stats) => total + failed(stats), 0)
    + input.frozenFallback.reduce(
      (total, suite) => total + failed(suite.detailed) + failed(suite.clean),
      0
    );
}
