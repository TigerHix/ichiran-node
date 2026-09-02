import { describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve } from 'node:path';

import { buildAnalyzerRelease } from '../src/browser-pack/release-manifest.js';
import { parseBrowserAlphaSourceLock } from '../src/browser-pack/release-orchestration.js';
import {
  parityObservationSha256,
  parityReportNormalizedSha256,
  parseSourceCompilerParityAttestation,
  validateSourceCompilerParityReport,
  verifySourceCompilerParityAttestation
} from '../src/source-compiler/parity-attestation.js';

const repository = resolve(import.meta.dir, '../../..');
const data = resolve(repository, 'data');

function sha256(value: Uint8Array | string): string {
  return createHash('sha256').update(value).digest('hex');
}

function clone<T>(value: T): T {
  return structuredClone(value);
}

const syntheticTestedRelease = {
  sourceCommit: '4'.repeat(40),
  manifestFileSha256: '5'.repeat(64),
  manifestSha256: '6'.repeat(64),
  hot: {
    file: 'hot.bin.gz', encoding: 'gzip',
    downloadBytes: 1, downloadSha256: '7'.repeat(64),
    installedBytes: 1, installedSha256: '8'.repeat(64)
  },
  details: {
    file: 'details.bin.gz', encoding: 'gzip',
    downloadBytes: 1, downloadSha256: '9'.repeat(64),
    installedBytes: 1, installedSha256: 'a'.repeat(64)
  }
} as const;

interface TestedReleaseFixture {
  sourceCommit: string;
  manifestFileSha256: string;
  manifestSha256: string;
  hot: {
    file: string;
    encoding: 'gzip' | 'identity';
    downloadBytes: number;
    downloadSha256: string;
    installedBytes: number;
    installedSha256: string;
  };
  details: TestedReleaseFixture['hot'];
}

function diagnosticFixture(sourceLockSha256: string, oracle: {
  readonly sha256: string;
  readonly database: string;
  readonly upstreamIchiranCommit: string;
}, testedRelease: TestedReleaseFixture = syntheticTestedRelease) {
  const chosen = {
    suite: 'cli', request: 'chosen|1', classification: 'analyzer',
    qualifiedOutputSha256: 'b'.repeat(64),
    sourceOutputSha256: 'c'.repeat(64),
    pathDifference: { path: '$[0]', kind: 'value', expected: '"a"', actual: '"b"' }
  };
  const fallback = {
    suite: 'probes', request: 'fallback|5', classification: 'analyzer',
    qualifiedOutputSha256: 'd'.repeat(64),
    sourceOutputSha256: 'e'.repeat(64),
    cleanDifference: { path: '$[1]', kind: 'value', expected: '"c"', actual: '"d"' },
    pathDifference: { path: '$[2]', kind: 'value', expected: '"e"', actual: '"f"' }
  };
  const report = {
    formatVersion: 4,
    generatedAt: '2026-01-01T00:00:00.000Z',
    completeCorpus: true,
    testedRelease,
    authoritativeOracleTotals: {
      allComparisons: {
        operations: 1, exact: 0, divergent: 1,
        analyzerDivergent: 1, presentationDivergent: 0, errors: 0
      },
      frozenFallbackCleanSemantic: {
        operations: 1, exact: 0, divergent: 1,
        analyzerDivergent: 1, presentationDivergent: 0, errors: 0
      }
    },
    gate: { currentOracleAllowlist: [] },
    releaseInputLock: { kind: 'source-compiler', sha256: sourceLockSha256 },
    frozenOracleLock: oracle,
    samples: [chosen, fallback]
  };
  const totals = {
    operations: 1, exact: 0, analyzerDeltas: 1, presentationDeltas: 0, errors: 0
  };
  const row = (
    id: string,
    authority: 'current-lisp' | 'postgresql-fallback',
    sample: typeof chosen | typeof fallback
  ) => {
    const difference = 'cleanDifference' in sample
      ? sample.cleanDifference
      : sample.pathDifference;
    return {
      id,
      authority,
      suite: sample.suite,
      request: sample.request,
      classification: 'analyzer',
      observationSha256: parityObservationSha256(sample),
      field: difference.path,
      qualified: difference.expected,
      source: difference.actual,
      cause: 'Reviewed deterministic-order observation.'
    };
  };
  return {
    report,
    attestation: {
      formatVersion: 1,
      scope: 'Synthetic complete-corpus qualification fixture',
      report: {
        historicalReportSha256: '0'.repeat(64),
        normalizedSha256: parityReportNormalizedSha256(report),
        formatVersion: 4,
        chosenAuthority: totals,
        postgresqlFallback: totals
      },
      pack: {},
      oracle: {
        lockSha256: oracle.sha256,
        database: oracle.database,
        upstreamIchiranCommit: oracle.upstreamIchiranCommit
      },
      policy: { runtimeAllowlist: [], decision: 'Qualification evidence only.' },
      rows: [
        row('chosen', 'current-lisp', chosen),
        row('fallback', 'postgresql-fallback', fallback)
      ]
    }
  };
}

describe('source-compiler complete-corpus parity attestation', () => {
  test('tracks exactly the 16 chosen-authority and five fallback reviews', async () => {
    const [attestationText, reportText] = await Promise.all([
      readFile(resolve(data, 'source-compiler-parity-attestation.json'), 'utf8'),
      readFile(resolve(data, 'source-compiler-parity-report.json'), 'utf8')
    ]);
    const value = JSON.parse(attestationText);
    const attestation = parseSourceCompilerParityAttestation(value);
    expect(attestation.rows).toHaveLength(21);
    expect(attestation.rows.filter(row => row.authority === 'current-lisp')).toHaveLength(16);
    expect(attestation.rows.filter(row => row.authority === 'postgresql-fallback')).toHaveLength(5);
    expect(attestation.policy.runtimeAllowlist).toEqual([]);
    expect(sha256(reportText)).toBe(attestation.report.historicalReportSha256);
    expect(validateSourceCompilerParityReport(
      attestation,
      JSON.parse(reportText)
    ).reviewedRows).toBe(21);
  });

  test('requires an exact one-to-one review of every retained observation', () => {
    const fixture = diagnosticFixture('1'.repeat(64), {
      sha256: '2'.repeat(64), database: 'oracle', upstreamIchiranCommit: '3'.repeat(40)
    });
    const attestationValue = {
      ...fixture.attestation,
      pack: {
        historicalSourceCommit: '4'.repeat(40),
        historicalManifestFileSha256: '5'.repeat(64),
        historicalManifestSha256: '6'.repeat(64),
        sourceLockSha256: '1'.repeat(64),
        hot: {
          encoding: 'gzip',
          download: { bytes: 1, sha256: '7'.repeat(64) },
          installed: { bytes: 1, sha256: '8'.repeat(64) }
        },
        details: {
          encoding: 'gzip',
          download: { bytes: 1, sha256: '9'.repeat(64) },
          installed: { bytes: 1, sha256: 'a'.repeat(64) }
        }
      }
    };
    const attestation = parseSourceCompilerParityAttestation(attestationValue);
    expect(validateSourceCompilerParityReport(attestation, fixture.report).reviewedRows).toBe(2);

    const missingReview = clone(attestationValue);
    missingReview.rows.pop();
    expect(() => validateSourceCompilerParityReport(
      parseSourceCompilerParityAttestation(missingReview),
      fixture.report
    )).toThrow('Unreviewed parity diagnostic row');

    const extraReview = clone(attestationValue);
    extraReview.rows.push({
      ...extraReview.rows[0]!, id: 'extra', request: 'not-observed|1'
    });
    expect(() => validateSourceCompilerParityReport(
      parseSourceCompilerParityAttestation(extraReview),
      fixture.report
    )).toThrow('Extra parity review row');

    const duplicateReview = clone(attestationValue);
    duplicateReview.rows.push({ ...duplicateReview.rows[0]!, id: 'duplicate-key' });
    expect(() => validateSourceCompilerParityReport(
      parseSourceCompilerParityAttestation(duplicateReview),
      fixture.report
    )).toThrow('Duplicate parity review row');

    const duplicateSample = clone(fixture.report);
    duplicateSample.samples[1] = clone(duplicateSample.samples[0]!);
    expect(() => validateSourceCompilerParityReport(attestation, duplicateSample))
      .toThrow('Duplicate parity diagnostic row');

    const staleSample = clone(fixture.report);
    staleSample.samples[0]!.pathDifference.actual = '"changed"';
    expect(() => validateSourceCompilerParityReport(attestation, staleSample))
      .toThrow('Parity review row chosen has stale reviewed evidence');

    const staleReview = clone(attestationValue);
    staleReview.rows[0]!.field = '$.not-the-observed-field';
    expect(() => validateSourceCompilerParityReport(
      parseSourceCompilerParityAttestation(staleReview),
      fixture.report
    )).toThrow('has stale reviewed evidence');

    const staleCounts = clone(fixture.report);
    staleCounts.authoritativeOracleTotals.allComparisons.exact = 1;
    expect(() => validateSourceCompilerParityReport(attestation, staleCounts))
      .toThrow('Chosen-authority report counts do not close');

    const extraSample = clone(fixture.report);
    extraSample.samples.push({
      ...clone(extraSample.samples[0]!), request: 'unreviewed-extra|1'
    });
    expect(() => validateSourceCompilerParityReport(attestation, extraSample))
      .toThrow('Unreviewed parity diagnostic row');
  });

  test('ignores only generatedAt and rejects changed samples, locks, and pack bytes', async () => {
    const directory = await mkdtemp(resolve(tmpdir(), 'ichiran-parity-attestation-'));
    try {
      const sourceLockBytes = await readFile(resolve(data, 'source-compiler-sources.lock.json'));
      const oracleLockBytes = await readFile(resolve(repository, 'browser-alpha/sources.lock.json'));
      const parsedOracle = parseBrowserAlphaSourceLock(oracleLockBytes.toString('utf8'));
      const sourceLockSha256 = sha256(sourceLockBytes);
      const oracle = {
        sha256: sha256(oracleLockBytes),
        database: parsedOracle.database.name,
        upstreamIchiranCommit: parsedOracle.upstreamIchiran.commit
      };
      const release = buildAnalyzerRelease({
        packVersion: 'test',
        sourceCommit: 'b'.repeat(40),
        sourcesLockSha256: sourceLockSha256,
        hot: new Uint8Array([1, 2, 3]),
        details: new Uint8Array([4, 5, 6])
      });
      const manifestFileSha256 = sha256(release.manifestBytes);
      const testedAsset = (side: 'hot' | 'details') => {
        const manifest = release.manifest[side];
        return {
          file: manifest.file,
          encoding: manifest.encoding,
          downloadBytes: manifest.downloadBytes,
          downloadSha256: manifest.downloadSha256,
          installedBytes: manifest.installedBytes,
          installedSha256: manifest.installedSha256
        };
      };
      const fixture = diagnosticFixture(sourceLockSha256, oracle, {
        sourceCommit: release.manifest.sourceCommit,
        manifestFileSha256,
        manifestSha256: release.manifest.manifestSha256,
        hot: testedAsset('hot'),
        details: testedAsset('details')
      });
      const asset = (side: 'hot' | 'details') => {
        const manifest = release.manifest[side];
        return {
          encoding: manifest.encoding,
          download: {
            bytes: manifest.downloadBytes,
            sha256: manifest.downloadSha256
          },
          installed: {
            bytes: manifest.installedBytes,
            sha256: manifest.installedSha256
          }
        };
      };
      const attestationValue = {
        ...fixture.attestation,
        pack: {
          historicalSourceCommit: release.manifest.sourceCommit,
          historicalManifestFileSha256: manifestFileSha256,
          historicalManifestSha256: release.manifest.manifestSha256,
          sourceLockSha256,
          hot: asset('hot'),
          details: asset('details')
        }
      };
      const paths = {
        attestation: resolve(directory, 'attestation.json'),
        report: resolve(directory, 'report.json'),
        sourceLock: resolve(directory, 'source-lock.json'),
        oracleLock: resolve(directory, 'oracle-lock.json')
      };
      await Promise.all([
        writeFile(paths.attestation, JSON.stringify(attestationValue)),
        writeFile(paths.report, JSON.stringify(fixture.report)),
        writeFile(paths.sourceLock, sourceLockBytes),
        writeFile(paths.oracleLock, oracleLockBytes),
        writeFile(resolve(directory, 'manifest.json'), release.manifestBytes),
        writeFile(resolve(directory, release.manifest.hot.file), release.hotDownload),
        writeFile(resolve(directory, release.manifest.details.file), release.detailsDownload)
      ]);
      const input = {
        attestationPath: paths.attestation,
        reportPath: paths.report,
        releaseDirectory: directory,
        sourceLockPath: paths.sourceLock,
        oracleLockPath: paths.oracleLock
      };
      expect((await verifySourceCompilerParityAttestation(input)).reviewedRows).toBe(2);

      const timestampOnly = clone(fixture.report);
      timestampOnly.generatedAt = '2099-12-31T23:59:59.999Z';
      expect(parityReportNormalizedSha256(timestampOnly))
        .toBe(parityReportNormalizedSha256(fixture.report));
      await writeFile(paths.report, JSON.stringify(timestampOnly));
      expect((await verifySourceCompilerParityAttestation(input)).reviewedRows).toBe(2);

      const changedSample = clone(timestampOnly);
      changedSample.samples[0]!.pathDifference.actual = '"changed"';
      await writeFile(paths.report, JSON.stringify(changedSample));
      await expect(verifySourceCompilerParityAttestation(input)).rejects
        .toThrow('has stale reviewed evidence');
      await writeFile(paths.report, JSON.stringify(timestampOnly));

      const changedCompleteOutput = clone(timestampOnly);
      changedCompleteOutput.samples[0]!.sourceOutputSha256 = 'f'.repeat(64);
      await writeFile(paths.report, JSON.stringify(changedCompleteOutput));
      await expect(verifySourceCompilerParityAttestation(input)).rejects.toThrow('is stale');
      await writeFile(paths.report, JSON.stringify(timestampOnly));

      const changedTestedRelease = clone(timestampOnly);
      changedTestedRelease.testedRelease.sourceCommit = 'f'.repeat(40);
      await writeFile(paths.report, JSON.stringify(changedTestedRelease));
      await expect(verifySourceCompilerParityAttestation(input)).rejects
        .toThrow('tested-release identity is stale');
      await writeFile(paths.report, JSON.stringify(timestampOnly));

      await writeFile(paths.sourceLock, '{}');
      await expect(verifySourceCompilerParityAttestation(input)).rejects
        .toThrow('Parity source-lock identity is stale');
      await writeFile(paths.sourceLock, sourceLockBytes);

      const changedOracle = JSON.parse(oracleLockBytes.toString('utf8'));
      changedOracle.database.name = 'changed_oracle';
      await writeFile(paths.oracleLock, JSON.stringify(changedOracle));
      await expect(verifySourceCompilerParityAttestation(input)).rejects
        .toThrow('Parity oracle-lock identity is stale');
      await writeFile(paths.oracleLock, oracleLockBytes);

      const laterManifest = buildAnalyzerRelease({
        packVersion: 'test',
        sourceCommit: 'c'.repeat(40),
        sourcesLockSha256: sourceLockSha256,
        hot: new Uint8Array([1, 2, 3]),
        details: new Uint8Array([4, 5, 6])
      });
      await Promise.all([
        writeFile(resolve(directory, 'manifest.json'), laterManifest.manifestBytes),
        writeFile(resolve(directory, laterManifest.manifest.hot.file), laterManifest.hotDownload),
        writeFile(
          resolve(directory, laterManifest.manifest.details.file),
          laterManifest.detailsDownload
        )
      ]);
      expect((await verifySourceCompilerParityAttestation(input)).reviewedRows).toBe(2);

      const changedRelease = buildAnalyzerRelease({
        packVersion: 'test',
        sourceCommit: 'd'.repeat(40),
        sourcesLockSha256: sourceLockSha256,
        hot: new Uint8Array([1, 2, 4]),
        details: new Uint8Array([4, 5, 6])
      });
      await Promise.all([
        writeFile(resolve(directory, 'manifest.json'), changedRelease.manifestBytes),
        writeFile(resolve(directory, changedRelease.manifest.hot.file), changedRelease.hotDownload),
        writeFile(
          resolve(directory, changedRelease.manifest.details.file),
          changedRelease.detailsDownload
        )
      ]);
      await expect(verifySourceCompilerParityAttestation(input)).rejects
        .toThrow('Parity hot-pack identity is stale');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
