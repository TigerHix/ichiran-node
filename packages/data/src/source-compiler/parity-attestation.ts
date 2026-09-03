import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

import { parseAnalyzerReleaseManifest } from '../browser-pack/release-manifest.js';
import { parseBrowserAlphaSourceLock } from '../browser-pack/release-orchestration.js';

export type ParityAuthority = 'current-lisp' | 'postgresql-fallback';
export type ParityClassification = 'analyzer' | 'presentation';

interface ParityTotals {
  readonly operations: number;
  readonly exact: number;
  readonly analyzerDeltas: number;
  readonly presentationDeltas: number;
  readonly errors: number;
}

interface ParityAssetIdentity {
  readonly bytes: number;
  readonly sha256: string;
}

interface ParityReleaseAsset {
  readonly encoding: 'gzip' | 'identity';
  readonly download: ParityAssetIdentity;
  readonly installed: ParityAssetIdentity;
}

interface TestedReleaseAsset {
  readonly file: string;
  readonly encoding: 'gzip' | 'identity';
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

interface TestedReleaseIdentity {
  readonly sourceCommit: string;
  readonly manifestFileSha256: string;
  readonly manifestSha256: string;
  readonly hot: TestedReleaseAsset;
  readonly details: TestedReleaseAsset;
}

export interface ParityReviewRow {
  readonly id: string;
  readonly authority: ParityAuthority;
  readonly suite: string;
  readonly request: string;
  readonly classification: ParityClassification;
  readonly observationSha256: string;
  readonly field: string;
  readonly qualified: string;
  readonly source: string;
  readonly cause: string;
}

export interface SourceCompilerParityAttestation {
  readonly formatVersion: 1;
  readonly scope: string;
  readonly report: {
    /** Raw identity of the historical retained report; provenance, not a live gate. */
    readonly historicalReportSha256: string;
    /** Canonical report identity excluding only its volatile generatedAt field. */
    readonly normalizedSha256: string;
    readonly formatVersion: 4;
    readonly chosenAuthority: ParityTotals;
    readonly postgresqlFallback: ParityTotals;
  };
  readonly pack: {
    readonly historicalSourceCommit: string;
    readonly historicalManifestFileSha256: string;
    readonly historicalManifestSha256: string;
    readonly sourceLockSha256: string;
    readonly hot: ParityReleaseAsset;
    readonly details: ParityReleaseAsset;
  };
  readonly oracle: {
    readonly lockSha256: string;
    readonly database: string;
    readonly upstreamIchiranCommit: string;
  };
  readonly policy: {
    readonly runtimeAllowlist: readonly [];
    readonly decision: string;
  };
  readonly rows: readonly ParityReviewRow[];
}

export interface VerifiedParityAttestation {
  readonly reportNormalizedSha256: string;
  readonly sourceLockSha256: string;
  readonly oracleLockSha256: string;
  readonly chosenAuthorityDeltas: number;
  readonly postgresqlFallbackDeltas: number;
  readonly reviewedRows: number;
}

function sha256(bytes: Uint8Array | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function exactKeys(
  value: Record<string, unknown>,
  expected: readonly string[],
  label: string
): void {
  const actual = Object.keys(value).sort();
  const wanted = [...expected].sort();
  if (actual.join('\n') !== wanted.join('\n')) {
    throw new Error(`${label} has unsupported fields: ${actual.join(', ')}`);
  }
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new Error(`${label} must be text`);
  }
  return value;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return Number(value);
}

function digest(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^[0-9a-f]{64}$/.test(result)) throw new Error(`${label} must be a lowercase SHA-256`);
  return result;
}

function commit(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^[0-9a-f]{40}$/.test(result)) throw new Error(`${label} must be a lowercase Git commit`);
  return result;
}

function totals(value: unknown, label: string): ParityTotals {
  const input = record(value, label);
  exactKeys(input, [
    'operations', 'exact', 'analyzerDeltas', 'presentationDeltas', 'errors'
  ], label);
  return {
    operations: integer(input.operations, `${label} operations`),
    exact: integer(input.exact, `${label} exact`),
    analyzerDeltas: integer(input.analyzerDeltas, `${label} analyzer deltas`),
    presentationDeltas: integer(input.presentationDeltas, `${label} presentation deltas`),
    errors: integer(input.errors, `${label} errors`)
  };
}

function asset(value: unknown, label: string): { readonly bytes: number; readonly sha256: string } {
  const input = record(value, label);
  exactKeys(input, ['bytes', 'sha256'], label);
  return {
    bytes: integer(input.bytes, `${label} bytes`),
    sha256: digest(input.sha256, `${label} digest`)
  };
}

function releaseAsset(value: unknown, label: string): ParityReleaseAsset {
  const input = record(value, label);
  exactKeys(input, ['encoding', 'download', 'installed'], label);
  if (input.encoding !== 'gzip' && input.encoding !== 'identity') {
    throw new Error(`${label} encoding must be gzip or identity`);
  }
  return {
    encoding: input.encoding,
    download: asset(input.download, `${label} download`),
    installed: asset(input.installed, `${label} installed`)
  };
}

function testedReleaseAsset(value: unknown, label: string): TestedReleaseAsset {
  const input = record(value, label);
  exactKeys(input, [
    'file', 'encoding', 'downloadBytes', 'downloadSha256',
    'installedBytes', 'installedSha256'
  ], label);
  if (input.encoding !== 'gzip' && input.encoding !== 'identity') {
    throw new Error(`${label} encoding must be gzip or identity`);
  }
  return {
    file: text(input.file, `${label} file`),
    encoding: input.encoding,
    downloadBytes: integer(input.downloadBytes, `${label} download bytes`),
    downloadSha256: digest(input.downloadSha256, `${label} download digest`),
    installedBytes: integer(input.installedBytes, `${label} installed bytes`),
    installedSha256: digest(input.installedSha256, `${label} installed digest`)
  };
}

function testedReleaseIdentity(value: unknown): TestedReleaseIdentity {
  const input = record(value, 'Parity report tested release');
  exactKeys(input, [
    'sourceCommit', 'manifestFileSha256', 'manifestSha256', 'hot', 'details'
  ], 'Parity report tested release');
  return {
    sourceCommit: commit(input.sourceCommit, 'Parity report tested source commit'),
    manifestFileSha256: digest(
      input.manifestFileSha256,
      'Parity report tested manifest-file digest'
    ),
    manifestSha256: digest(input.manifestSha256, 'Parity report tested manifest digest'),
    hot: testedReleaseAsset(input.hot, 'Parity report tested hot asset'),
    details: testedReleaseAsset(input.details, 'Parity report tested details asset')
  };
}

function reviewRow(value: unknown, index: number): ParityReviewRow {
  const label = `Parity review row ${index}`;
  const input = record(value, label);
  exactKeys(input, [
    'id', 'authority', 'suite', 'request', 'classification', 'observationSha256',
    'field', 'qualified', 'source', 'cause'
  ], label);
  const authority = input.authority;
  if (authority !== 'current-lisp' && authority !== 'postgresql-fallback') {
    throw new Error(`${label} has an invalid authority`);
  }
  const classification = input.classification;
  if (classification !== 'analyzer' && classification !== 'presentation') {
    throw new Error(`${label} has an invalid classification`);
  }
  return {
    id: text(input.id, `${label} id`),
    authority,
    suite: text(input.suite, `${label} suite`),
    request: text(input.request, `${label} request`),
    classification,
    observationSha256: digest(input.observationSha256, `${label} observation digest`),
    field: text(input.field, `${label} field`),
    qualified: text(input.qualified, `${label} qualified value`),
    source: text(input.source, `${label} source value`),
    cause: text(input.cause, `${label} cause`)
  };
}

/** Parse the compact, tracked review record with no ignored or optional fields. */
export function parseSourceCompilerParityAttestation(
  value: unknown
): SourceCompilerParityAttestation {
  const root = record(value, 'Source-compiler parity attestation');
  exactKeys(root, ['formatVersion', 'scope', 'report', 'pack', 'oracle', 'policy', 'rows'],
    'Source-compiler parity attestation');
  if (root.formatVersion !== 1) throw new Error('Unsupported source-compiler parity attestation');
  const report = record(root.report, 'Parity attestation report');
  exactKeys(report, [
    'historicalReportSha256', 'normalizedSha256', 'formatVersion',
    'chosenAuthority', 'postgresqlFallback'
  ],
    'Parity attestation report');
  if (report.formatVersion !== 4) throw new Error('Parity attestation requires report format 4');
  const pack = record(root.pack, 'Parity attestation pack');
  exactKeys(pack, [
    'historicalSourceCommit', 'historicalManifestFileSha256',
    'historicalManifestSha256', 'sourceLockSha256', 'hot', 'details'
  ], 'Parity attestation pack');
  const oracle = record(root.oracle, 'Parity attestation oracle');
  exactKeys(oracle, ['lockSha256', 'database', 'upstreamIchiranCommit'],
    'Parity attestation oracle');
  const policy = record(root.policy, 'Parity attestation policy');
  exactKeys(policy, ['runtimeAllowlist', 'decision'], 'Parity attestation policy');
  if (!Array.isArray(policy.runtimeAllowlist) || policy.runtimeAllowlist.length !== 0) {
    throw new Error('Parity attestation runtime allowlist must be empty');
  }
  if (!Array.isArray(root.rows)) throw new Error('Parity attestation rows must be an array');
  return {
    formatVersion: 1,
    scope: text(root.scope, 'Parity attestation scope'),
    report: {
      historicalReportSha256: digest(
        report.historicalReportSha256,
        'Historical parity diagnostic report digest'
      ),
      normalizedSha256: digest(
        report.normalizedSha256,
        'Normalized parity diagnostic report digest'
      ),
      formatVersion: 4,
      chosenAuthority: totals(report.chosenAuthority, 'Chosen-authority totals'),
      postgresqlFallback: totals(report.postgresqlFallback, 'PostgreSQL-fallback totals')
    },
    pack: {
      historicalSourceCommit: commit(
        pack.historicalSourceCommit,
        'Historical parity pack source commit'
      ),
      historicalManifestFileSha256: digest(
        pack.historicalManifestFileSha256,
        'Historical parity manifest-file digest'
      ),
      historicalManifestSha256: digest(
        pack.historicalManifestSha256,
        'Historical parity manifest digest'
      ),
      sourceLockSha256: digest(pack.sourceLockSha256, 'Parity pack source-lock digest'),
      hot: releaseAsset(pack.hot, 'Parity hot asset'),
      details: releaseAsset(pack.details, 'Parity details asset')
    },
    oracle: {
      lockSha256: digest(oracle.lockSha256, 'Parity oracle-lock digest'),
      database: text(oracle.database, 'Parity oracle database'),
      upstreamIchiranCommit: commit(
        oracle.upstreamIchiranCommit,
        'Parity oracle upstream Ichiran commit'
      )
    },
    policy: {
      runtimeAllowlist: [],
      decision: text(policy.decision, 'Parity review decision')
    },
    rows: root.rows.map(reviewRow)
  };
}

function canonicalValue(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(canonicalValue);
  if (!value || typeof value !== 'object') return value;
  const input = value as Record<string, unknown>;
  return Object.fromEntries(Object.keys(input).sort().map(key => [key, canonicalValue(input[key])]));
}

/** Stable identity of one complete retained diagnostic observation. */
export function parityObservationSha256(value: unknown): string {
  return sha256(JSON.stringify(canonicalValue(value)));
}

/** Stable identity of a complete report, excluding only its run timestamp. */
export function parityReportNormalizedSha256(value: unknown): string {
  const input = record(value, 'Source-compiler parity report');
  const { generatedAt: _generatedAt, ...stable } = input;
  return sha256(JSON.stringify(canonicalValue(stable)));
}

function reportTotals(value: unknown, label: string): ParityTotals {
  const input = record(value, label);
  const result = {
    operations: integer(input.operations, `${label} operations`),
    exact: integer(input.exact, `${label} exact`),
    analyzerDeltas: integer(input.analyzerDivergent, `${label} analyzer divergences`),
    presentationDeltas: integer(
      input.presentationDivergent,
      `${label} presentation divergences`
    ),
    errors: integer(input.errors, `${label} errors`)
  };
  const divergent = integer(input.divergent, `${label} divergences`);
  if (divergent !== result.analyzerDeltas + result.presentationDeltas
    || result.operations !== result.exact + divergent + result.errors) {
    throw new Error(`${label} counts do not close`);
  }
  return result;
}

function sameTotals(left: ParityTotals, right: ParityTotals): boolean {
  return left.operations === right.operations
    && left.exact === right.exact
    && left.analyzerDeltas === right.analyzerDeltas
    && left.presentationDeltas === right.presentationDeltas
    && left.errors === right.errors;
}

function authorityForSample(sample: Record<string, unknown>, index: number): ParityAuthority {
  if (Object.hasOwn(sample, 'cleanDifference')) return 'postgresql-fallback';
  if (Object.hasOwn(sample, 'pathDifference')
    || Object.hasOwn(sample, 'detailedDifference')) return 'current-lisp';
  throw new Error(`Parity diagnostic sample ${index} does not identify an oracle side`);
}

function reviewDifference(
  sample: Record<string, unknown>,
  authority: ParityAuthority,
  index: number
): { readonly path: string; readonly expected: string; readonly actual: string } {
  const value = authority === 'postgresql-fallback'
    ? sample.cleanDifference
    : sample.pathDifference ?? sample.detailedDifference;
  const difference = record(value, `Parity diagnostic sample ${index} review difference`);
  exactKeys(difference, ['path', 'kind', 'expected', 'actual'],
    `Parity diagnostic sample ${index} review difference`);
  if (!['type', 'missing', 'length', 'value'].includes(String(difference.kind))) {
    throw new Error(`Parity diagnostic sample ${index} has an invalid difference kind`);
  }
  return {
    path: text(difference.path, `Parity diagnostic sample ${index} difference path`),
    expected: text(difference.expected, `Parity diagnostic sample ${index} qualified value`),
    actual: text(difference.actual, `Parity diagnostic sample ${index} source value`)
  };
}

function sameTestedAsset(
  tested: TestedReleaseAsset,
  attested: ParityReleaseAsset
): boolean {
  return tested.encoding === attested.encoding
    && tested.downloadBytes === attested.download.bytes
    && tested.downloadSha256 === attested.download.sha256
    && tested.installedBytes === attested.installed.bytes
    && tested.installedSha256 === attested.installed.sha256;
}

function sameTestedRelease(
  left: TestedReleaseIdentity,
  right: TestedReleaseIdentity
): boolean {
  const sameAsset = (a: TestedReleaseAsset, b: TestedReleaseAsset): boolean =>
    a.file === b.file
    && a.encoding === b.encoding
    && a.downloadBytes === b.downloadBytes
    && a.downloadSha256 === b.downloadSha256
    && a.installedBytes === b.installedBytes
    && a.installedSha256 === b.installedSha256;
  return left.sourceCommit === right.sourceCommit
    && left.manifestFileSha256 === right.manifestFileSha256
    && left.manifestSha256 === right.manifestSha256
    && sameAsset(left.hot, right.hot)
    && sameAsset(left.details, right.details);
}

function rowKey(authority: ParityAuthority, suite: string, request: string): string {
  return `${authority}\u0000${suite}\u0000${request}`;
}

function assertReviewCounts(
  rows: readonly ParityReviewRow[],
  authority: ParityAuthority,
  expected: ParityTotals
): void {
  const selected = rows.filter(row => row.authority === authority);
  const analyzer = selected.filter(row => row.classification === 'analyzer').length;
  const presentation = selected.filter(row => row.classification === 'presentation').length;
  if (analyzer !== expected.analyzerDeltas || presentation !== expected.presentationDeltas) {
    throw new Error(`${authority} reviewed row counts are stale`);
  }
}

/**
 * Require a bijection between every retained diagnostic observation and one
 * reviewed attestation row. This is qualification evidence, never an analyzer
 * or release acceptance list.
 */
export function validateSourceCompilerParityReport(
  attestation: SourceCompilerParityAttestation,
  value: unknown
): VerifiedParityAttestation {
  const report = record(value, 'Source-compiler parity report');
  if (report.formatVersion !== attestation.report.formatVersion) {
    throw new Error('Parity diagnostic report format is stale');
  }
  text(report.generatedAt, 'Parity diagnostic report generatedAt');
  if (report.completeCorpus !== true) throw new Error('Parity diagnostic report is not complete');
  const testedRelease = testedReleaseIdentity(report.testedRelease);
  if (testedRelease.sourceCommit !== attestation.pack.historicalSourceCommit
    || testedRelease.manifestFileSha256 !== attestation.pack.historicalManifestFileSha256
    || testedRelease.manifestSha256 !== attestation.pack.historicalManifestSha256
    || !sameTestedAsset(testedRelease.hot, attestation.pack.hot)
    || !sameTestedAsset(testedRelease.details, attestation.pack.details)) {
    throw new Error('Parity diagnostic report tested-release identity is stale');
  }
  const releaseLock = record(report.releaseInputLock, 'Parity report release lock');
  exactKeys(releaseLock, ['kind', 'sha256'], 'Parity report release lock');
  if (releaseLock.kind !== 'source-compiler'
    || releaseLock.sha256 !== attestation.pack.sourceLockSha256) {
    throw new Error('Parity report is not bound to the attested source lock');
  }
  const oracle = record(report.frozenOracleLock, 'Parity report oracle lock');
  exactKeys(oracle, ['sha256', 'database', 'upstreamIchiranCommit'],
    'Parity report oracle lock');
  if (oracle.sha256 !== attestation.oracle.lockSha256
    || oracle.database !== attestation.oracle.database
    || oracle.upstreamIchiranCommit !== attestation.oracle.upstreamIchiranCommit) {
    throw new Error('Parity report is not bound to the attested oracle lock');
  }
  const totalsInput = record(report.authoritativeOracleTotals, 'Parity authoritative totals');
  const chosen = reportTotals(totalsInput.allComparisons, 'Chosen-authority report');
  const fallback = reportTotals(
    totalsInput.frozenFallbackCleanSemantic,
    'PostgreSQL-fallback report'
  );
  if (chosen.errors !== 0 || fallback.errors !== 0) {
    throw new Error('Parity diagnostic report contains analyzer errors');
  }
  const gate = record(report.gate, 'Parity report gate');
  if (!Array.isArray(gate.currentOracleAllowlist) || gate.currentOracleAllowlist.length !== 0) {
    throw new Error('Parity diagnostic report runtime allowlist must be empty');
  }
  if (!Array.isArray(report.samples)) throw new Error('Parity diagnostic report has no samples');
  const expectedSamples = chosen.analyzerDeltas + chosen.presentationDeltas
    + fallback.analyzerDeltas + fallback.presentationDeltas;
  if (report.samples.length < expectedSamples) {
    throw new Error(
      `Parity diagnostic report is missing retained samples (${report.samples.length}/${expectedSamples})`
    );
  }

  const rowsByKey = new Map<string, ParityReviewRow>();
  const rowIds = new Set<string>();
  for (const row of attestation.rows) {
    if (rowIds.has(row.id)) throw new Error(`Duplicate parity review row id ${row.id}`);
    rowIds.add(row.id);
    const key = rowKey(row.authority, row.suite, row.request);
    if (rowsByKey.has(key)) {
      throw new Error(`Duplicate parity review row ${row.authority}/${row.suite}/${row.request}`);
    }
    rowsByKey.set(key, row);
  }

  const observed = new Set<string>();
  for (const [index, sampleValue] of report.samples.entries()) {
    const sample = record(sampleValue, `Parity diagnostic sample ${index}`);
    const suite = text(sample.suite, `Parity diagnostic sample ${index} suite`);
    const request = text(sample.request, `Parity diagnostic sample ${index} request`);
    const authority = authorityForSample(sample, index);
    const qualifiedOutputSha256 = digest(
      sample.qualifiedOutputSha256,
      `Parity diagnostic sample ${index} qualified output digest`
    );
    const sourceOutputSha256 = digest(
      sample.sourceOutputSha256,
      `Parity diagnostic sample ${index} source output digest`
    );
    if (qualifiedOutputSha256 === sourceOutputSha256) {
      throw new Error(`Parity diagnostic sample ${index} has equal complete output digests`);
    }
    const key = rowKey(authority, suite, request);
    if (observed.has(key)) {
      throw new Error(`Duplicate parity diagnostic row ${authority}/${suite}/${request}`);
    }
    observed.add(key);
    const row = rowsByKey.get(key);
    if (!row) throw new Error(`Unreviewed parity diagnostic row ${authority}/${suite}/${request}`);
    if (sample.classification !== row.classification) {
      throw new Error(`Parity review row ${row.id} has stale classification`);
    }
    const difference = reviewDifference(sample, authority, index);
    if (row.field !== difference.path
      || row.qualified !== difference.expected
      || row.source !== difference.actual) {
      throw new Error(`Parity review row ${row.id} has stale reviewed evidence`);
    }
    if (parityObservationSha256(sample) !== row.observationSha256) {
      throw new Error(`Parity review row ${row.id} is stale`);
    }
  }
  const extra = attestation.rows.find(row =>
    !observed.has(rowKey(row.authority, row.suite, row.request)));
  if (extra) throw new Error(`Extra parity review row ${extra.id}`);
  if (report.samples.length !== expectedSamples) {
    throw new Error(`Parity diagnostic report has extra retained samples`);
  }
  assertReviewCounts(attestation.rows, 'current-lisp', chosen);
  assertReviewCounts(attestation.rows, 'postgresql-fallback', fallback);
  if (!sameTotals(chosen, attestation.report.chosenAuthority)
    || !sameTotals(fallback, attestation.report.postgresqlFallback)) {
    throw new Error('Parity diagnostic aggregate counts are stale');
  }
  const normalizedSha256 = parityReportNormalizedSha256(report);
  if (normalizedSha256 !== attestation.report.normalizedSha256) {
    throw new Error('Parity diagnostic report digest is stale');
  }
  return {
    reportNormalizedSha256: normalizedSha256,
    sourceLockSha256: attestation.pack.sourceLockSha256,
    oracleLockSha256: attestation.oracle.lockSha256,
    chosenAuthorityDeltas: chosen.analyzerDeltas + chosen.presentationDeltas,
    postgresqlFallbackDeltas: fallback.analyzerDeltas + fallback.presentationDeltas,
    reviewedRows: attestation.rows.length
  };
}

export interface VerifyParityAttestationInput {
  readonly attestationPath: string;
  readonly reportPath: string;
  readonly releaseDirectory: string;
  readonly sourceLockPath: string;
  readonly oracleLockPath: string;
}

/** Bind one tracked diagnostic report to its review and exact inputs. */
export async function verifySourceCompilerParityAttestation(
  input: VerifyParityAttestationInput
): Promise<VerifiedParityAttestation> {
  const [attestationBytes, reportBytes, manifestBytes, sourceLockBytes, oracleLockBytes] =
    await Promise.all([
      readFile(input.attestationPath),
      readFile(input.reportPath),
      readFile(resolve(input.releaseDirectory, 'manifest.json')),
      readFile(input.sourceLockPath),
      readFile(input.oracleLockPath)
    ]);
  const attestation = parseSourceCompilerParityAttestation(
    JSON.parse(attestationBytes.toString('utf8'))
  );
  const reportValue: unknown = JSON.parse(reportBytes.toString('utf8'));
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(manifestBytes.toString('utf8')),
    value => sha256(value)
  );
  const oracleLock = parseBrowserAlphaSourceLock(oracleLockBytes.toString('utf8'));
  const [hotBytes, detailsBytes] = await Promise.all([
    readFile(resolve(input.releaseDirectory, manifest.hot.file)),
    readFile(resolve(input.releaseDirectory, manifest.details.file))
  ]);

  if (sha256(sourceLockBytes) !== attestation.pack.sourceLockSha256
    || manifest.sourcesLockSha256 !== attestation.pack.sourceLockSha256) {
    throw new Error('Parity source-lock identity is stale');
  }
  const hotInstalled = manifest.hot.encoding === 'gzip'
    ? new Uint8Array(gunzipSync(hotBytes))
    : hotBytes;
  const detailsInstalled = manifest.details.encoding === 'gzip'
    ? new Uint8Array(gunzipSync(detailsBytes))
    : detailsBytes;
  if (manifest.hot.encoding !== attestation.pack.hot.encoding
    || hotBytes.byteLength !== attestation.pack.hot.download.bytes
    || sha256(hotBytes) !== attestation.pack.hot.download.sha256
    || hotInstalled.byteLength !== attestation.pack.hot.installed.bytes
    || sha256(hotInstalled) !== attestation.pack.hot.installed.sha256
    || manifest.hot.downloadBytes !== attestation.pack.hot.download.bytes
    || manifest.hot.downloadSha256 !== attestation.pack.hot.download.sha256
    || manifest.hot.installedBytes !== attestation.pack.hot.installed.bytes
    || manifest.hot.installedSha256 !== attestation.pack.hot.installed.sha256) {
    throw new Error('Parity hot-pack identity is stale');
  }
  if (manifest.details.encoding !== attestation.pack.details.encoding
    || detailsBytes.byteLength !== attestation.pack.details.download.bytes
    || sha256(detailsBytes) !== attestation.pack.details.download.sha256
    || detailsInstalled.byteLength !== attestation.pack.details.installed.bytes
    || sha256(detailsInstalled) !== attestation.pack.details.installed.sha256
    || manifest.details.downloadBytes !== attestation.pack.details.download.bytes
    || manifest.details.downloadSha256 !== attestation.pack.details.download.sha256
    || manifest.details.installedBytes !== attestation.pack.details.installed.bytes
    || manifest.details.installedSha256 !== attestation.pack.details.installed.sha256) {
    throw new Error('Parity details-pack identity is stale');
  }
  if (sha256(oracleLockBytes) !== attestation.oracle.lockSha256
    || oracleLock.database.name !== attestation.oracle.database
    || oracleLock.upstreamIchiran.commit !== attestation.oracle.upstreamIchiranCommit) {
    throw new Error('Parity oracle-lock identity is stale');
  }

  const manifestAsset = (side: 'hot' | 'details'): TestedReleaseAsset => {
    const value = manifest[side];
    return {
      file: value.file,
      encoding: value.encoding,
      downloadBytes: value.downloadBytes,
      downloadSha256: value.downloadSha256,
      installedBytes: value.installedBytes,
      installedSha256: value.installedSha256
    };
  };
  const currentRelease: TestedReleaseIdentity = {
    sourceCommit: manifest.sourceCommit,
    manifestFileSha256: sha256(manifestBytes),
    manifestSha256: manifest.manifestSha256,
    hot: manifestAsset('hot'),
    details: manifestAsset('details')
  };
  const report = record(reportValue, 'Source-compiler parity report');
  const testedRelease = testedReleaseIdentity(report.testedRelease);
  const historical = testedRelease.sourceCommit === attestation.pack.historicalSourceCommit
    && testedRelease.manifestFileSha256 === attestation.pack.historicalManifestFileSha256
    && testedRelease.manifestSha256 === attestation.pack.historicalManifestSha256;
  let validatedReport: unknown = report;
  if (!historical) {
    if (!sameTestedRelease(testedRelease, currentRelease)) {
      throw new Error(
        'Parity report tested release does not match the historical attestation or supplied release'
      );
    }
    validatedReport = {
      ...report,
      testedRelease: {
        ...record(report.testedRelease, 'Parity report tested release'),
        sourceCommit: attestation.pack.historicalSourceCommit,
        manifestFileSha256: attestation.pack.historicalManifestFileSha256,
        manifestSha256: attestation.pack.historicalManifestSha256
      }
    };
  }

  return validateSourceCompilerParityReport(attestation, validatedReport);
}
