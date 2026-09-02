import { spawn } from 'node:child_process';
import { readFile, realpath } from 'node:fs/promises';
import { join } from 'node:path';
import { gunzipSync } from 'node:zlib';

import {
  ANALYZER_ANNOTATIONS_SECTION_ID
} from '../browser-pack/analyzer-annotations.js';
import type { AnalyzerSupportSource } from '../browser-pack/analyzer-support.js';
import {
  assertAnalyzerReleaseSize,
  buildAnalyzerRelease,
  parseAnalyzerReleaseManifest
} from '../browser-pack/release-manifest.js';
import {
  assertActiveReleaseGeneration,
  assertExactReleaseInventory,
  publishAnalyzerRelease
} from '../browser-pack/release-publication.js';
import {
  assertBytesEqual,
  deterministicJson,
  sha256Bytes,
  type BrowserAlphaArtifactCounts
} from '../browser-pack/release-orchestration.js';
import {
  ANALYZER_SUPPORT_SECTION_ID,
  MORPHOLOGY_SECTION_ID,
  ROOT_PAYLOAD_SECTION_ID,
  SURFACE_INDEX_SECTION_ID,
  openPack
} from '@ichiran/core';
import { buildSourceCompilerBinarySections, buildSourceCompilerHotPack } from './release-input.js';
import {
  parseGeneratedOrderAttestation,
  parseRootPayloadOrderAttestation
} from './release-evidence.js';
import {
  artifactIdentities,
  compareQualifiedArtifactBytes,
  compareQualifiedArtifactCounts,
  parseSurfaceCompilerStats,
  sourceReleaseArtifactCounts,
  type ArtifactComparison,
  type CountDifference,
  type QualifiedArtifactBytes,
  type SurfaceCompilerStats
} from './release-comparison.js';
import { writeBoundedSurfaceIndexTsv } from './surface-index-spool.js';
import type { VerifiedSourceCompilerLock } from './source-lock.js';
import type { MorphologySource } from '../browser-pack/morphology-compiler.js';
import type { CanonicalEntry } from './model.js';
import type { BoundedAnalyzerSupportSummary } from './analyzer-support-stream.js';
import type { GeneratedProjectionSpoolSummary } from './generated-projection-spool.js';
import type { PhysicalTarget } from './conjugation-emissions-physical.js';
import type { ConjugationRulePaths } from '../data/conj-rules.js';

const ROOT_ORDER_ATTESTATION_SHA256 = '12ca177bf7765e4337f3c1cc4d836a7bcfc84b3f60b08e07d6eb238ad72dc4cf';
const GENERATED_ORDER_ATTESTATION_SHA256 =
  '3ecb9af387502836b45a98a6570bbaadeaf0ba2a0dc530928bcdeae1d7ae36c1';
const QUALIFIED_ARTIFACT_INDEX_SHA256 = 'a032bbd11c257259877b438a79c674544985a58acdaff86327ae57ae8cbeb3ac';

interface CommandResult {
  readonly stdout: string;
  readonly stderr: string;
}

export interface SourceReleaseOutputInput {
  readonly repository: string;
  readonly output: string;
  readonly temporaryDirectory: string;
  readonly sourceCommit: string;
  readonly packVersion: string;
  readonly sourceLock: VerifiedSourceCompilerLock;
  readonly baseline?: {
    readonly directory: string;
    readonly directOrderAttestationPath: string;
    readonly generatedOrderAttestationPath: string;
  };
  readonly entries: readonly CanonicalEntry[];
  readonly morphology: MorphologySource;
  readonly support: AnalyzerSupportSource;
  readonly occurrencesPath: string;
  readonly physicalTargets: readonly PhysicalTarget[];
  readonly conjugationRules: ConjugationRulePaths;
  readonly surfaceChunkRows?: number;
  readonly projectionSummary: SourceReleaseProjectionSummary;
  readonly sourceSummary: SourceReleaseSemanticSummary;
}

export interface SourceReleaseSemanticSummary {
  readonly mode: 'baseline' | 'update';
  readonly jmdict: { readonly id: string; readonly path: string };
  readonly canonicalEntries: number;
  readonly jmdictEntries: number;
  readonly customCreatedRoots: number;
  readonly chronologicalErrataRows: number;
  readonly conjugationErrataRows: number;
  readonly errataNoopRowIds: readonly string[];
  readonly compatibilityRows: number;
  readonly compatibilityUsage: readonly {
    readonly id: string;
    readonly kind: string;
    readonly phase: string;
  }[];
}

export interface SourceReleaseProjectionSummary {
  readonly spool: GeneratedProjectionSpoolSummary;
  readonly targets: number;
  readonly generatedTargets: number;
  readonly ruleAliases: number;
  readonly aliasProperties: number;
  readonly phases: Readonly<Record<number, number>>;
  readonly patches: number;
  readonly analyzerSupport: BoundedAnalyzerSupportSummary;
}

export interface SourceReleaseOutput {
  readonly generation: string;
  readonly report: Uint8Array;
  readonly counts: BrowserAlphaArtifactCounts;
}

function command(executable: string, args: readonly string[], cwd: string): Promise<CommandResult> {
  return new Promise((resolvePromise, reject) => {
    const child = spawn(executable, args, { cwd, stdio: ['ignore', 'pipe', 'pipe'] });
    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    child.stdout.on('data', (chunk: Buffer) => stdout.push(chunk));
    child.stderr.on('data', (chunk: Buffer) => stderr.push(chunk));
    child.once('error', reject);
    child.once('close', (code, signal) => {
      const result = {
        stdout: Buffer.concat(stdout).toString('utf8'),
        stderr: Buffer.concat(stderr).toString('utf8')
      };
      if (code === 0) resolvePromise(result);
      else reject(new Error(`${executable} failed (${signal ?? code}): ${result.stderr || result.stdout}`));
    });
  });
}

async function surfaceCompiler(repository: string): Promise<string> {
  const manifest = join(repository, 'packages/data/tools/surface-index/Cargo.toml');
  await command('cargo', ['build', '--locked', '--release', '--manifest-path', manifest], repository);
  return join(repository, 'packages/data/tools/surface-index/target/release/ichiran-surface-index');
}

async function assertSourceCheckoutUnchanged(
  repository: string,
  sourceCommit: string
): Promise<void> {
  const head = (await command('git', ['rev-parse', 'HEAD'], repository)).stdout.trim();
  if (head !== sourceCommit) {
    throw new Error(`Source checkout moved during release build (${sourceCommit} -> ${head})`);
  }
  const status = (await command(
    'git',
    ['status', '--porcelain=v1', '--untracked-files=all'],
    repository
  )).stdout;
  if (status.length !== 0) {
    throw new Error('Source checkout changed during release build; refusing to activate it');
  }
}

async function compileSurface(
  compiler: string,
  input: string,
  output: string,
  repository: string
): Promise<SurfaceCompilerStats> {
  const result = await command(compiler, ['--input', input, '--output', output], repository);
  return parseSurfaceCompilerStats(result.stderr);
}

async function verifyQualifiedArtifactIndex(directory: string): Promise<void> {
  const indexBytes = new Uint8Array(await readFile(join(directory, 'artifact-sha256.txt')));
  if (sha256Bytes(indexBytes) !== QUALIFIED_ARTIFACT_INDEX_SHA256) {
    throw new Error('Qualified artifact checksum index is not the immutable reviewed release index');
  }
  const identities = new Map<string, string>();
  for (const line of new TextDecoder().decode(indexBytes).trim().split('\n')) {
    const match = /^([0-9a-f]{64})  ([^/]+)$/.exec(line);
    if (!match) throw new Error(`Invalid qualified artifact checksum row: ${line}`);
    identities.set(match[2]!, match[1]!);
  }
  for (const name of ['manifest.json', 'hot.bin.gz', 'details.bin.gz', 'stats.json']) {
    const expected = identities.get(name);
    if (!expected) throw new Error(`Qualified artifact checksum index omits ${name}`);
    const actual = sha256Bytes(new Uint8Array(await readFile(join(directory, name))));
    if (actual !== expected) throw new Error(`Qualified artifact ${name} is not the reviewed release byte stream`);
  }
}

async function qualifiedArtifacts(directory: string): Promise<{
  readonly bytes: QualifiedArtifactBytes;
  readonly counts: BrowserAlphaArtifactCounts;
}> {
  await verifyQualifiedArtifactIndex(directory);
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(await readFile(join(directory, 'manifest.json'), 'utf8')),
    value => sha256Bytes(new TextEncoder().encode(value))
  );
  if (manifest.hot.encoding !== 'gzip' || manifest.details.encoding !== 'gzip') {
    throw new Error('Qualified baseline assets must use the reviewed gzip representation');
  }
  const hotDownload = new Uint8Array(await readFile(join(directory, manifest.hot.file)));
  const detailsDownload = new Uint8Array(await readFile(join(directory, manifest.details.file)));
  for (const [label, asset, bytes] of [
    ['hot', manifest.hot, hotDownload],
    ['details', manifest.details, detailsDownload]
  ] as const) {
    if (bytes.byteLength !== asset.downloadBytes || sha256Bytes(bytes) !== asset.downloadSha256) {
      throw new Error(`Qualified ${label} download identity differs from its manifest`);
    }
  }
  const hot = new Uint8Array(gunzipSync(hotDownload));
  const details = new Uint8Array(gunzipSync(detailsDownload));
  for (const [label, asset, bytes] of [
    ['hot', manifest.hot, hot],
    ['details', manifest.details, details]
  ] as const) {
    if (bytes.byteLength !== asset.installedBytes || sha256Bytes(bytes) !== asset.installedSha256) {
      throw new Error(`Qualified ${label} installed identity differs from its manifest`);
    }
  }
  const pack = openPack(hot);
  pack.verifyAll();
  const stats = JSON.parse(await readFile(join(directory, 'stats.json'), 'utf8')) as {
    artifacts: BrowserAlphaArtifactCounts;
  };
  return {
    bytes: {
      surfaceIndex: pack.getSection(SURFACE_INDEX_SECTION_ID),
      rootPayload: pack.getSection(ROOT_PAYLOAD_SECTION_ID),
      morphology: pack.getSection(MORPHOLOGY_SECTION_ID),
      analyzerSupport: pack.getSection(ANALYZER_SUPPORT_SECTION_ID),
      analyzerAnnotations: pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID),
      details
    },
    counts: stats.artifacts
  };
}

async function compareQualifiedBaseline(
  source: QualifiedArtifactBytes,
  counts: BrowserAlphaArtifactCounts,
  baseline: NonNullable<SourceReleaseOutputInput['baseline']>
): Promise<{
  readonly countDifferences: readonly CountDifference[];
  readonly artifacts: readonly ArtifactComparison[];
  readonly orderAttestation: ReturnType<typeof parseRootPayloadOrderAttestation>;
  readonly generatedOrderAttestation: ReturnType<typeof parseGeneratedOrderAttestation>;
  readonly generatedOrderAttestationSha256: string;
}> {
  const qualified = await qualifiedArtifacts(baseline.directory);
  const attestationBytes = new Uint8Array(await readFile(baseline.directOrderAttestationPath));
  const orderAttestation = parseRootPayloadOrderAttestation(
    JSON.parse(new TextDecoder().decode(attestationBytes))
  );
  const generatedAttestationBytes = new Uint8Array(
    await readFile(baseline.generatedOrderAttestationPath)
  );
  if (sha256Bytes(generatedAttestationBytes) !== GENERATED_ORDER_ATTESTATION_SHA256) {
    throw new Error('Generated-order attestation is not the reviewed baseline proof');
  }
  const generatedOrderAttestation = parseGeneratedOrderAttestation(
    JSON.parse(new TextDecoder().decode(generatedAttestationBytes))
  );
  if (source.rootPayload.byteLength !== orderAttestation.sourcePayload.bytes
    || qualified.bytes.rootPayload.byteLength !== orderAttestation.qualifiedPayload.bytes) {
    throw new Error('Root payload byte counts differ from the reviewed order attestation');
  }
  const generatedReview = {
    attestationSha256: GENERATED_ORDER_ATTESTATION_SHA256,
    attestation: generatedOrderAttestation
  };
  const countDifferences = compareQualifiedArtifactCounts(
    counts,
    qualified.counts,
    generatedReview
  );
  const artifacts = compareQualifiedArtifactBytes(source, qualified.bytes, {
    sourceSha256: orderAttestation.sourcePayload.sha256,
    qualifiedSha256: orderAttestation.qualifiedPayload.sha256,
    attestation: attestationBytes,
    attestationSha256: ROOT_ORDER_ATTESTATION_SHA256,
    fullEvidence: orderAttestation.fullEvidence,
    provenance: orderAttestation.provenance.qualifiedOrder,
    policy: orderAttestation.deterministicPolicy,
    preservedBehavior: orderAttestation.preservedBehavior.verdict
  }, generatedReview);
  return {
    countDifferences,
    artifacts,
    orderAttestation,
    generatedOrderAttestation,
    generatedOrderAttestationSha256: GENERATED_ORDER_ATTESTATION_SHA256
  };
}

/** Build, compare, and atomically publish one source-native pack-v1 release. */
export async function writeSourceCompilerRelease(
  input: SourceReleaseOutputInput
): Promise<SourceReleaseOutput> {
  const sections = buildSourceCompilerBinarySections({
    entries: input.entries,
    morphology: input.morphology,
    support: input.support,
    conjugationRules: input.conjugationRules
  });
  const rebuiltSections = buildSourceCompilerBinarySections({
    entries: input.entries,
    morphology: input.morphology,
    support: input.support,
    conjugationRules: input.conjugationRules
  });
  assertBytesEqual(sections.root.bytes, rebuiltSections.root.bytes, 'Root payload');
  assertBytesEqual(sections.details.bytes, rebuiltSections.details.bytes, 'Details');
  assertBytesEqual(sections.morphology.bytes, rebuiltSections.morphology.bytes, 'Morphology');
  assertBytesEqual(sections.support.bytes, rebuiltSections.support.bytes, 'Analyzer support');
  assertBytesEqual(sections.annotations.bytes, rebuiltSections.annotations.bytes, 'Annotations');
  const surfaceTsv = join(input.temporaryDirectory, 'surface.tsv');
  const surfaceSpool = await writeBoundedSurfaceIndexTsv({
    entries: input.entries,
    occurrencesPath: input.occurrencesPath,
    physicalTargets: input.physicalTargets,
    temporaryDirectory: input.temporaryDirectory,
    destination: surfaceTsv,
    ...(input.surfaceChunkRows === undefined ? {} : { maxChunkRows: input.surfaceChunkRows })
  });
  const compiler = await surfaceCompiler(input.repository);
  const firstPath = join(input.temporaryDirectory, 'surface-first.bin');
  const secondPath = join(input.temporaryDirectory, 'surface-second.bin');
  const [firstStats, secondStats] = await Promise.all([
    compileSurface(compiler, surfaceTsv, firstPath, input.repository),
    compileSurface(compiler, surfaceTsv, secondPath, input.repository)
  ]);
  if (JSON.stringify(firstStats) !== JSON.stringify(secondStats)) {
    throw new Error('Surface-index rebuild changed deterministic counts');
  }
  if (surfaceSpool.surfaces !== firstStats.input) {
    throw new Error(`Rust surface compiler read ${firstStats.input}/${surfaceSpool.surfaces} surfaces`);
  }
  const surfaceBytes = new Uint8Array(await readFile(firstPath));
  if (surfaceBytes.byteLength !== firstStats.bytes) {
    throw new Error(`Rust surface compiler wrote ${surfaceBytes.byteLength}/${firstStats.bytes} bytes`);
  }
  assertBytesEqual(surfaceBytes, new Uint8Array(await readFile(secondPath)), 'Surface index');

  const hot = buildSourceCompilerHotPack(sections, surfaceBytes);
  assertBytesEqual(hot, buildSourceCompilerHotPack(sections, surfaceBytes), 'Hot pack');
  const sourceArtifacts: QualifiedArtifactBytes = {
    surfaceIndex: surfaceBytes,
    rootPayload: sections.root.bytes,
    morphology: sections.morphology.bytes,
    analyzerSupport: sections.support.bytes,
    analyzerAnnotations: sections.annotations.bytes,
    details: sections.details.bytes
  };
  const counts = sourceReleaseArtifactCounts(sections, firstStats);
  const qualifiedComparison = input.baseline === undefined
    ? null
    : await compareQualifiedBaseline(sourceArtifacts, counts, input.baseline);

  const releaseOptions = {
    packVersion: input.packVersion,
    sourceCommit: input.sourceCommit,
    sourcesLockSha256: input.sourceLock.sha256,
    hot,
    details: sections.details.bytes,
    hotEncoding: 'gzip',
    detailsEncoding: 'gzip'
  } as const;
  const release = buildAnalyzerRelease(releaseOptions);
  const releaseSize = assertAnalyzerReleaseSize(release);
  const rebuilt = buildAnalyzerRelease(releaseOptions);
  assertBytesEqual(release.hotDownload, rebuilt.hotDownload, 'Compressed hot asset');
  assertBytesEqual(release.detailsDownload, rebuilt.detailsDownload, 'Compressed details asset');
  assertBytesEqual(release.manifestBytes, rebuilt.manifestBytes, 'Release manifest');
  const reportValue = {
    formatVersion: 1,
    packFormat: 1,
    packVersion: input.packVersion,
    sourceCommit: input.sourceCommit,
    cleanSourceRequired: true,
    postgresqlRequired: false,
    sources: {
      lockSha256: input.sourceLock.sha256,
      qualifiedArtifactTag: input.sourceLock.lock.baseline.qualifiedArtifactTag,
      upstreamIchiranCommit: input.sourceLock.lock.baseline.upstreamIchiranCommit,
      upstreamDataReleaseTag: input.sourceLock.lock.baseline.upstreamDataReleaseTag,
      files: input.sourceLock.files,
      semantic: input.sourceSummary
    },
    surfaceSpool,
    projection: input.projectionSummary,
    artifacts: counts,
    artifactIdentities: artifactIdentities(sourceArtifacts),
    qualifiedComparison,
    pack: { bytes: hot.byteLength, sha256: sha256Bytes(hot) },
    release: { manifest: release.manifest, size: releaseSize }
  };
  const report = deterministicJson(reportValue);
  assertBytesEqual(report, deterministicJson(reportValue), 'Release stats');
  const files = new Map<string, Uint8Array>([
    [release.manifest.hot.file, release.hotDownload],
    [release.manifest.details.file, release.detailsDownload],
    ['manifest.json', release.manifestBytes],
    ['stats.json', report]
  ]);
  const generation = await publishAnalyzerRelease(input.output, files, {
    verify: async directory => {
      await assertExactReleaseInventory(directory, [...files.keys()]);
      const publishedHot = new Uint8Array(
        gunzipSync(await readFile(join(directory, release.manifest.hot.file)))
      );
      const publishedDetails = new Uint8Array(
        gunzipSync(await readFile(join(directory, release.manifest.details.file)))
      );
      openPack(publishedHot).verifyAll();
      assertBytesEqual(publishedHot, hot, 'Published hot pack');
      assertBytesEqual(publishedDetails, sections.details.bytes, 'Published details');
    },
    beforeActivate: async () => {
      await assertSourceCheckoutUnchanged(input.repository, input.sourceCommit);
    }
  });
  await assertActiveReleaseGeneration(input.output, [...files.keys()]);
  if (await realpath(input.output) !== await realpath(generation)) {
    throw new Error('A different release generation replaced the one just published');
  }
  return { generation, report, counts };
}
