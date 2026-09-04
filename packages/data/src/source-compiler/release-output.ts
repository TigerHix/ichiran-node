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
  type AnalyzerReleaseAsset
} from '../browser-pack/release-manifest.js';
import {
  assertActiveReleaseGeneration,
  assertExactReleaseInventory,
  publishAnalyzerRelease,
  readRegularArtifact
} from '../browser-pack/release-publication.js';
import {
  assertBytesEqual,
  deterministicJson,
  sha256Bytes,
  type BrowserAlphaArtifactCounts
} from './artifact-contract.js';
import {
  ANALYZER_SUPPORT_SECTION_ID,
  MORPHOLOGY_SECTION_ID,
  ROOT_PAYLOAD_SECTION_ID,
  SURFACE_INDEX_SECTION_ID,
  openPack
} from '@ichiran/core/compiler';
import { buildSourceCompilerBinarySections, buildSourceCompilerHotPack } from './release-input.js';
import { assertSourceReleaseDestination } from './release-path.js';
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
import type { SurfaceIndexTsvSpoolSummary } from './surface-index-spool.js';
import type { VerifiedSourceCompilerLock } from './source-lock.js';
import type { MorphologySource } from '../browser-pack/morphology-compiler.js';
import type { CanonicalEntry } from './model.js';
import type { LocaleGlossEntrySource } from '../browser-pack/locale-gloss.js';
import type { BoundedAnalyzerSupportSummary } from './analyzer-support-stream.js';
import type { GeneratedProjectionSpoolSummary } from './generated-projection-spool.js';
import type { ConjugationRulePaths } from '../data/conj-rules.js';

const ROOT_ORDER_ATTESTATION_SHA256 = '12ca177bf7765e4337f3c1cc4d836a7bcfc84b3f60b08e07d6eb238ad72dc4cf';
const GENERATED_ORDER_ATTESTATION_SHA256 =
  '4f2a767eb48b09194af7e20070e2b5e79765a70b5d6ebf4eaa9a3048ef5776cf';
const HISTORICAL_QUALIFIED_ARTIFACT_INDEX_SHA256 =
  'a032bbd11c257259877b438a79c674544985a58acdaff86327ae57ae8cbeb3ac';
const SOURCE_COMPILER_RUST_TOOLCHAIN = '1.92.0';
const QUALIFIED_ARTIFACT_NAMES = [
  'manifest.json',
  'hot.bin.gz',
  'details.bin.gz',
  'stats.json'
] as const;

/** Immutable format-1 evidence; never a shape accepted by the format-2 publisher. */
export interface VerifiedHistoricalQualifiedArtifactBytes {
  readonly manifest: Uint8Array;
  readonly hotDownload: Uint8Array;
  readonly historicalDetailsDownload: Uint8Array;
  readonly stats: Uint8Array;
}

export interface CapturedHistoricalQualifiedArtifactBytes
  extends VerifiedHistoricalQualifiedArtifactBytes {
  readonly artifactIndex: Uint8Array;
}

interface CommandResult {
  readonly stdout: string;
  readonly stderr: string;
}

export interface SourceReleaseOutputInput {
  readonly repository: string;
  readonly output: string;
  readonly outputPhysical: string;
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
  readonly zhHans: readonly LocaleGlossEntrySource[];
  readonly morphology: MorphologySource;
  readonly support: AnalyzerSupportSource;
  readonly surfaceTsv: string;
  readonly surfaceSpool: SurfaceIndexTsvSpoolSummary;
  readonly conjugationRules: ConjugationRulePaths;
  readonly projectionSummary: SourceReleaseProjectionSummary;
  readonly sourceSummary: SourceReleaseSemanticSummary;
}

export interface SourceReleaseSemanticSummary {
  readonly mode: 'baseline' | 'update';
  readonly jmdict: { readonly id: string; readonly path: string };
  readonly tomoshi: {
    readonly id: string;
    readonly path: string;
    readonly locale: 'zh-Hans';
    readonly projection: {
      readonly baseEntryCount: number;
      readonly baseSenseCount: number;
      readonly sourceEntryCount: number;
      readonly staleSourceEntryCount: number;
      readonly translatedEntryCount: number;
      readonly fallbackEntryCount: number;
      readonly translatedSenseCount: number;
      readonly fallbackSenseCount: number;
      readonly mismatchedSenseCount: number;
      readonly glossCount: number;
    };
  };
  readonly zhHansSenseInfo: {
    readonly id: string;
    readonly path: string;
    readonly locale: 'zh-Hans';
    readonly projection: {
      readonly catalogTranslationCount: number;
      readonly patternPolicy: string;
      readonly sourceInfoCount: number;
      readonly translatedInfoCount: number;
      readonly catalogTranslatedInfoCount: number;
      readonly patternTranslatedInfoCount: number;
      readonly fallbackInfoCount: number;
      readonly uniqueSourceInfoCount: number;
      readonly translatedUniqueInfoCount: number;
      readonly catalogTranslatedUniqueInfoCount: number;
      readonly patternTranslatedUniqueInfoCount: number;
      readonly unusedTranslationCount: number;
      readonly patternRuleCounts: Readonly<Record<string, number>>;
    };
  };
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

async function surfaceCompiler(repository: string, temporaryDirectory: string): Promise<string> {
  const manifest = join(repository, 'packages/data/tools/surface-index/Cargo.toml');
  const target = join(temporaryDirectory, 'surface-compiler-target');
  await command('cargo', [
    `+${SOURCE_COMPILER_RUST_TOOLCHAIN}`,
    'build',
    '--locked',
    '--release',
    '--manifest-path', manifest,
    '--target-dir', target
  ], repository);
  return join(target, 'release/ichiran-surface-index');
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

export async function captureHistoricalQualifiedArtifactBytes(
  directory: string
): Promise<CapturedHistoricalQualifiedArtifactBytes> {
  const [artifactIndex, manifest, hotDownload, detailsDownload, stats] = await Promise.all([
    readRegularArtifact(directory, 'artifact-sha256.txt'),
    readRegularArtifact(directory, 'manifest.json'),
    readRegularArtifact(directory, 'hot.bin.gz'),
    readRegularArtifact(directory, 'details.bin.gz'),
    readRegularArtifact(directory, 'stats.json')
  ]);
  return {
    artifactIndex: new Uint8Array(artifactIndex),
    manifest: new Uint8Array(manifest),
    hotDownload: new Uint8Array(hotDownload),
    historicalDetailsDownload: new Uint8Array(detailsDownload),
    stats: new Uint8Array(stats)
  };
}

export async function verifyHistoricalQualifiedArtifactIndex(
  directory: string
): Promise<VerifiedHistoricalQualifiedArtifactBytes> {
  const captured = await captureHistoricalQualifiedArtifactBytes(directory);
  const indexBytes = captured.artifactIndex;
  if (sha256Bytes(indexBytes) !== HISTORICAL_QUALIFIED_ARTIFACT_INDEX_SHA256) {
    throw new Error('Qualified artifact checksum index is not the immutable reviewed release index');
  }
  const identities = new Map<string, string>();
  const rows = new TextDecoder().decode(indexBytes).trim().split('\n');
  for (const line of rows) {
    const match = /^([0-9a-f]{64})  ([^/]+)$/.exec(line);
    if (!match) throw new Error(`Invalid qualified artifact checksum row: ${line}`);
    if (identities.has(match[2]!)) {
      throw new Error(`Qualified artifact checksum index duplicates ${match[2]}`);
    }
    identities.set(match[2]!, match[1]!);
  }
  if (rows.length !== QUALIFIED_ARTIFACT_NAMES.length
    || identities.size !== QUALIFIED_ARTIFACT_NAMES.length
    || QUALIFIED_ARTIFACT_NAMES.some(name => !identities.has(name))) {
    throw new Error('Qualified artifact checksum index does not name exactly four release artifacts');
  }
  const artifacts = new Map<string, Uint8Array>([
    ['manifest.json', captured.manifest],
    ['hot.bin.gz', captured.hotDownload],
    ['details.bin.gz', captured.historicalDetailsDownload],
    ['stats.json', captured.stats]
  ]);
  for (const name of QUALIFIED_ARTIFACT_NAMES) {
    const expected = identities.get(name);
    if (!expected) throw new Error(`Qualified artifact checksum index omits ${name}`);
    const actual = sha256Bytes(artifacts.get(name)!);
    if (actual !== expected) throw new Error(`Qualified artifact ${name} is not the reviewed release byte stream`);
  }
  return {
    manifest: captured.manifest,
    hotDownload: captured.hotDownload,
    historicalDetailsDownload: captured.historicalDetailsDownload,
    stats: captured.stats
  };
}

function historicalQualifiedArtifacts(captured: VerifiedHistoricalQualifiedArtifactBytes): {
  readonly bytes: QualifiedArtifactBytes;
  readonly counts: BrowserAlphaArtifactCounts;
} {
  const manifest = JSON.parse(new TextDecoder().decode(captured.manifest)) as {
    readonly formatVersion?: unknown;
    readonly hot: AnalyzerReleaseAsset;
    readonly details: AnalyzerReleaseAsset;
  };
  if (manifest.formatVersion !== 1) {
    throw new Error('Qualified baseline manifest must be the retained format-1 release');
  }
  if (manifest.hot.file !== 'hot.bin.gz' || manifest.details.file !== 'details.bin.gz') {
    throw new Error('Qualified baseline manifest must select the captured release inventory');
  }
  if (manifest.hot.encoding !== 'gzip' || manifest.details.encoding !== 'gzip') {
    throw new Error('Qualified baseline assets must use the reviewed gzip representation');
  }
  const { hotDownload, historicalDetailsDownload: detailsDownload } = captured;
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
  const stats = JSON.parse(new TextDecoder().decode(captured.stats)) as {
    artifacts: BrowserAlphaArtifactCounts;
  };
  return {
    bytes: {
      surfaceIndex: pack.getSection(SURFACE_INDEX_SECTION_ID),
      rootPayload: pack.getSection(ROOT_PAYLOAD_SECTION_ID),
      morphology: pack.getSection(MORPHOLOGY_SECTION_ID),
      analyzerSupport: pack.getSection(ANALYZER_SUPPORT_SECTION_ID),
      analyzerAnnotations: pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)
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
  const qualified = historicalQualifiedArtifacts(
    await verifyHistoricalQualifiedArtifactIndex(baseline.directory)
  );
  const attestationBytes = new Uint8Array(await readFile(baseline.directOrderAttestationPath));
  if (sha256Bytes(attestationBytes) !== ROOT_ORDER_ATTESTATION_SHA256) {
    throw new Error('Direct-root ordering attestation is not the reviewed baseline proof');
  }
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

/** Build, compare the language-neutral analyzer, and atomically publish pack v2. */
export async function writeSourceCompilerRelease(
  input: SourceReleaseOutputInput
): Promise<SourceReleaseOutput> {
  const sections = buildSourceCompilerBinarySections({
    entries: input.entries,
    zhHans: input.zhHans,
    morphology: input.morphology,
    support: input.support,
    conjugationRules: input.conjugationRules
  });
  const rebuiltSections = buildSourceCompilerBinarySections({
    entries: input.entries,
    zhHans: input.zhHans,
    morphology: input.morphology,
    support: input.support,
    conjugationRules: input.conjugationRules
  });
  assertBytesEqual(sections.root.bytes, rebuiltSections.root.bytes, 'Root payload');
  assertBytesEqual(sections.lexicon.bytes, rebuiltSections.lexicon.bytes, 'Lexicon');
  assertBytesEqual(
    sections.locales.en.bytes,
    rebuiltSections.locales.en.bytes,
    'English locale'
  );
  assertBytesEqual(
    sections.locales['zh-Hans'].bytes,
    rebuiltSections.locales['zh-Hans'].bytes,
    'Simplified Chinese locale'
  );
  assertBytesEqual(sections.morphology.bytes, rebuiltSections.morphology.bytes, 'Morphology');
  assertBytesEqual(sections.support.bytes, rebuiltSections.support.bytes, 'Analyzer support');
  assertBytesEqual(sections.annotations.bytes, rebuiltSections.annotations.bytes, 'Annotations');
  const compiler = await surfaceCompiler(input.repository, input.temporaryDirectory);
  const firstPath = join(input.temporaryDirectory, 'surface-first.bin');
  const secondPath = join(input.temporaryDirectory, 'surface-second.bin');
  // Each compiler indexes millions of rows. Run the deterministic rebuilds
  // sequentially so their large Rust working sets never overlap.
  const firstStats = await compileSurface(compiler, input.surfaceTsv, firstPath, input.repository);
  const secondStats = await compileSurface(compiler, input.surfaceTsv, secondPath, input.repository);
  if (JSON.stringify(firstStats) !== JSON.stringify(secondStats)) {
    throw new Error('Surface-index rebuild changed deterministic counts');
  }
  if (input.surfaceSpool.surfaces !== firstStats.input) {
    throw new Error(
      `Rust surface compiler read ${firstStats.input}/${input.surfaceSpool.surfaces} surfaces`
    );
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
    analyzerAnnotations: sections.annotations.bytes
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
    lexicon: sections.lexicon.bytes,
    locales: {
      en: sections.locales.en.bytes,
      'zh-Hans': sections.locales['zh-Hans'].bytes
    },
    hotEncoding: 'gzip',
    lexiconEncoding: 'gzip',
    localeEncodings: { en: 'gzip', 'zh-Hans': 'gzip' }
  } as const;
  const release = buildAnalyzerRelease(releaseOptions);
  const releaseSize = assertAnalyzerReleaseSize(release);
  const rebuilt = buildAnalyzerRelease(releaseOptions);
  assertBytesEqual(release.hotDownload, rebuilt.hotDownload, 'Compressed hot asset');
  assertBytesEqual(release.lexiconDownload, rebuilt.lexiconDownload, 'Compressed lexicon asset');
  for (const locale of ['en', 'zh-Hans'] as const) {
    assertBytesEqual(
      release.localeDownloads[locale]!,
      rebuilt.localeDownloads[locale]!,
      `Compressed ${locale} locale asset`
    );
  }
  assertBytesEqual(release.manifestBytes, rebuilt.manifestBytes, 'Release manifest');
  const reportValue = {
    formatVersion: 2,
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
    surfaceSpool: input.surfaceSpool,
    projection: input.projectionSummary,
    artifacts: counts,
    artifactIdentities: {
      ...artifactIdentities(sourceArtifacts),
      lexicon: {
        bytes: sections.lexicon.bytes.byteLength,
        sha256: sha256Bytes(sections.lexicon.bytes)
      },
      locales: Object.fromEntries(Object.entries(sections.locales).map(([locale, build]) => [
        locale,
        { bytes: build.bytes.byteLength, sha256: sha256Bytes(build.bytes) }
      ]))
    },
    qualifiedComparison,
    pack: { bytes: hot.byteLength, sha256: sha256Bytes(hot) },
    release: { manifest: release.manifest, size: releaseSize }
  };
  const report = deterministicJson(reportValue);
  assertBytesEqual(report, deterministicJson(reportValue), 'Release stats');
  const files = new Map<string, Uint8Array>([
    [release.manifest.hot.file, release.hotDownload],
    [release.manifest.lexicon.file, release.lexiconDownload],
    ...Object.entries(release.manifest.locales).map(([locale, asset]) => [
      asset.file,
      release.localeDownloads[locale]!
    ] as [string, Uint8Array]),
    ['manifest.json', release.manifestBytes],
    ['stats.json', report]
  ]);
  const generation = await publishAnalyzerRelease(input.output, files, {
    beforeWrite: async () => {
      await assertSourceReleaseDestination(input.repository, {
        lexical: input.output,
        physical: input.outputPhysical
      });
    },
    verify: async directory => {
      await assertExactReleaseInventory(directory, [...files.keys()]);
      const publishedHot = new Uint8Array(
        gunzipSync(await readFile(join(directory, release.manifest.hot.file)))
      );
      const publishedLexicon = new Uint8Array(
        gunzipSync(await readFile(join(directory, release.manifest.lexicon.file)))
      );
      openPack(publishedHot).verifyAll();
      assertBytesEqual(publishedHot, hot, 'Published hot pack');
      assertBytesEqual(publishedLexicon, sections.lexicon.bytes, 'Published lexicon');
      for (const locale of ['en', 'zh-Hans'] as const) {
        const asset = release.manifest.locales[locale]!;
        const published = new Uint8Array(gunzipSync(await readFile(join(directory, asset.file))));
        assertBytesEqual(published, sections.locales[locale].bytes, `Published ${locale} locale`);
      }
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
