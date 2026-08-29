import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';

export const ANALYZER_RELEASE_FORMAT_VERSION = 1;
export const ANALYZER_HOT_MAX_BYTES = 24 * 1024 * 1024;
export const ANALYZER_PERSISTED_MAX_BYTES = 64 * 1024 * 1024;
export const ANALYZER_WIRE_MAX_BYTES = 25 * 1024 * 1024;

export interface AnalyzerReleaseAsset {
  readonly file: string;
  readonly encoding: 'identity' | 'gzip';
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

export interface AnalyzerReleaseManifest {
  readonly formatVersion: 1;
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly manifestSha256: string;
  readonly hot: AnalyzerReleaseAsset;
  readonly details: AnalyzerReleaseAsset;
}

export interface AnalyzerReleaseBuild {
  readonly manifest: AnalyzerReleaseManifest;
  readonly manifestBytes: Uint8Array;
  readonly hotDownload: Uint8Array;
  readonly detailsDownload: Uint8Array;
}

export interface AnalyzerReleaseSizeReport {
  readonly hotBytes: number;
  readonly persistedBytes: number;
  readonly wireBytes: number;
  readonly shellBytes: number;
  readonly cachedManifestBytes: number;
  readonly installedMarkerBytes: number;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function nonEmpty(value: string, label: string): void {
  if (value.length === 0) throw new Error(`${label} must not be empty`);
}

function exactSha256(value: string, label: string): void {
  if (!/^[0-9a-f]{64}$/.test(value)) throw new Error(`${label} must be a lowercase SHA-256`);
}

function releaseAsset(
  file: string,
  installed: Uint8Array,
  encoding: 'identity' | 'gzip'
): { readonly manifest: AnalyzerReleaseAsset; readonly download: Uint8Array } {
  nonEmpty(file, 'Release asset filename');
  if (installed.byteLength === 0) throw new Error(`${file} must not be empty`);
  const download = encoding === 'gzip'
    ? new Uint8Array(gzipSync(installed, { level: 9 }))
    : installed.slice();
  return {
    manifest: {
      file,
      encoding,
      downloadBytes: download.byteLength,
      downloadSha256: sha256(download),
      installedBytes: installed.byteLength,
      installedSha256: sha256(installed)
    },
    download
  };
}

/** Exact compact JSON hashed by both the compiler and browser installer. */
export function analyzerManifestDigestInput(
  manifest: Omit<AnalyzerReleaseManifest, 'manifestSha256'>
): string {
  return JSON.stringify({
    formatVersion: manifest.formatVersion,
    packVersion: manifest.packVersion,
    sourceCommit: manifest.sourceCommit,
    sourcesLockSha256: manifest.sourcesLockSha256,
    hot: {
      file: manifest.hot.file,
      encoding: manifest.hot.encoding,
      downloadBytes: manifest.hot.downloadBytes,
      downloadSha256: manifest.hot.downloadSha256,
      installedBytes: manifest.hot.installedBytes,
      installedSha256: manifest.hot.installedSha256
    },
    details: {
      file: manifest.details.file,
      encoding: manifest.details.encoding,
      downloadBytes: manifest.details.downloadBytes,
      downloadSha256: manifest.details.downloadSha256,
      installedBytes: manifest.details.installedBytes,
      installedSha256: manifest.details.installedSha256
    }
  });
}

export function buildAnalyzerRelease(options: {
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly hot: Uint8Array;
  readonly details: Uint8Array;
  readonly hotEncoding?: 'identity' | 'gzip';
  readonly detailsEncoding?: 'identity' | 'gzip';
}): AnalyzerReleaseBuild {
  nonEmpty(options.packVersion, 'Pack version');
  if (!/^[0-9a-f]{40}$/.test(options.sourceCommit)) {
    throw new Error('Source commit must be a full lowercase Git object ID');
  }
  exactSha256(options.sourcesLockSha256, 'Sources lock digest');

  const hot = releaseAsset(
    options.hotEncoding === 'identity' ? 'hot.bin' : 'hot.bin.gz',
    options.hot,
    options.hotEncoding ?? 'gzip'
  );
  const details = releaseAsset(
    options.detailsEncoding === 'identity' ? 'details.bin' : 'details.bin.gz',
    options.details,
    options.detailsEncoding ?? 'gzip'
  );
  const unsigned = {
    formatVersion: ANALYZER_RELEASE_FORMAT_VERSION,
    packVersion: options.packVersion,
    sourceCommit: options.sourceCommit,
    sourcesLockSha256: options.sourcesLockSha256,
    hot: hot.manifest,
    details: details.manifest
  } as const;
  const manifest: AnalyzerReleaseManifest = {
    ...unsigned,
    manifestSha256: sha256(new TextEncoder().encode(analyzerManifestDigestInput(unsigned)))
  };
  return {
    manifest,
    manifestBytes: new TextEncoder().encode(`${JSON.stringify(manifest, null, 2)}\n`),
    hotDownload: hot.download,
    detailsDownload: details.download
  };
}

export function assertAnalyzerReleaseSize(
  build: AnalyzerReleaseBuild,
  shellBytes = 0
): AnalyzerReleaseSizeReport {
  if (!Number.isSafeInteger(shellBytes) || shellBytes < 0) {
    throw new Error('Shell size must be a non-negative integer');
  }
  // measure-shell deliberately excludes analyzer/*. The Service Worker caches
  // manifest.json alongside that shell, and OPFS stores one compact copy inside
  // install.json. Count both so persistedBytes describes every installed payload.
  const cachedManifestBytes = build.manifestBytes.byteLength;
  const installedMarkerBytes = new TextEncoder().encode(JSON.stringify({
    state: 'ready',
    manifest: build.manifest,
    installedAt: '1970-01-01T00:00:00.000Z'
  })).byteLength;
  const report = {
    hotBytes: build.manifest.hot.installedBytes,
    persistedBytes:
      build.manifest.hot.installedBytes
      + build.manifest.details.installedBytes
      + shellBytes
      + cachedManifestBytes
      + installedMarkerBytes,
    wireBytes:
      build.hotDownload.byteLength
      + build.detailsDownload.byteLength
      + build.manifestBytes.byteLength
      + shellBytes,
    shellBytes,
    cachedManifestBytes,
    installedMarkerBytes
  };
  if (report.hotBytes > ANALYZER_HOT_MAX_BYTES) {
    throw new Error(`hot.bin is ${report.hotBytes} bytes; limit is ${ANALYZER_HOT_MAX_BYTES}`);
  }
  if (report.persistedBytes > ANALYZER_PERSISTED_MAX_BYTES) {
    throw new Error(
      `Persisted release is ${report.persistedBytes} bytes; limit is ${ANALYZER_PERSISTED_MAX_BYTES}`
    );
  }
  if (report.wireBytes > ANALYZER_WIRE_MAX_BYTES) {
    throw new Error(`Release transfer is ${report.wireBytes} bytes; limit is ${ANALYZER_WIRE_MAX_BYTES}`);
  }
  return report;
}
