import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';

import {
  ANALYZER_PERSISTED_MAX_BYTES,
  ANALYZER_RELEASE_FORMAT_VERSION,
  analyzerReadyStateSize,
  analyzerManifestDigestInput,
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseEncoding,
  type AnalyzerReleaseManifest
} from '@ichiran/core/compiler';

export {
  ANALYZER_PERSISTED_MAX_BYTES,
  ANALYZER_RELEASE_FORMAT_VERSION,
  analyzerManifestDigestInput,
  parseAnalyzerReleaseManifest
} from '@ichiran/core/compiler';
export type {
  AnalyzerReleaseAsset,
  AnalyzerReleaseEncoding,
  AnalyzerReleaseManifest
} from '@ichiran/core/compiler';

export const ANALYZER_HOT_MAX_BYTES = 25 * 1024 * 1024;
export const ANALYZER_WIRE_MAX_BYTES = 26 * 1024 * 1024;

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
  readonly installedIdentityPayloadBytes: number;
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
  encoding: AnalyzerReleaseEncoding
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

export function buildAnalyzerRelease(options: {
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly hot: Uint8Array;
  readonly details: Uint8Array;
  readonly hotEncoding?: AnalyzerReleaseEncoding;
  readonly detailsEncoding?: AnalyzerReleaseEncoding;
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
  parseAnalyzerReleaseManifest(
    manifest,
    text => createHash('sha256').update(text).digest('hex')
  );
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
  // manifest.json alongside that shell, OPFS stores one compact copy inside
  // the active slot's install-{a,b}.json, and IndexedDB stores the install ID read
  // on every request. The inactive slot exists only while staging an upgrade, so it
  // is not part of the ready-state persisted total. Browser-managed IndexedDB
  // allocation overhead is implementation-defined and is not part of this gate.
  const readyState = analyzerReadyStateSize(
    build.manifest,
    build.manifestBytes.byteLength,
    shellBytes
  );
  const report = {
    hotBytes: build.manifest.hot.installedBytes,
    persistedBytes: readyState.persistedBytes,
    wireBytes:
      build.hotDownload.byteLength
      + build.detailsDownload.byteLength
      + build.manifestBytes.byteLength
      + shellBytes,
    shellBytes,
    cachedManifestBytes: readyState.cachedManifestBytes,
    installedMarkerBytes: readyState.installedMarkerBytes,
    installedIdentityPayloadBytes: readyState.installedIdentityPayloadBytes
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
