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
export const ANALYZER_WIRE_MAX_BYTES = 36 * 1024 * 1024;

export interface AnalyzerReleaseBuild {
  readonly manifest: AnalyzerReleaseManifest;
  readonly manifestBytes: Uint8Array;
  readonly hotDownload: Uint8Array;
  readonly lexiconDownload: Uint8Array;
  readonly localeDownloads: Readonly<Record<string, Uint8Array>>;
}

export interface AnalyzerReleaseSizeReport {
  readonly hotBytes: number;
  readonly persistedBytes: number;
  readonly wireBytes: number;
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
  readonly lexicon: Uint8Array;
  readonly locales: Readonly<Record<string, Uint8Array>>;
  readonly hotEncoding?: AnalyzerReleaseEncoding;
  readonly lexiconEncoding?: AnalyzerReleaseEncoding;
  readonly localeEncodings?: Readonly<Record<string, AnalyzerReleaseEncoding>>;
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
  const lexicon = releaseAsset(
    options.lexiconEncoding === 'identity' ? 'lexicon.bin' : 'lexicon.bin.gz',
    options.lexicon,
    options.lexiconEncoding ?? 'gzip'
  );
  if (options.locales.en === undefined) throw new Error('Release locales must include en');
  if (options.locales['zh-Hans'] === undefined) {
    throw new Error('Release locales must include zh-Hans');
  }
  const locale = (name: string) => {
    const encoding = options.localeEncodings?.[name] ?? 'gzip';
    return releaseAsset(
      `gloss.${name}.bin${encoding === 'gzip' ? '.gz' : ''}`,
      options.locales[name],
      encoding
    );
  };
  const localeBuilds = Object.fromEntries(
    Object.keys(options.locales).sort().map(name => [name, locale(name)])
  );
  const unsigned = {
    formatVersion: ANALYZER_RELEASE_FORMAT_VERSION,
    packVersion: options.packVersion,
    sourceCommit: options.sourceCommit,
    sourcesLockSha256: options.sourcesLockSha256,
    hot: hot.manifest,
    lexicon: lexicon.manifest,
    locales: Object.fromEntries(
      Object.entries(localeBuilds).map(([name, build]) => [name, build.manifest])
    )
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
    lexiconDownload: lexicon.download,
    localeDownloads: Object.fromEntries(
      Object.entries(localeBuilds).map(([name, build]) => [name, build.download])
    )
  };
}

export function assertAnalyzerReleaseSize(
  build: AnalyzerReleaseBuild
): AnalyzerReleaseSizeReport {
  // OPFS stores the two pack assets and one compact install marker. IndexedDB
  // stores the install ID read on every request. HTTP/application-shell caches
  // belong to the consumer and are deliberately outside this release contract.
  const readyState = analyzerReadyStateSize(build.manifest);
  const report = {
    hotBytes: build.manifest.hot.installedBytes,
    persistedBytes: readyState.persistedBytes,
    wireBytes:
      build.hotDownload.byteLength
      + build.lexiconDownload.byteLength
      + Object.values(build.localeDownloads).reduce((total, bytes) => total + bytes.byteLength, 0)
      + build.manifestBytes.byteLength,
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
