export const ANALYZER_RELEASE_FORMAT_VERSION = 2;
export const ANALYZER_PACK_VERSION_MAX_UTF8_BYTES = 128;
export const ANALYZER_PERSISTED_MAX_BYTES = 64 * 1024 * 1024;
const ANALYZER_INSTALL_ID_BYTES = 36;

export type AnalyzerReleaseEncoding = 'identity' | 'gzip';

export interface AnalyzerReleaseAsset {
  readonly file: string;
  readonly encoding: AnalyzerReleaseEncoding;
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

export interface AnalyzerReleaseManifest {
  readonly formatVersion: 2;
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly manifestSha256: string;
  readonly hot: AnalyzerReleaseAsset;
  readonly lexicon: AnalyzerReleaseAsset;
  readonly locales: Readonly<Record<string, AnalyzerReleaseAsset>>;
}

export interface AnalyzerReadyStateSize {
  readonly persistedBytes: number;
  readonly installedMarkerBytes: number;
  readonly installedIdentityPayloadBytes: number;
}

export type AnalyzerReleaseManifestWithoutDigest = Omit<
  AnalyzerReleaseManifest,
  'manifestSha256'
>;

export type AnalyzerReleaseSha256 = (utf8Text: string) => string;

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function expectSha256(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${label} must be a lowercase SHA-256`);
  }
}

// TextEncoder is not available in every runtime that consumes @ichiran/core.
// This follows its replacement behavior for lone UTF-16 surrogates.
function utf8ByteLength(value: string): number {
  let bytes = 0;
  for (let index = 0; index < value.length; index++) {
    const code = value.charCodeAt(index);
    if (code <= 0x7f) bytes += 1;
    else if (code <= 0x7ff) bytes += 2;
    else if (
      code >= 0xd800 && code <= 0xdbff
      && index + 1 < value.length
      && value.charCodeAt(index + 1) >= 0xdc00
      && value.charCodeAt(index + 1) <= 0xdfff
    ) {
      bytes += 4;
      index += 1;
    } else bytes += 3;
  }
  return bytes;
}

/** Exact ready-state payload accounted against the browser persistence budget. */
export function analyzerReadyStateSize(
  manifest: AnalyzerReleaseManifest
): AnalyzerReadyStateSize {
  const installedMarkerBytes = utf8ByteLength(JSON.stringify({
    state: 'ready',
    manifest,
    installId: '00000000-0000-4000-8000-000000000000',
    installedAt: '1970-01-01T00:00:00.000Z',
    slot: 'a'
  }));
  const persistedBytes = manifest.hot.installedBytes
    + manifest.lexicon.installedBytes
    + Object.values(manifest.locales).reduce(
      (total, asset) => total + asset.installedBytes,
      0
    )
    + installedMarkerBytes
    + ANALYZER_INSTALL_ID_BYTES;
  if (!Number.isSafeInteger(persistedBytes)) {
    throw new Error('Ready-state size exceeds the safe integer range');
  }
  return {
    persistedBytes,
    installedMarkerBytes,
    installedIdentityPayloadBytes: ANALYZER_INSTALL_ID_BYTES
  };
}

function expectExactKeys(
  value: Record<string, unknown>,
  keys: readonly string[],
  label: string
): void {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (actual.join('\n') !== expected.join('\n')) {
    throw new Error(`${label} has unsupported fields: ${actual.join(', ')}`);
  }
}

function expectedAssetFile(name: 'hot' | 'lexicon' | string): string {
  if (name !== 'hot' && name !== 'lexicon') return `gloss.${name}.bin`;
  return `${name}.bin`;
}

function parseAsset(
  value: unknown,
  name: 'hot' | 'lexicon' | string
): AnalyzerReleaseAsset {
  if (!isObject(value)) throw new Error(`Analyzer manifest is missing ${name}`);
  expectExactKeys(value, [
    'file',
    'encoding',
    'downloadBytes',
    'downloadSha256',
    'installedBytes',
    'installedSha256'
  ], `Analyzer manifest ${name}`);
  if (value.encoding !== 'identity' && value.encoding !== 'gzip') {
    throw new Error(`Analyzer manifest has an invalid ${name} encoding`);
  }
  const expectedFile = `${expectedAssetFile(name)}${value.encoding === 'gzip' ? '.gz' : ''}`;
  if (value.file !== expectedFile) {
    throw new Error(`Analyzer manifest ${name}.file must be ${expectedFile}`);
  }
  for (const field of ['downloadBytes', 'installedBytes'] as const) {
    if (!Number.isSafeInteger(value[field]) || (value[field] as number) <= 0) {
      throw new Error(`Analyzer manifest ${name}.${field} must be a positive integer`);
    }
  }
  expectSha256(value.downloadSha256, `Analyzer manifest ${name}.downloadSha256`);
  expectSha256(value.installedSha256, `Analyzer manifest ${name}.installedSha256`);
  if (
    value.encoding === 'identity'
    && (
      value.downloadBytes !== value.installedBytes
      || value.downloadSha256 !== value.installedSha256
    )
  ) {
    throw new Error(`Analyzer manifest ${name} identity sizes and digests must match`);
  }
  return {
    file: value.file,
    encoding: value.encoding,
    downloadBytes: value.downloadBytes as number,
    downloadSha256: value.downloadSha256,
    installedBytes: value.installedBytes as number,
    installedSha256: value.installedSha256
  };
}

/** Exact compact JSON used to authenticate every analyzer release manifest. */
export function analyzerManifestDigestInput(
  manifest: AnalyzerReleaseManifestWithoutDigest
): string {
  const locales: Record<string, AnalyzerReleaseAsset> = {};
  for (const locale of Object.keys(manifest.locales).sort()) {
    const asset = manifest.locales[locale]!;
    locales[locale] = {
      file: asset.file,
      encoding: asset.encoding,
      downloadBytes: asset.downloadBytes,
      downloadSha256: asset.downloadSha256,
      installedBytes: asset.installedBytes,
      installedSha256: asset.installedSha256
    };
  }
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
    lexicon: {
      file: manifest.lexicon.file,
      encoding: manifest.lexicon.encoding,
      downloadBytes: manifest.lexicon.downloadBytes,
      downloadSha256: manifest.lexicon.downloadSha256,
      installedBytes: manifest.lexicon.installedBytes,
      installedSha256: manifest.lexicon.installedSha256
    },
    locales
  });
}

/** Parse and authenticate a manifest with the host's SHA-256 implementation. */
export function parseAnalyzerReleaseManifest(
  value: unknown,
  sha256: AnalyzerReleaseSha256
): AnalyzerReleaseManifest {
  if (!isObject(value)) throw new Error('Analyzer manifest must be an object');
  expectExactKeys(value, [
    'formatVersion',
    'packVersion',
    'sourceCommit',
    'sourcesLockSha256',
    'manifestSha256',
    'hot',
    'lexicon',
    'locales'
  ], 'Analyzer manifest');
  if (value.formatVersion !== ANALYZER_RELEASE_FORMAT_VERSION) {
    throw new Error('Analyzer manifest has an unsupported format');
  }
  if (typeof value.packVersion !== 'string' || value.packVersion.length === 0) {
    throw new Error('Analyzer manifest packVersion must not be empty');
  }
  if (utf8ByteLength(value.packVersion) > ANALYZER_PACK_VERSION_MAX_UTF8_BYTES) {
    throw new Error(
      `Analyzer manifest packVersion exceeds ${ANALYZER_PACK_VERSION_MAX_UTF8_BYTES} UTF-8 bytes`
    );
  }
  if (typeof value.sourceCommit !== 'string' || !/^[0-9a-f]{40}$/.test(value.sourceCommit)) {
    throw new Error('Analyzer manifest sourceCommit must be a full lowercase Git object ID');
  }
  expectSha256(value.sourcesLockSha256, 'Analyzer manifest sourcesLockSha256');
  expectSha256(value.manifestSha256, 'Analyzer manifest manifestSha256');
  if (!isObject(value.locales)) throw new Error('Analyzer manifest is missing locales');
  if (value.locales.en === undefined) throw new Error('Analyzer manifest locales must include en');
  if (value.locales['zh-Hans'] === undefined) {
    throw new Error('Analyzer manifest locales must include zh-Hans');
  }
  const locales: Record<string, AnalyzerReleaseAsset> = {};
  for (const locale of Object.keys(value.locales).sort()) {
    if (!/^[A-Za-z]{2,3}(?:-[A-Za-z0-9]{2,8})*$/.test(locale)) {
      throw new Error(`Analyzer manifest has an invalid locale ${locale}`);
    }
    locales[locale] = parseAsset(value.locales[locale], locale);
  }
  const manifest: AnalyzerReleaseManifest = {
    formatVersion: ANALYZER_RELEASE_FORMAT_VERSION,
    packVersion: value.packVersion,
    sourceCommit: value.sourceCommit,
    sourcesLockSha256: value.sourcesLockSha256,
    manifestSha256: value.manifestSha256,
    hot: parseAsset(value.hot, 'hot'),
    lexicon: parseAsset(value.lexicon, 'lexicon'),
    locales
  };
  const { manifestSha256: _manifestSha256, ...unsigned } = manifest;
  const digest = sha256(analyzerManifestDigestInput(unsigned));
  if (digest !== manifest.manifestSha256) {
    throw new Error('Analyzer manifest checksum does not match');
  }
  return manifest;
}
