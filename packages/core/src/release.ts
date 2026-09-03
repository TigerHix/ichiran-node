/** Release-manifest contract shared by trusted browser and Node pack loaders. */
export {
  ANALYZER_PACK_VERSION_MAX_UTF8_BYTES,
  ANALYZER_RELEASE_FORMAT_VERSION,
  analyzerManifestDigestInput,
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseAsset,
  type AnalyzerReleaseEncoding,
  type AnalyzerReleaseManifest,
  type AnalyzerReleaseManifestWithoutDigest,
  type AnalyzerReleaseSha256
} from './release-manifest.js';
