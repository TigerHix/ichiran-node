const ARTIFACT_ERROR_NAMES = new Set([
  'RustKernelError',
  'PackFormatError',
  'SurfaceIndexFormatError',
  'RootPayloadFormatError',
  'AnalyzerSupportFormatError',
  'AnalyzerAnnotationsError',
  'DetailStoreError'
]);

const ARTIFACT_CORRUPTION_CODES = new Set([
  'invalid-header',
  'unsupported-version',
  'invalid-directory',
  'invalid-states',
  'invalid-edges',
  'corrupt-section',
  'corrupt-payload',
  'corrupt-index',
  'corrupt-block',
  'missing-section'
]);

/** True only for explicit invalidity in immutable installed analyzer bytes. */
export function isArtifactCorruption(error: unknown): boolean {
  if (!(error instanceof Error)) return false;
  const code = (error as Error & { readonly code?: unknown }).code;
  if (error.name === 'AnalyzerError' && code === 'invalid-pack') return true;
  if (!ARTIFACT_ERROR_NAMES.has(error.name)) return false;
  return typeof code === 'string' && ARTIFACT_CORRUPTION_CODES.has(code);
}
