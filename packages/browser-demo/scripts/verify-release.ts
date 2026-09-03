import { resolve } from 'node:path';
import { verifyAnalyzerRelease } from './release-files.js';

const directory = process.argv[2];
if (!directory || process.argv.length > 3) {
  throw new Error('Usage: bun scripts/verify-release.ts <release-directory>');
}
if (process.env.ICHIRAN_QUALIFIED_ARTIFACT !== undefined) {
  throw new Error('Source release verification does not accept ICHIRAN_QUALIFIED_ARTIFACT');
}

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const release = await verifyAnalyzerRelease(
  resolve(directory),
  repositoryRoot
);
console.error(`Verified analyzer release ${release.manifest.packVersion}`);
