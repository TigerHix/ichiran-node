import { resolve } from 'node:path';
import { verifyAnalyzerRelease } from './release-files.js';

const directory = process.argv[2];
let sourceLock: string | undefined;
for (let index = 3; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument !== '--source-lock') throw new Error(`Unknown verification argument: ${argument}`);
  sourceLock = process.argv[++index];
  if (!sourceLock) throw new Error('--source-lock requires a file');
}
if (!directory) {
  throw new Error(
    'Usage: bun scripts/verify-release.ts <release-directory> [--source-lock <file>]'
  );
}
if (process.env.ICHIRAN_QUALIFIED_ARTIFACT !== undefined) {
  throw new Error('Source release verification does not accept ICHIRAN_QUALIFIED_ARTIFACT');
}

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const release = await verifyAnalyzerRelease(
  resolve(directory),
  repositoryRoot,
  { sourceLock }
);
console.error(`Verified analyzer release ${release.manifest.packVersion}`);
