import { copyFile, mkdir, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { verifyAnalyzerRelease } from './release-files.js';

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const source = resolve(process.argv[2] ?? join(repositoryRoot, 'dist', 'browser-alpha'));
let sourceLock: string | undefined;
for (let index = 3; index < process.argv.length; index++) {
  const argument = process.argv[index];
  if (argument !== '--source-lock') throw new Error(`Unknown staging argument: ${argument}`);
  sourceLock = process.argv[++index];
  if (!sourceLock) throw new Error('--source-lock requires a file');
}
const target = join(packageRoot, 'public', 'analyzer');
const { manifest } = await verifyAnalyzerRelease(
  source,
  repositoryRoot,
  {
    qualifiedArtifact: process.env.ICHIRAN_QUALIFIED_ARTIFACT,
    sourceLock
  }
);

await rm(target, { recursive: true, force: true });
await mkdir(target, { recursive: true });
for (const file of ['manifest.json', manifest.hot.file, manifest.details.file]) {
  await copyFile(join(source, file), join(target, file));
}

console.log(`Staged ${manifest.hot.file}, ${manifest.details.file}, and manifest.json`);
