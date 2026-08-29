import { copyFile, mkdir, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { verifyAnalyzerRelease } from './release-files.js';

const packageRoot = resolve(import.meta.dir, '..');
const repositoryRoot = resolve(packageRoot, '..', '..');
const source = resolve(process.argv[2] ?? join(repositoryRoot, 'dist', 'browser-alpha'));
const target = join(packageRoot, 'public', 'analyzer');
const { manifest } = await verifyAnalyzerRelease(source, repositoryRoot);

await rm(target, { recursive: true, force: true });
await mkdir(target, { recursive: true });
for (const file of ['manifest.json', manifest.hot.file, manifest.details.file]) {
  await copyFile(join(source, file), join(target, file));
}

console.log(`Staged ${manifest.hot.file}, ${manifest.details.file}, and manifest.json`);
