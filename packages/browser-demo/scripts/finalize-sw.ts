import { createHash } from 'node:crypto';
import { readdir, readFile, writeFile } from 'node:fs/promises';
import { join, relative, sep } from 'node:path';

const output = join(import.meta.dir, '..', 'dist');

async function files(directory: string): Promise<string[]> {
  const result: string[] = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await files(path));
    else result.push(path);
  }
  return result;
}

const outputFiles = await files(output);
const precache = outputFiles
  .map(path => relative(output, path).split(sep).join('/'))
  .filter(path => path !== 'sw.js' && !path.startsWith('analyzer/'))
  .map(path => `/${path}`)
  .sort();

// Keep the shell byte count identical before and after the release pack is staged.
// A production deployment always contains this file.
precache.push('/analyzer/manifest.json');

const version = createHash('sha256');
for (const path of precache) {
  version.update(path).update('\0');
  const source = outputFiles.find(value => relative(output, value).split(sep).join('/') === path.slice(1));
  // The first shell-only measurement intentionally runs before analyzer assets
  // are staged. Keep a deterministic fixed-width version in that build; the
  // deployable post-stage build hashes the real manifest bytes.
  version.update(source ? await readFile(source) : '<not-staged>');
}
const cacheVersion = version.digest('hex').slice(0, 16);

const serviceWorkerPath = join(output, 'sw.js');
const serviceWorker = await readFile(serviceWorkerPath, 'utf8');
const marker = '/*__PRECACHE__*/[]';
if (!serviceWorker.includes(marker)) throw new Error('Service Worker precache marker is missing');
const cacheMarker = '__CACHE_VERSION__';
if (!serviceWorker.includes(cacheMarker)) throw new Error('Service Worker cache-version marker is missing');
await writeFile(
  serviceWorkerPath,
  serviceWorker
    .replace(marker, JSON.stringify(precache))
    .replace(cacheMarker, cacheVersion)
);
