import { createHash } from 'node:crypto';
import { readdir, readFile } from 'node:fs/promises';
import { join, relative, resolve, sep } from 'node:path';

export const PRODUCTION_SHELL_MEASUREMENT_VERSION = 1;

export interface ProductionShellMeasurement {
  readonly formatVersion: typeof PRODUCTION_SHELL_MEASUREMENT_VERSION;
  readonly bytes: number;
  readonly sha256: string;
  readonly fileCount: number;
  readonly cacheVersion: string;
}

async function files(directory: string): Promise<string[]> {
  const result: string[] = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await files(path));
    else if (entry.isFile()) result.push(path);
    else throw new Error(`Production shell contains a non-file entry: ${path}`);
  }
  return result;
}

function cacheVersion(
  precache: readonly string[],
  shell: ReadonlyMap<string, Uint8Array>,
  manifestBytes: Uint8Array
): string {
  const hash = createHash('sha256');
  for (const path of precache) {
    hash.update(path).update('\0');
    const bytes = path === '/analyzer/manifest.json'
      ? manifestBytes
      : shell.get(path.slice(1));
    if (!bytes) throw new Error(`Service Worker precache names a missing shell file: ${path}`);
    hash.update(bytes);
  }
  return hash.digest('hex').slice(0, 16);
}

/**
 * Measure the exact production shell. A pre-stage build is projected to the
 * deterministic cache version for `manifestBytes`; verification can require
 * that the on-disk Service Worker is already that final projection.
 */
export async function measureProductionShell(
  directory: string,
  manifestBytes: Uint8Array,
  options: { readonly requireFinalizedServiceWorker?: boolean } = {}
): Promise<ProductionShellMeasurement> {
  const root = resolve(directory);
  const paths = (await files(root))
    .map(path => ({ path, name: relative(root, path).split(sep).join('/') }))
    .filter(value => !value.name.startsWith('analyzer/'))
    .sort((left, right) => left.name.localeCompare(right.name));
  const shell = new Map<string, Uint8Array>();
  for (const value of paths) shell.set(value.name, new Uint8Array(await readFile(value.path)));

  const serviceWorkerBytes = shell.get('sw.js');
  if (!serviceWorkerBytes) throw new Error('Production shell is missing sw.js');
  const serviceWorker = new TextDecoder().decode(serviceWorkerBytes);
  const cacheMatch = /const CACHE = 'ichiran-shell-([0-9a-f]{16})';/.exec(serviceWorker);
  if (!cacheMatch) throw new Error('Production Service Worker has no finalized cache version');
  const coreMatch = /const CORE = (\[[^;]*\]);/.exec(serviceWorker);
  if (!coreMatch) throw new Error('Production Service Worker has no finalized precache');
  let actualPrecache: unknown;
  try {
    actualPrecache = JSON.parse(coreMatch[1]!);
  } catch {
    throw new Error('Production Service Worker precache is not valid JSON');
  }
  const expectedPrecache = paths
    .map(value => value.name)
    .filter(name => name !== 'sw.js')
    .map(name => `/${name}`)
    .sort();
  expectedPrecache.push('/analyzer/manifest.json');
  if (
    !Array.isArray(actualPrecache)
    || actualPrecache.some(value => typeof value !== 'string')
    || JSON.stringify(actualPrecache) !== JSON.stringify(expectedPrecache)
  ) {
    throw new Error('Production Service Worker precache does not match the shell inventory');
  }
  const expectedCacheVersion = cacheVersion(expectedPrecache, shell, manifestBytes);
  if (
    options.requireFinalizedServiceWorker
    && cacheMatch[1] !== expectedCacheVersion
  ) {
    throw new Error(
      `Production Service Worker cache version ${cacheMatch[1]} does not match release ${expectedCacheVersion}`
    );
  }

  const projectedServiceWorker = new TextEncoder().encode(
    serviceWorker.replace(cacheMatch[1]!, expectedCacheVersion)
  );
  const digest = createHash('sha256');
  let bytes = 0;
  for (const value of paths) {
    const raw = shell.get(value.name)!;
    const measured = value.name === 'sw.js' ? projectedServiceWorker : raw;
    bytes += raw.byteLength;
    digest.update(value.name).update('\0').update(String(measured.byteLength)).update('\0');
    digest.update(measured);
  }
  return {
    formatVersion: PRODUCTION_SHELL_MEASUREMENT_VERSION,
    bytes,
    sha256: digest.digest('hex'),
    fileCount: paths.length,
    cacheVersion: expectedCacheVersion
  };
}
