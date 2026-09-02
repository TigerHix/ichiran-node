import { describe, expect, test } from 'bun:test';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { downloadDataFile, getDataPath } from '../src/data/download.js';

async function sourceLockWithPaths(
  pathForRole: (role: string) => string
): Promise<Record<string, unknown>> {
  const lock = JSON.parse(await readFile(
    join(import.meta.dir, '../../../data/source-compiler-sources.lock.json'),
    'utf8'
  )) as { sources: Array<Record<string, unknown>> };
  for (const source of lock.sources) {
    if (typeof source.role === 'string') {
      source.pinnedPath = pathForRole(source.role);
    } else if (Array.isArray(source.roles)) {
      source.pinnedPaths = source.roles.map(role => pathForRole(String(role)));
    }
  }
  return lock;
}

describe('legacy data downloader safety', () => {
  test('uses the ignored live-data area instead of compiler pins', () => {
    expect(getDataPath('jmdict')).toEndWith('/work/live-data/JMdict_e.gz');
    expect(getDataPath('kanjidic')).toEndWith('/work/live-data/kanjidic2.xml.gz');
  });

  test('force cannot overwrite a role selected by an alternate source lock', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-download-lock-'));
    try {
      const lockPath = join(directory, 'alternate.lock.json');
      const lock = await sourceLockWithPaths(role => role === 'jmdict'
        ? 'work/live-data/JMdict_e.gz'
        : `test-inputs/${role}`);
      await writeFile(lockPath, JSON.stringify(lock));
      await expect(downloadDataFile('jmdict', {
        force: true,
        silent: true,
        sourceLockPaths: [lockPath]
      })).rejects.toThrow('Refusing to overwrite pinned source-compiler input');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('an alternate lock cannot disable protection for the standard locks', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-download-lock-'));
    try {
      const unrelatedLock = join(directory, 'unrelated.lock.json');
      const lock = await sourceLockWithPaths(role => `unrelated/${role}`);
      await writeFile(unrelatedLock, JSON.stringify(lock));
      await expect(downloadDataFile('jmdict', {
        force: true,
        silent: true,
        destinationDirectory: join(import.meta.dir, '..'),
        sourceLockPaths: [unrelatedLock]
      })).rejects.toThrow('Refusing to overwrite pinned source-compiler input');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
