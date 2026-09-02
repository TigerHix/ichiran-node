import { describe, expect, test } from 'bun:test';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { downloadDataFile, getDataPath } from '../src/data/download.js';
import { SOURCE_COMPILER_INPUT_ROLES } from '../src/source-compiler/source-lock.js';

describe('legacy data downloader safety', () => {
  test('uses the ignored live-data area instead of compiler pins', () => {
    expect(getDataPath('jmdict')).toEndWith('/work/live-data/JMdict_e.gz');
    expect(getDataPath('kanjidic')).toEndWith('/work/live-data/kanjidic2.xml.gz');
  });

  test('force cannot overwrite a role selected by an alternate source lock', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-download-lock-'));
    try {
      const lockPath = join(directory, 'alternate.lock.json');
      const sources = SOURCE_COMPILER_INPUT_ROLES.map(role => ({
        id: role,
        role,
        pinnedPath: role === 'jmdict' ? 'work/live-data/JMdict_e.gz' : `test-inputs/${role}`,
        pinnedBytes: 0,
        pinnedSha256: '0'.repeat(64)
      }));
      await writeFile(lockPath, JSON.stringify({
        formatVersion: 1,
        baseline: {
          repository: 'https://example.test/repository.git',
          startingCommit: '0123456789abcdef0123456789abcdef01234567',
          qualifiedArtifactTag: 'baseline',
          upstreamIchiranCommit: '89abcdef0123456789abcdef0123456789abcdef',
          upstreamDataReleaseTag: 'data'
        },
        sources
      }));
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
      const sources = SOURCE_COMPILER_INPUT_ROLES.map(role => ({
        id: role,
        role,
        pinnedPath: `unrelated/${role}`,
        pinnedBytes: 0,
        pinnedSha256: '0'.repeat(64)
      }));
      await writeFile(unrelatedLock, JSON.stringify({
        formatVersion: 1,
        baseline: {
          repository: 'https://example.test/repository.git',
          startingCommit: '0123456789abcdef0123456789abcdef01234567',
          qualifiedArtifactTag: 'baseline',
          upstreamIchiranCommit: '89abcdef0123456789abcdef0123456789abcdef',
          upstreamDataReleaseTag: 'data'
        },
        sources
      }));
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
