import { describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { mkdtemp, mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  parseSourceCompilerLock,
  verifySourceCompilerLock
} from '../src/source-compiler/source-lock.js';

function sha256(text: string): string {
  return createHash('sha256').update(text).digest('hex');
}

describe('source compiler lock', () => {
  test('pins the first post-baseline JMdict identity', async () => {
    const path = join(import.meta.dir, '../../../data/source-compiler-update-2026-01-02.lock.json');
    const lock = parseSourceCompilerLock(JSON.parse(await readFile(path, 'utf8')));
    expect(lock.sources[0]).toEqual({
      id: 'edrdg-jmdict-e-2026-01-02',
      kind: 'file',
      file: {
        path: 'work/m6-transition/JMdict_e-2026-01-02.gz',
        bytes: 10_261_624,
        sha256: '34cc33abe2ae37a8572a9a45ce68c5e7fb6ccccd55c021366eb4fa6c49f6c90c'
      }
    });
  });

  test('verifies every concrete pinned source identity', async () => {
    const repository = await mkdtemp(join(tmpdir(), 'ichiran-source-lock-'));
    try {
      await mkdir(join(repository, 'data'));
      await writeFile(join(repository, 'one.txt'), 'one');
      await writeFile(join(repository, 'two.txt'), 'two');
      const lock = {
        formatVersion: 1,
        jmdictSourceId: 'one',
        baseline: {
          qualifiedArtifactTag: 'baseline',
          upstreamIchiranCommit: 'upstream',
          upstreamDataReleaseTag: 'data'
        },
        sources: [
          {
            id: 'one', pinnedPath: 'one.txt', pinnedBytes: 3, pinnedSha256: sha256('one')
          },
          {
            id: 'two', pinnedPaths: ['two.txt'], pinnedBytes: [3], pinnedSha256: [sha256('two')]
          },
          { id: 'external-authority-without-local-file' }
        ]
      };
      await writeFile(
        join(repository, 'data/source-compiler-sources.lock.json'),
        `${JSON.stringify(lock)}\n`
      );
      const result = await verifySourceCompilerLock(repository);
      expect(result.files.map(value => value.path)).toEqual(['one.txt', 'two.txt']);
      expect(result.jmdict).toMatchObject({ id: 'one', path: 'one.txt', bytes: 3 });
      expect(result.lock.sources).toHaveLength(3);
      await writeFile(join(repository, 'two.txt'), 'changed');
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow('Locked source two.txt');
    } finally {
      await rm(repository, { recursive: true, force: true });
    }
  });

  test('requires the JMdict identity to name exactly one pinned file source', () => {
    const baseline = {
      qualifiedArtifactTag: 'baseline',
      upstreamIchiranCommit: 'upstream',
      upstreamDataReleaseTag: 'data'
    };
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      jmdictSourceId: 'missing',
      baseline,
      sources: [{ id: 'one', pinnedPath: 'one', pinnedBytes: 1, pinnedSha256: '0'.repeat(64) }]
    })).toThrow('JMdict source id must name one pinned file');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      jmdictSourceId: 'external',
      baseline,
      sources: [{ id: 'external' }]
    })).toThrow('JMdict source id must name one pinned file');
  });
});
