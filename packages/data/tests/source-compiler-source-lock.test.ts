import { describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { mkdtemp, mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { loadExtraRootDrafts } from '../src/source-compiler/custom-sources.js';
import {
  parseSourceCompilerLock,
  SOURCE_COMPILER_INPUT_ROLES,
  verifySourceCompilerLock,
  type SourceCompilerInputRole
} from '../src/source-compiler/source-lock.js';

function sha256(value: string | Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

const baseline = {
  repository: 'https://example.test/repository.git',
  startingCommit: '0123456789abcdef0123456789abcdef01234567',
  qualifiedArtifactTag: 'baseline',
  upstreamIchiranCommit: '89abcdef0123456789abcdef0123456789abcdef',
  upstreamDataReleaseTag: 'data'
};

function source(role: SourceCompilerInputRole, path: string, contents: Uint8Array | string) {
  return {
    id: `source-${role}`,
    role,
    pinnedPath: path,
    pinnedBytes: typeof contents === 'string' ? Buffer.byteLength(contents) : contents.byteLength,
    pinnedSha256: sha256(contents)
  };
}

describe('source compiler lock', () => {
  test('pins the first post-baseline JMdict identity and all semantic roles', async () => {
    const path = join(import.meta.dir, '../../../data/source-compiler-update-2026-01-02.lock.json');
    const lock = parseSourceCompilerLock(JSON.parse(await readFile(path, 'utf8')));
    expect(lock.sources[0]).toEqual({
      id: 'edrdg-jmdict-e-2026-01-02',
      kind: 'file',
      file: {
        role: 'jmdict',
        path: 'work/m6-transition/JMdict_e-2026-01-02.gz',
        bytes: 10_261_624,
        sha256: '34cc33abe2ae37a8572a9a45ce68c5e7fb6ccccd55c021366eb4fa6c49f6c90c'
      }
    });
    expect(lock.sources.flatMap(item => item.kind === 'authority'
      ? []
      : item.kind === 'file' ? [item.file.role] : item.files.map(file => file.role)).sort()
    ).toEqual([...SOURCE_COMPILER_INPUT_ROLES].sort());
  });

  test('verifies exactly the role-selected files and the compiler consumes an alternate path', async () => {
    const repository = await mkdtemp(join(tmpdir(), 'ichiran-source-lock-'));
    try {
      await mkdir(join(repository, 'data'));
      const extra = new Uint8Array(await readFile(resolve(
        import.meta.dir,
        '../../../data/sources/extra.xml'
      )));
      const contents = new Map<SourceCompilerInputRole, Uint8Array | string>([
        ['jmdict', 'jmdict'],
        ['kanjidic', 'kanjidic'],
        ['extra', extra],
        ['municipality', 'municipality'],
        ['ward', 'ward'],
        ['chronologicalErrata', 'errata'],
        ['compatibility', 'compatibility'],
        ['kwpos', 'kwpos'],
        ['conjo', 'conjo']
      ]);
      const sources = SOURCE_COMPILER_INPUT_ROLES.map(role => {
        const path = role === 'extra' ? 'alternate/extra.xml' : `inputs/${role}`;
        return source(role, path, contents.get(role)!);
      });
      for (const item of sources) {
        await mkdir(join(repository, item.pinnedPath, '..'), { recursive: true });
        await writeFile(join(repository, item.pinnedPath), contents.get(item.role)!);
      }
      await writeFile(join(repository, 'data/source-compiler-sources.lock.json'), `${JSON.stringify({
        formatVersion: 1,
        baseline,
        sources: [...sources, { id: 'external-authority-without-local-file' }]
      })}\n`);

      const result = await verifySourceCompilerLock(repository);
      expect(result.files).toHaveLength(SOURCE_COMPILER_INPUT_ROLES.length);
      expect(result.inputs.extra).toMatchObject({
        id: 'source-extra',
        role: 'extra',
        path: 'alternate/extra.xml',
        bytes: extra.byteLength
      });
      expect((await loadExtraRootDrafts(result.inputs.extra.absolutePath, 0)).map(row => row.entry.seq))
        .toEqual([12_294_525, 12_294_526, 12_294_576, 900_000, 900_001]);

      await writeFile(join(repository, 'inputs/conjo'), 'changed');
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow('Locked source inputs/conjo');
    } finally {
      await rm(repository, { recursive: true, force: true });
    }
  });

  test('rejects missing, duplicate, ambiguous, and unexpected role assignments', () => {
    const complete = SOURCE_COMPILER_INPUT_ROLES.map(role => source(role, role, role));
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: complete.slice(1)
    })).toThrow('missing required roles: jmdict');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [
        ...complete,
        { ...source('jmdict', 'second-jmdict', 'jmdict'), id: 'second-jmdict' }
      ]
    })).toThrow('assigns duplicate roles: jmdict');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [{ ...complete[0], role: 'databaseExport' }, ...complete.slice(1)]
    })).toThrow('unexpected compiler input role databaseExport');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [{ ...complete[0], pinnedPaths: ['other'], roles: ['jmdict'] }, ...complete.slice(1)]
    })).toThrow('mixes single-file and multi-file role assignments');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [complete[0], { ...complete[1], pinnedPath: complete[0]!.pinnedPath }, ...complete.slice(2)]
    })).toThrow('assigns one file to multiple roles: jmdict');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sourcez: complete,
      sources: complete
    })).toThrow('unknown fields: sourcez');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline: { ...baseline, startingComit: baseline.startingCommit },
      sources: complete
    })).toThrow('unknown fields: startingComit');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [{ ...complete[0], pinnedSha25: complete[0]!.pinnedSha256 }, ...complete.slice(1)]
    })).toThrow('unknown fields: pinnedSha25');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [{ id: 'orphaned-pin', pinnedBytes: 1, pinnedSha256: '0'.repeat(64) }, ...complete]
    })).toThrow('incomplete pinned file identity');
    expect(() => parseSourceCompilerLock({
      formatVersion: 1,
      baseline,
      sources: [{ ...complete[0], pinnedPath: 'inputs/../jmdict' }, ...complete.slice(1)]
    })).toThrow('normalized repository-relative path');
  });

  test('qualification workflows do not reopen semantic inputs by conventional path', async () => {
    const repository = resolve(import.meta.dir, '../../..');
    const workflows = [
      'scripts/source-compiler-bounded-support.ts',
      'scripts/source-compiler-surface-probe.ts',
      'scripts/source-compiler-configured-forward.ts',
      'scripts/source-compiler-root-proof.ts',
      'scripts/source-compiler-generated-order-proof.ts',
      'scripts/source-compiler-update-witness.ts',
      'scripts/source-compiler-conjugation-proof.ts'
    ];
    const conventionalPath = /JMdict_e\.gz|kanjidic2\.xml\.gz|extra\.xml|jichitai\.csv|gyoseiku\.csv|kwpos\.csv|conjo\.csv|source-compiler-(?:errata|compatibility)\.json/;
    for (const workflow of workflows) {
      const source = await readFile(join(repository, workflow), 'utf8');
      expect(source).toContain('verifySourceCompilerLock');
      expect(source).not.toMatch(conventionalPath);
    }
  });
});
