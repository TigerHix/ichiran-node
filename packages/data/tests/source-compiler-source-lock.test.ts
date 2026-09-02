import { describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { mkdtemp, mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
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

const archive = {
  repository: 'https://example.test/archive.git',
  commit: baseline.startingCommit,
  date: '2026-01-01',
  acquisitionScript: 'scripts/acquire.ts'
};

function fixtureLock(
  sources: readonly Record<string, unknown>[],
  extra: Record<string, unknown> = {}
): Record<string, unknown> {
  return { formatVersion: 1, baseline, archive, sources, ...extra };
}

function pin(role: SourceCompilerInputRole, path: string, contents: Uint8Array | string) {
  return {
    role,
    pinnedPath: path,
    pinnedBytes: typeof contents === 'string' ? Buffer.byteLength(contents) : contents.byteLength,
    pinnedSha256: sha256(contents)
  };
}

function sourceRecords(
  path: (role: SourceCompilerInputRole) => string = role => role,
  contents: (role: SourceCompilerInputRole) => Uint8Array | string = role => role
): Record<string, unknown>[] {
  const pinned = (role: SourceCompilerInputRole) => pin(role, path(role), contents(role));
  const multiple = (roles: readonly SourceCompilerInputRole[]) => ({
    roles,
    pinnedPaths: roles.map(path),
    pinnedBytes: roles.map(role => {
      const value = contents(role);
      return typeof value === 'string' ? Buffer.byteLength(value) : value.byteLength;
    }),
    pinnedSha256: roles.map(role => sha256(contents(role)))
  });
  return [
    {
      id: 'source-jmdict', kind: 'jmdict', ...pinned('jmdict'),
      authoritativeUrl: 'https://example.test/JMdict_e.gz',
      archivePath: 'archive/2026/01/01.patch.br',
      upstreamIdentity: 'JMdict created: 2026-01-01',
      uncompressedBytes: 1,
      uncompressedSha256: sha256('uncompressed-jmdict'),
      license: 'CC-BY-SA-4.0',
      licenseUrl: 'https://example.test/license',
      attribution: 'Fixture dictionary authority'
    },
    {
      id: 'source-kanjidic', kind: 'kanjidic2', ...pinned('kanjidic'),
      authoritativeUrl: 'http://example.test/kanjidic2.xml.gz',
      historicalCaptureUrl: 'https://example.test/archive/kanjidic2.xml.gz',
      upstreamIdentity: 'file_version 4; database_version fixture; date_of_creation 2026-01-01',
      uncompressedBytes: 1,
      uncompressedSha256: sha256('uncompressed-kanjidic'),
      license: 'CC-BY-SA-3.0',
      licenseUrl: 'https://example.test/license',
      attribution: 'Fixture dictionary authority'
    },
    {
      id: 'source-extra', kind: 'custom-entries', ...pinned('extra'),
      authoritativeUrl: 'https://example.test/extra.xml',
      license: 'GPL-3.0-or-later'
    },
    {
      id: 'source-municipalities', kind: 'custom-entries',
      ...multiple(['municipality', 'ward']),
      authoritativeUrl: 'https://example.test/custom-sources',
      license: 'GPL-3.0-or-later'
    },
    {
      id: 'source-intended-behavior', kind: 'intended-behavior',
      authoritativeUrl: 'https://example.test/upstream',
      upstreamPaths: ['dict-custom.lisp', 'dict-errata.lisp'],
      upstreamBytes: [1, 1],
      upstreamSha256: [sha256('custom'), sha256('errata')],
      license: 'GPL-3.0-or-later'
    },
    {
      id: 'source-errata', kind: 'semantic-ledger', ...pinned('chronologicalErrata'),
      generatedBy: 'scripts/extract.ts',
      authority: 'Fixture upstream errata identity',
      rows: 1,
      license: 'GPL-3.0-or-later'
    },
    {
      id: 'source-compatibility', kind: 'compatibility-ledger', ...pinned('compatibility'),
      authority: 'Each fixture row carries explicit provenance.',
      rows: 1
    },
    {
      id: 'source-conjugation', kind: 'conjugation-rules',
      ...multiple(['kwpos', 'conjo']),
      authoritativeUrl: 'https://example.test/conjugation-rules',
      license: 'GPL-3.0-or-later'
    }
  ];
}

describe('source compiler lock', () => {
  test('pins the first post-baseline JMdict identity and all semantic roles', async () => {
    const path = join(import.meta.dir, '../../../data/source-compiler-update-2026-01-02.lock.json');
    const lock = parseSourceCompilerLock(JSON.parse(await readFile(path, 'utf8')));
    const baselinePath = join(import.meta.dir, '../../../data/source-compiler-sources.lock.json');
    const baselineLock = parseSourceCompilerLock(JSON.parse(await readFile(baselinePath, 'utf8')));
    expect(baselineLock.sources[0]).toMatchObject({
      kind: 'jmdict',
      archivePath: 'JMdict_e/patches/2026/01/01.patch.br'
    });
    expect(lock.sources[0]).toMatchObject({
      id: 'edrdg-jmdict-e-2026-01-02',
      kind: 'jmdict',
      file: {
        role: 'jmdict',
        path: 'work/m6-transition/JMdict_e-2026-01-02.gz',
        bytes: 10_261_624,
        sha256: '34cc33abe2ae37a8572a9a45ce68c5e7fb6ccccd55c021366eb4fa6c49f6c90c'
      },
      archiveCommit: 'fbc4afb4c786b7f4c304c173a475553279bbb528',
      upstreamIdentity: 'JMdict created: 2026-01-02',
      uncompressedSha256: '9f125b6f574102e37279660fa022de63fdff28f99c3f5d47ba69c80c3c999f34'
    });
    expect(lock.sources.flatMap(item => 'file' in item
      ? [item.file.role]
      : 'files' in item ? item.files.map(file => file.role) : []).sort()
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
      const jmdictXml = '<!-- JMdict created: 2026-01-01 -->';
      const kanjidicXml = '<file_version>4</file_version>'
        + '<database_version>fixture</database_version>'
        + '<date_of_creation>2026-01-01</date_of_creation>';
      const errata = new Uint8Array(await readFile(resolve(
        import.meta.dir,
        '../../../data/source-compiler-errata.json'
      )));
      const compatibility = new Uint8Array(await readFile(resolve(
        import.meta.dir,
        '../../../data/source-compiler-compatibility.json'
      )));
      const contents = new Map<SourceCompilerInputRole, Uint8Array | string>([
        ['jmdict', new Uint8Array(gzipSync(jmdictXml))],
        ['kanjidic', new Uint8Array(gzipSync(kanjidicXml))],
        ['extra', extra],
        ['municipality', 'municipality'],
        ['ward', 'ward'],
        ['chronologicalErrata', errata],
        ['compatibility', compatibility],
        ['kwpos', 'kwpos'],
        ['conjo', 'conjo']
      ]);
      const inputPath = (role: SourceCompilerInputRole): string =>
        role === 'extra' ? 'alternate/extra.xml' : `inputs/${role}`;
      const sources = sourceRecords(inputPath, role => contents.get(role)!);
      Object.assign(sources.find(source => source.kind === 'jmdict')!, {
        uncompressedBytes: Buffer.byteLength(jmdictXml),
        uncompressedSha256: sha256(jmdictXml)
      });
      Object.assign(sources.find(source => source.kind === 'kanjidic2')!, {
        uncompressedBytes: Buffer.byteLength(kanjidicXml),
        uncompressedSha256: sha256(kanjidicXml)
      });
      Object.assign(sources.find(source => source.kind === 'semantic-ledger')!, { rows: 601 });
      Object.assign(sources.find(source => source.kind === 'compatibility-ledger')!, { rows: 25 });
      for (const role of SOURCE_COMPILER_INPUT_ROLES) {
        await mkdir(join(repository, inputPath(role), '..'), { recursive: true });
        await writeFile(join(repository, inputPath(role)), contents.get(role)!);
      }
      const writeLock = async (): Promise<void> => {
        await writeFile(
          join(repository, 'data/source-compiler-sources.lock.json'),
          `${JSON.stringify(fixtureLock(sources))}\n`
        );
      };
      await writeLock();

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

      const jmdict = sources.find(source => source.kind === 'jmdict')!;
      const originalUncompressedSha256 = jmdict.uncompressedSha256;
      jmdict.uncompressedSha256 = '0'.repeat(64);
      await writeLock();
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow('expands to');
      jmdict.uncompressedSha256 = originalUncompressedSha256;

      const kanjidic = sources.find(source => source.kind === 'kanjidic2')!;
      const originalUpstreamIdentity = kanjidic.upstreamIdentity;
      kanjidic.upstreamIdentity =
        'file_version 4; database_version stale; date_of_creation 2026-01-01';
      await writeLock();
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow('lacks file_version 4');
      kanjidic.upstreamIdentity = originalUpstreamIdentity;

      const compatibilitySource = sources.find(
        source => source.kind === 'compatibility-ledger'
      )!;
      compatibilitySource.rows = 999;
      await writeLock();
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow(
        'has 25 rows; expected 999'
      );
      compatibilitySource.rows = 25;
      await writeLock();

      await writeFile(join(repository, 'inputs/conjo'), 'changed');
      await expect(verifySourceCompilerLock(repository)).rejects.toThrow('Locked source inputs/conjo');
    } finally {
      await rm(repository, { recursive: true, force: true });
    }
  });

  test('rejects missing, duplicate, ambiguous, and unexpected role assignments', () => {
    const complete = sourceRecords();
    expect(() => parseSourceCompilerLock(fixtureLock(
      complete.filter(item => item.kind !== 'jmdict')
    ))).toThrow('missing required roles: jmdict');
    expect(() => parseSourceCompilerLock(fixtureLock(
      complete.filter(item => item.kind !== 'intended-behavior')
    ))).toThrow('requires exactly one intended-behavior authority; found 0');
    expect(() => parseSourceCompilerLock(fixtureLock([
      ...complete,
      { ...complete[4], id: 'second-intended-behavior' }
    ]))).toThrow('requires exactly one intended-behavior authority; found 2');
    expect(() => parseSourceCompilerLock(fixtureLock([
        ...complete,
        { ...complete[0], id: 'second-jmdict', pinnedPath: 'second-jmdict' }
      ]))).toThrow('assigns duplicate roles: jmdict');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], role: 'databaseExport' }, ...complete.slice(1)
    ]))).toThrow('unexpected compiler input role databaseExport');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], pinnedPaths: ['other'], roles: ['jmdict'] }, ...complete.slice(1)
    ]))).toThrow('unknown fields');
    expect(() => parseSourceCompilerLock(fixtureLock([
      complete[0]!, { ...complete[1], pinnedPath: complete[0]!.pinnedPath }, ...complete.slice(2)
    ]))).toThrow('assigns one file to multiple roles: jmdict');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      sourcez: complete
    }))).toThrow('unknown fields: sourcez');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      baseline: { ...baseline, startingComit: baseline.startingCommit }
    }))).toThrow('unknown fields: startingComit');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], pinnedSha25: complete[0]!.pinnedSha256 }, ...complete.slice(1)
    ]))).toThrow('unknown fields: pinnedSha25');
    const { pinnedPath: _pinnedPath, ...incompletePin } = complete[0]!;
    expect(() => parseSourceCompilerLock(fixtureLock([
      incompletePin, ...complete.slice(1)
    ]))).toThrow('missing fields: pinnedPath');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], pinnedPath: 'inputs/../jmdict' }, ...complete.slice(1)
    ]))).toThrow('normalized repository-relative path');
  });

  test('rejects missing and unsupported source kinds', () => {
    const complete = sourceRecords();
    const { kind: _kind, ...missingKind } = complete[0]!;
    expect(() => parseSourceCompilerLock(fixtureLock([
      missingKind, ...complete.slice(1)
    ]))).toThrow('Source source-jmdict kind must be non-empty text');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], kind: 'jmdcit' }, ...complete.slice(1)
    ]))).toThrow('Source source-jmdict kind is unsupported: jmdcit');
  });

  test('rejects missing and malformed provenance claims', () => {
    const complete = sourceRecords();
    const { upstreamIdentity: _identity, ...missingIdentity } = complete[0]!;
    expect(() => parseSourceCompilerLock(fixtureLock([
      missingIdentity, ...complete.slice(1)
    ]))).toThrow('missing fields: upstreamIdentity');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], authoritativeUrl: 'not-a-url' }, ...complete.slice(1)
    ]))).toThrow('authoritativeUrl must be an absolute HTTP(S) URL');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], authoritativeUrl: 42 }, ...complete.slice(1)
    ]))).toThrow('authoritativeUrl');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], uncompressedSha256: 'ABC' }, ...complete.slice(1)
    ]))).toThrow('uncompressedSha256 must be a lowercase SHA-256');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], uncompressedSha256: 'not-a-digest' }, ...complete.slice(1)
    ]))).toThrow('uncompressedSha256 must be a lowercase SHA-256');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], license: [] }, ...complete.slice(1)
    ]))).toThrow('license');
    expect(() => parseSourceCompilerLock(fixtureLock([
      ...complete.slice(0, 6), { ...complete[6], authority: '   ' }, ...complete.slice(7)
    ]))).toThrow('authority must be non-empty text');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[0], uncompressedBytes: 0 }, ...complete.slice(1)
    ]))).toThrow('uncompressedBytes must be positive');
    expect(() => parseSourceCompilerLock(fixtureLock([
      { ...complete[4], upstreamBytes: [1] }, ...complete.slice(0, 4), ...complete.slice(5)
    ]))).toThrow('upstream identities have different lengths');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      baseline: { ...baseline, repository: 'git@example.test:repository.git' }
    }))).toThrow('Baseline repository must be an absolute HTTP(S) URL');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      archive: { ...archive, repository: 42 }
    }))).toThrow('Archive repository');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      archive: { ...archive, date: '2026-02-30' }
    }))).toThrow('Archive date must be an ISO YYYY-MM-DD date');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      transition: {
        date: '2026-1-2',
        scope: 'fixture transition',
        acquisitionScript: 'scripts/acquire.ts'
      }
    }))).toThrow('Transition date must be an ISO YYYY-MM-DD date');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      archive: undefined
    }))).toThrow('Archived JMdict capture requires archive provenance');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      transition: {
        date: '2026-01-01',
        scope: 'contradictory transition',
        acquisitionScript: 'scripts/acquire.ts'
      }
    }))).toThrow('Archived JMdict capture requires archive provenance and no transition');
    expect(() => parseSourceCompilerLock(fixtureLock(complete, {
      archive: { ...archive, date: '2026-01-02' }
    }))).toThrow('Archived JMdict capture contradicts the archive date');
    const { archivePath: _archivePath, ...jmdict } = complete[0]!;
    const patchedJmdict = {
      ...jmdict,
      archiveRepository: archive.repository,
      archiveCommit: archive.commit,
      archivePatch: 'archive/2026/01/01.patch.br',
      archivePatchBytes: 1,
      archivePatchSha256: sha256('patch')
    };
    expect(() => parseSourceCompilerLock(fixtureLock([
      patchedJmdict, ...complete.slice(1)
    ]))).toThrow('Patched JMdict transition requires transition provenance and no archive');
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
