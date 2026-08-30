import { afterEach, describe, expect, test } from 'bun:test';
import { execFile as execFileCallback } from 'node:child_process';
import { mkdtemp, mkdir, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { promisify } from 'node:util';
import {
  assertBrowserAlphaMorphologyAttestation,
  assertBytesEqual,
  deterministicJson,
  parseBrowserAlphaSourceLock,
  sha256Bytes,
  verifyBrowserAlphaOracleCore,
  verifyBrowserAlphaSources,
  verifyBrowserAlphaToolchain,
  type BrowserAlphaActualToolchain,
  type BrowserAlphaMorphologyAttestation
} from '../src/browser-pack/release-orchestration.js';

const temporaryDirectories: string[] = [];
const execFile = promisify(execFileCallback);

afterEach(async () => {
  await Promise.all(temporaryDirectories.splice(0).map((path) => rm(path, {
    recursive: true,
    force: true
  })));
});

function lock(source: { readonly bytes: number; readonly sha256: string }) {
  return {
    formatVersion: 2,
    upstreamIchiran: {
      repository: 'https://github.com/tshatrov/ichiran.git',
      commit: '0123456789abcdef0123456789abcdef01234567',
      dataReleaseTag: 'ichiran-260118'
    },
    postgresReference: {
      repositoryCommit: 'fedcba9876543210fedcba9876543210fedcba98'
    },
    databaseDump: {
      url: 'https://example.test/ichiran-260118.pgdump',
      bytes: 42,
      sha256: '7'.repeat(64)
    },
    database: {
      name: 'fixture',
      postgresServerVersion: '16.15',
      encoding: 'UTF8',
      collation: 'C.UTF-8',
      ctype: 'C.UTF-8',
      schemaSha256: 'a'.repeat(64)
    },
    toolchain: {
      bun: '1.3.5',
      node: '22.18.0',
      cargo: 'cargo 1.92.0',
      rustc: 'rustc 1.92.0',
      pgDump: 'pg_dump (PostgreSQL) 16.15',
      packFormat: 1,
      detailsFormat: 2,
      surfaceIndexFormat: 1,
      rootPayloadFormat: 2,
      morphologyFormat: 1,
      analyzerSupportFormat: 2,
      analyzerAnnotationsFormat: 4
    },
    sources: [{ path: 'data/source.txt', ...source }],
    artifacts: artifactCounts(),
    artifactDigests: artifactDigests(morphologyAttestation())
  } as const;
}

function artifactCounts() {
  const counts = (...fields: string[]) => Object.fromEntries(fields.map(field => [field, 1]));
  return {
    surfaceIndex: counts('input', 'accepted', 'direct', 'morphology', 'overlap', 'omitted', 'states', 'edges'),
    rootPayload: counts('surfaces', 'forms', 'entries', 'restrictions'),
    morphology: counts('positions', 'rules', 'templates', 'suffixes', 'rootKeys', 'rootGroups', 'patches', 'tombstones'),
    analyzerSupport: counts('suffixKeys', 'suffixValues', 'suffixClasses', 'counterKeys', 'counterVariants', 'collisions', 'generatedRules', 'generatedAliases'),
    annotations: counts(
      'blocks', 'splits', 'hints', 'generatedBlocks', 'generatedRoots', 'generatedRecords',
      'lookupOrderRecords', 'lookupOrderRoots', 'lookupOrderBytes',
      'lookupOrderExceptionSurfaces', 'lookupOrderExceptionClasses',
      'lookupOrderExceptionLocators', 'lookupOrderExceptionBytes', 'generatedPhysicalGroups',
      'generatedFactPairs', 'indexBytes', 'uncompressedBytes', 'compressedBytes',
      'annotationUncompressedBytes', 'annotationCompressedBytes', 'generatedUncompressedBytes',
      'generatedCompressedBytes', 'totalBytes', 'largestUncompressedBlock',
      'largestGeneratedBlock', 'largestGeneratedCompressedBlock'
    ),
    details: counts('entries', 'forms', 'senses', 'glosses', 'properties')
  };
}

function morphologyAttestation(): BrowserAlphaMorphologyAttestation {
  return {
    rows: 1,
    sha256: '9'.repeat(64),
    relationRows: 3,
    surfaceGroups: 2,
    exactSurfaceGroups: 1,
    legacyRelationKeys: 3,
    alphaRelationKeys: 2,
    legacyOnly: 1,
    alphaOnly: 0,
    duplicateLegacyRows: 0,
    duplicateAlphaCandidates: 0,
    databaseArtifacts: {
      csrRows: 10,
      installedRouteCsrRows: 9,
      activeRouteCsrRows: 8,
      inactiveRouteCsrRows: 1,
      uninstalledCsrRows: 1,
      dualRouteCsrRows: 0,
      ghostSourceRows: 2,
      ghostRootSurfacePairs: 1,
      multiPropertyLinks: 3,
      staleRawKanaSurfaces: 1
    }
  };
}

function artifactDigests(attestation: BrowserAlphaMorphologyAttestation) {
  const artifact = { bytes: 1, sha256: '8'.repeat(64) };
  return {
    surfaceIndex: artifact,
    rootPayload: artifact,
    morphology: artifact,
    analyzerSupport: artifact,
    analyzerAnnotations: artifact,
    details: artifact,
    morphologyRelation: attestation
  };
}

describe('browser-alpha release orchestration', () => {
  test('verifies every raw source and hashes the exact lock bytes', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ichiran-release-test-'));
    temporaryDirectories.push(root);
    await mkdir(join(root, 'browser-alpha'));
    await mkdir(join(root, 'data'));
    const source = new TextEncoder().encode('pinned source\n');
    const expected = lock({ bytes: source.byteLength, sha256: sha256Bytes(source) });
    const lockBytes = deterministicJson(expected);
    await writeFile(join(root, 'data/source.txt'), source);
    await writeFile(join(root, 'browser-alpha/sources.lock.json'), lockBytes);

    const result = await verifyBrowserAlphaSources(root);
    expect(result.lock).toEqual(expected);
    expect(result.lockSha256).toBe(sha256Bytes(lockBytes));
    expect(result.sources).toEqual([{
      path: 'data/source.txt',
      bytes: source.byteLength,
      sha256: sha256Bytes(source)
    }]);

    await writeFile(join(root, 'data/source.txt'), 'changed\n');
    await expect(verifyBrowserAlphaSources(root)).rejects.toThrow('bytes');
  });

  test('requires explicit provenance, complete artifacts, safe paths, and exact toolchain', () => {
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
      ...lock({ bytes: 0, sha256: 'c'.repeat(64) }),
      sources: [{ path: '../outside', bytes: 0, sha256: 'c'.repeat(64) }]
    }))).toThrow('portable repository-relative');

    const complete = lock({ bytes: 0, sha256: 'c'.repeat(64) });
    const { upstreamIchiran: _missingUpstream, ...withoutUpstream } = complete;
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify(withoutUpstream)))
      .toThrow('missing provenance');
    const incompleteAnnotations = { ...complete.artifacts.annotations } as Record<string, unknown>;
    delete incompleteAnnotations.lookupOrderExceptionBytes;
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
      ...complete,
      artifacts: { ...complete.artifacts, annotations: incompleteAnnotations }
    }))).toThrow('annotations.lookupOrderExceptionBytes');

    const expected = complete.toolchain;
    const actual: BrowserAlphaActualToolchain = { ...expected };
    expect(() => verifyBrowserAlphaToolchain(expected, actual)).not.toThrow();
    expect(() => verifyBrowserAlphaToolchain(expected, { ...actual, node: '23.0.0' }))
      .toThrow('sources lock requires');
  });

  test('requires and compares the complete measured morphology attestation', () => {
    const expected = morphologyAttestation();
    expect(() => assertBrowserAlphaMorphologyAttestation(expected, expected)).not.toThrow();
    expect(() => assertBrowserAlphaMorphologyAttestation(
      { ...expected, relationRows: 4, duplicateLegacyRows: 1 },
      expected
    )).toThrow('duplicate legacy rows');
    expect(() => assertBrowserAlphaMorphologyAttestation(
      { ...expected, rows: 2, alphaRelationKeys: 3, alphaOnly: 1 },
      expected
    )).toThrow('alpha-only candidates');
    expect(() => assertBrowserAlphaMorphologyAttestation(
      { ...expected, sha256: '7'.repeat(64) },
      expected
    )).toThrow('digest');

    const complete = {
      ...lock({ bytes: 0, sha256: 'c'.repeat(64) }),
      artifactDigests: artifactDigests(expected)
    };
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify(complete))).not.toThrow();
    const incompleteDatabaseArtifacts: Record<string, unknown> = {
      ...expected.databaseArtifacts
    };
    delete incompleteDatabaseArtifacts.ghostSourceRows;
    const incomplete = {
      ...complete,
      artifactDigests: {
        ...complete.artifactDigests,
        morphologyRelation: {
          ...expected,
          databaseArtifacts: incompleteDatabaseArtifacts
        }
      }
    };
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify(incomplete)))
      .toThrow('databaseArtifacts.ghostSourceRows');
  });

  test('canonical reports and rebuild comparisons are byte exact', () => {
    const first = deterministicJson({ z: [1, 2], stable: true });
    const second = deterministicJson({ z: [1, 2], stable: true });
    expect(first).toEqual(second);
    expect(new TextDecoder().decode(first).endsWith('\n')).toBeTrue();
    expect(() => assertBytesEqual(first, second, 'fixture')).not.toThrow();
    const changed = second.slice();
    changed[2] ^= 1;
    expect(() => assertBytesEqual(first, changed, 'fixture')).toThrow('byte 2');
  });

  test('pins the complete legacy core tree to an ancestor commit', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ichiran-oracle-test-'));
    temporaryDirectories.push(root);
    await mkdir(join(root, 'packages/core/src'), { recursive: true });
    await execFile('git', ['-C', root, 'init', '-q']);
    await execFile('git', ['-C', root, 'config', 'user.email', 'fixture@example.test']);
    await execFile('git', ['-C', root, 'config', 'user.name', 'Fixture']);
    await writeFile(join(root, 'packages/core/src/oracle.ts'), 'export const oracle = true;\n');
    await execFile('git', ['-C', root, 'add', 'packages/core/src/oracle.ts']);
    await execFile('git', ['-C', root, 'commit', '-qm', 'oracle']);
    const { stdout } = await execFile('git', ['-C', root, 'rev-parse', 'HEAD'], { encoding: 'utf8' });
    const oracleCommit = stdout.trim();

    await mkdir(join(root, 'browser-alpha'));
    await writeFile(join(root, 'browser-alpha/readme.md'), 'browser work\n');
    await execFile('git', ['-C', root, 'add', 'browser-alpha/readme.md']);
    await execFile('git', ['-C', root, 'commit', '-qm', 'browser']);
    await mkdir(join(root, 'packages/reference-postgres'), { recursive: true });
    await execFile('git', [
      '-C', root, 'mv', 'packages/core/src', 'packages/reference-postgres/src'
    ]);
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).resolves.toBeUndefined();

    await writeFile(
      join(root, 'packages/reference-postgres/src/oracle.ts'),
      'export const oracle = false;\n'
    );
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).rejects.toThrow(
      'Legacy oracle core differs'
    );
    await writeFile(
      join(root, 'packages/reference-postgres/src/oracle.ts'),
      'export const oracle = true;\n'
    );
    await writeFile(join(root, 'packages/reference-postgres/src/untracked.ts'), 'export {};\n');
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).rejects.toThrow(
      'untracked files'
    );
  });
});
