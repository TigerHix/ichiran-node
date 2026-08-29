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
    formatVersion: 1,
    oracleRepositoryCommit: '0123456789abcdef0123456789abcdef01234567',
    database: {
      name: 'fixture',
      postgresServerVersion: '16.15',
      encoding: 'UTF8',
      collation: 'C.UTF-8',
      schemaSha256: 'a'.repeat(64)
    },
    toolchain: {
      bun: '1.3.5',
      node: '22.18.0',
      packFormat: 1,
      detailsFormat: 2,
      surfaceIndexFormat: 1,
      rootPayloadFormat: 2,
      morphologyFormat: 1,
      analyzerSupportFormat: 2,
      analyzerAnnotationsFormat: 4
    },
    sources: [{ path: 'data/source.txt', ...source }],
    projections: [{ name: 'root-forms', rows: 3, sha256: 'b'.repeat(64) }],
    directOrderProjection: {
      rows: 2,
      surfaces: 1,
      sha256: 'd'.repeat(64)
    },
    generatedProjection: {
      semanticPaths: 4,
      matchedPaths: 5,
      records: 3,
      lookupOrderRecords: 4,
      lookupOrderSourceRows: 7,
      lookupOrderSourceSha256: '1'.repeat(64),
      lookupOrderSurfaces: 2,
      lookupOrderClasses: 3,
      lookupOrderEquivalenceClasses: 3,
      lookupOrderComponents: 3,
      lookupOrderCyclicComponents: 1,
      lookupOrderEdges: 2,
      lookupOrderMaxRank: 1,
      lookupOrderSha256: 'e'.repeat(64),
      lookupOrderExceptionSurfaces: 1,
      lookupOrderExceptionClasses: 2,
      lookupOrderExceptionLocators: 2,
      countExceptions: 2,
      physicalGroups: 1,
      physicalMembers: 1,
      propertyOverrides: 1,
      maxMemberOrd: 1,
      maxViaMemberOrd: 0,
      maxPropOrd: 0,
      sha256: 'f'.repeat(64)
    }
  } as const;
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

  test('rejects path escapes, duplicate projections, and toolchain drift', () => {
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
      ...lock({ bytes: 0, sha256: 'c'.repeat(64) }),
      sources: [{ path: '../outside', bytes: 0, sha256: 'c'.repeat(64) }]
    }))).toThrow('portable repository-relative');
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
      ...lock({ bytes: 0, sha256: 'c'.repeat(64) }),
      projections: [
        { name: 'same', rows: 1, sha256: 'd'.repeat(64) },
        { name: 'same', rows: 1, sha256: 'e'.repeat(64) }
      ]
    }))).toThrow('Duplicate locked projection');

    const complete = lock({ bytes: 0, sha256: 'c'.repeat(64) });
    for (const [field, message] of [
      ['lookupOrderSourceRows', 'Generated lookup-order source rows'],
      ['lookupOrderSourceSha256', 'Generated lookup-order source digest'],
      ['lookupOrderComponents', 'Generated lookup-order components'],
      ['lookupOrderCyclicComponents', 'Generated lookup-order cyclic components'],
      ['lookupOrderEdges', 'Generated lookup-order edges'],
      ['lookupOrderMaxRank', 'Generated lookup-order maximum rank'],
      ['lookupOrderExceptionSurfaces', 'Generated lookup-order exception surfaces'],
      ['lookupOrderExceptionClasses', 'Generated lookup-order exception classes'],
      ['lookupOrderExceptionLocators', 'Generated lookup-order exception locators']
    ] as const) {
      const incompleteGeneratedProjection: Record<string, unknown> = {
        ...complete.generatedProjection
      };
      delete incompleteGeneratedProjection[field];
      expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
        ...complete,
        generatedProjection: incompleteGeneratedProjection
      }))).toThrow(message);
    }

    const artifactFixture = {
      annotations: {
        lookupOrderExceptionSurfaces: 1,
        lookupOrderExceptionClasses: 2,
        lookupOrderExceptionLocators: 3,
        lookupOrderExceptionBytes: 40
      }
    };
    for (const [field, message] of [
      ['lookupOrderExceptionSurfaces', 'Annotation lookup-order exception surfaces'],
      ['lookupOrderExceptionClasses', 'Annotation lookup-order exception classes'],
      ['lookupOrderExceptionLocators', 'Annotation lookup-order exception locators'],
      ['lookupOrderExceptionBytes', 'Annotation lookup-order exception bytes']
    ] as const) {
      const annotations: Record<string, unknown> = { ...artifactFixture.annotations };
      delete annotations[field];
      expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
        ...complete,
        artifacts: { annotations }
      }))).toThrow(message);
    }

    const { directOrderProjection: _missingDirectOrderProjection, ...withoutDirectOrder } = complete;
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify(withoutDirectOrder)))
      .toThrow('missing database, toolchain, sources, or projections');

    const { sha256: _missingDirectOrderDigest, ...incompleteDirectOrderProjection } =
      complete.directOrderProjection;
    expect(() => parseBrowserAlphaSourceLock(JSON.stringify({
      ...complete,
      directOrderProjection: incompleteDirectOrderProjection
    }))).toThrow('Direct-order projection digest');

    const expected = lock({ bytes: 0, sha256: 'c'.repeat(64) }).toolchain;
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
    await mkdir(join(root, 'packages/core'), { recursive: true });
    await execFile('git', ['-C', root, 'init', '-q']);
    await execFile('git', ['-C', root, 'config', 'user.email', 'fixture@example.test']);
    await execFile('git', ['-C', root, 'config', 'user.name', 'Fixture']);
    await writeFile(join(root, 'packages/core/oracle.ts'), 'export const oracle = true;\n');
    await execFile('git', ['-C', root, 'add', 'packages/core/oracle.ts']);
    await execFile('git', ['-C', root, 'commit', '-qm', 'oracle']);
    const { stdout } = await execFile('git', ['-C', root, 'rev-parse', 'HEAD'], { encoding: 'utf8' });
    const oracleCommit = stdout.trim();

    await mkdir(join(root, 'browser-alpha'));
    await writeFile(join(root, 'browser-alpha/readme.md'), 'browser work\n');
    await execFile('git', ['-C', root, 'add', 'browser-alpha/readme.md']);
    await execFile('git', ['-C', root, 'commit', '-qm', 'browser']);
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).resolves.toBeUndefined();

    await writeFile(join(root, 'packages/core/oracle.ts'), 'export const oracle = false;\n');
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).rejects.toThrow(
      'Legacy oracle core differs'
    );
    await writeFile(join(root, 'packages/core/oracle.ts'), 'export const oracle = true;\n');
    await writeFile(join(root, 'packages/core/untracked.ts'), 'export {};\n');
    await expect(verifyBrowserAlphaOracleCore(root, oracleCommit)).rejects.toThrow(
      'untracked files'
    );
  });
});
