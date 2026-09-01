import { describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { BrowserAlphaArtifactCounts } from '../src/browser-pack/release-orchestration.js';
import {
  artifactIdentities,
  compareArtifactCounts,
  compareQualifiedArtifactBytes,
  compareQualifiedArtifactCounts,
  generatedOrderReleaseGateCandidate,
  parseGeneratedOrderAttestation,
  parseRootPayloadOrderAttestation,
  parseSurfaceCompilerStats,
  type QualifiedArtifactBytes
} from '../src/source-compiler/release-evidence.js';

const data = resolve(import.meta.dir, '../../../data');

function artifacts(root: string): QualifiedArtifactBytes {
  return {
    surfaceIndex: new TextEncoder().encode('surface'),
    rootPayload: new TextEncoder().encode(root),
    morphology: new TextEncoder().encode('morphology'),
    analyzerSupport: new TextEncoder().encode('support'),
    analyzerAnnotations: new TextEncoder().encode('annotations'),
    details: new TextEncoder().encode('details')
  };
}

function counts(
  support: number,
  annotations: number,
  positions = 1
): BrowserAlphaArtifactCounts {
  return {
    surfaceIndex: { input: 1 },
    rootPayload: { surfaces: 1 },
    morphology: { positions },
    analyzerSupport: { suffixKeys: support },
    annotations: { blocks: annotations },
    details: { entries: 3 }
  } as unknown as BrowserAlphaArtifactCounts;
}

function exactRootReview() {
  return {
    sourceSha256: '32de7d5e7593fa4f8e752523816f44850e4c2298cb7289645fc2a3d906130ac1',
    qualifiedSha256: '32de7d5e7593fa4f8e752523816f44850e4c2298cb7289645fc2a3d906130ac1',
    attestation: new Uint8Array(),
    attestationSha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
    fullEvidence: {
      rows: 0,
      bytes: 0,
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
    },
    provenance: 'none',
    policy: 'none',
    preservedBehavior: 'none'
  };
}

describe('source release evidence', () => {
  test('parses the one reviewed baseline root-order attestation', async () => {
    const bytes = await readFile(resolve(
      import.meta.dir,
      `${data}/source-compiler-direct-order-attestation.json`
    ));
    const attestation = parseRootPayloadOrderAttestation(JSON.parse(bytes.toString('utf8')));
    expect(attestation.fullEvidence).toEqual({
      path: 'work/m6-evidence/direct-order.jsonl',
      rows: 3_435,
      bytes: 4_076_458,
      sha256: '5f4660a0afbc1a21021f3c4db49014554a3b7991a48960c2238a050ae05a1854'
    });
    expect(attestation.sourcePayload.sha256)
      .toBe('19204bdae9ec44f7a5240aa7b74e83cf302a8f8da09b4a1748445ef0dd5dc8d2');
    expect(attestation.qualifiedPayload.sha256)
      .toBe('2bd83550fc67ae90dcaed1db37dc0b596091ea49081c00f34f2325f846b9aafa');
  });

  test('enforces the exhaustive generated-order attestation gate', async () => {
    const bytes = await readFile(resolve(
      import.meta.dir,
      `${data}/source-compiler-generated-order-attestation.json`
    ));
    const value = JSON.parse(bytes.toString('utf8'));
    const attestation = parseGeneratedOrderAttestation(value);
    expect(attestation.sourceProjection).toMatchObject({
      semanticPaths: 2_468_434,
      matchedPaths: 2_468_441,
      records: 733_451,
      physicalGroups: 169_649
    });
    expect(attestation.lookupUniverse).toMatchObject({
      comparedSurfaces: 212_198,
      exactSurfaces: 173_111,
      changedSurfaces: 39_087,
      groupingChanges: 9_799,
      orderingOnlyChanges: 29_288,
      winnerChanges: 35_306,
      sourceLocators: 548_607,
      qualifiedLocators: 548_607
    });
    expect(attestation.lookupUniverse.fullEvidence.sha256)
      .toBe('6160662ad10a4c4dade2fef1b11dbfb689cf4f55ab225862f6b77435be2c708e');
    expect(attestation.qualifiedAccounting).toMatchObject({
      declaredAmbiguousSurfaces: 208_352,
      reachableAmbiguousSurfaces: 208_351,
      unreachableSurface: 'コケさせ'
    });
    expect(attestation.releaseGate).toMatchObject({
      source: {
        analyzerSupport: {
          bytes: 949_424,
          sha256: 'f600a57d489a4745184f6cc620a808d7d622e6078e778dbed50f145590a574bb'
        },
        analyzerAnnotations: {
          bytes: 3_421_680,
          sha256: '6b4078d0ae47c0081cfc8db6e9c7f0f10c7c933e8e9ec5158cabe85f5983444e'
        }
      }
    });
    value.lookupUniverse.reversePackedOnlyLocators = 1;
    expect(() => parseGeneratedOrderAttestation(value))
      .toThrow('Generated lookup proof has unresolved reversePackedOnlyLocators');
  });

  test('accepts only the pinned support and annotation identities and counts', async () => {
    const source = {
      ...artifacts('same-root'),
      analyzerSupport: new TextEncoder().encode('source-support'),
      analyzerAnnotations: new TextEncoder().encode('source-annotations')
    };
    const qualified = {
      ...artifacts('same-root'),
      analyzerSupport: new TextEncoder().encode('qualified-support'),
      analyzerAnnotations: new TextEncoder().encode('qualified-annotations')
    };
    const sourceCounts = counts(2, 4);
    const qualifiedCounts = counts(3, 5);
    const value = JSON.parse(await readFile(
      `${data}/source-compiler-generated-order-attestation.json`,
      'utf8'
    ));
    const nullValue = structuredClone(value);
    nullValue.releaseGate = null;
    const nullReview = {
      attestationSha256: 'unpopulated-generated-order',
      attestation: parseGeneratedOrderAttestation(nullValue)
    };
    expect(() => compareQualifiedArtifactCounts(sourceCounts, qualifiedCounts, nullReview))
      .toThrow('Generated-order release gate has no pinned source section identities');
    value.releaseGate = generatedOrderReleaseGateCandidate(
      source,
      qualified,
      sourceCounts,
      qualifiedCounts
    );
    const review = {
      attestationSha256: 'reviewed-generated-order',
      attestation: parseGeneratedOrderAttestation(value)
    };
    expect(compareQualifiedArtifactCounts(sourceCounts, qualifiedCounts, review)).toEqual([
      { path: 'analyzerSupport.suffixKeys', source: 2, qualified: 3 },
      { path: 'annotations.blocks', source: 4, qualified: 5 }
    ]);
    expect(compareQualifiedArtifactBytes(
      source,
      qualified,
      exactRootReview(),
      review
    ).filter(result => result.decision === 'reviewed-generated-order-delta')
      .map(result => result.name)).toEqual(['analyzerSupport', 'analyzerAnnotations']);

    const wrongBytes = {
      ...source,
      analyzerSupport: new TextEncoder().encode('different-source-support')
    };
    expect(() => compareQualifiedArtifactBytes(
      wrongBytes,
      qualified,
      exactRootReview(),
      review
    )).toThrow('Source analyzerSupport identity differs from the generated-order release gate');
    expect(() => compareQualifiedArtifactBytes(
      source,
      source,
      exactRootReview(),
      review
    )).toThrow('Qualified analyzerSupport identity differs from the generated-order release gate');
    expect(() => compareQualifiedArtifactCounts(counts(9, 4), qualifiedCounts, review))
      .toThrow('Source analyzerSupport counts differ from the generated-order release gate');
    expect(() => compareQualifiedArtifactCounts(counts(2, 4, 9), qualifiedCounts, review))
      .toThrow('Unreviewed qualified artifact count differences');
  });

  test('records the identity of every compiler-owned section', () => {
    expect(artifactIdentities(artifacts('root'))).toEqual({
      surfaceIndex: { bytes: 7, sha256: '763cdc62a869262b6ff432a40eae29a00bb96f96f7a3320845abc8cd144d12e2' },
      rootPayload: { bytes: 4, sha256: '4813494d137e1631bba301d5acab6e7bb7aa74ce1185d456565ef51d737677b2' },
      morphology: { bytes: 10, sha256: 'be3be3a7ea1e9c43a444b0336b8b929dc19e69495673afc700af02c44d00918e' },
      analyzerSupport: { bytes: 7, sha256: 'a18603086e5bdf9df88ccc9f5a083fed093e819976e87456b74dafcbd7011114' },
      analyzerAnnotations: { bytes: 11, sha256: '295df243c6a33994c30b6e16aea7ce6155a24b44514ec956d7a09ae0a4cb0411' },
      details: { bytes: 7, sha256: '41ffd8b76afc92a65758fd9a080ae80421c196f251263b87cbfb6e567dda0879' }
    });
  });

  test('parses the existing Rust compiler stats contract', () => {
    expect(parseSurfaceCompilerStats(
      'surfaces=9 accepted=8 direct=2 morphology=7 overlap=1 omitted=1 '
      + 'states=10 edges=11 bytes=12 elapsed_ms=99\n'
    )).toEqual({
      input: 9,
      accepted: 8,
      direct: 2,
      morphology: 7,
      overlap: 1,
      omitted: 1,
      states: 10,
      edges: 11,
      bytes: 12
    });
  });

  test('permits only the named direct-root ordering delta', () => {
    const evidence = new TextEncoder().encode('reviewed ordering evidence\n');
    const result = compareQualifiedArtifactBytes(
      artifacts('source-root'),
      artifacts('qualified-root'),
      {
        sourceSha256: 'c230d44fe4f353089df40a6c13a0f5e5308d917d72a08f15e14424db6b4ff555',
        qualifiedSha256: 'a9375a0f2000c848d958a95be60bc037347cba530cb78886f7c754d151a0805d',
        attestation: evidence,
        attestationSha256: 'f575fc0e29febee4ab912c6cf8668b16cf49b60366ba91a365b138614615de32',
        fullEvidence: {
          rows: 1,
          bytes: 1,
          sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
        },
        provenance: 'qualified ctid audit',
        policy: 'canonical source order',
        preservedBehavior: 'same form set with reviewed tie changes'
      }
    );
    expect(result.find(value => value.name === 'rootPayload')).toMatchObject({
      byteEqual: false,
      decision: 'reviewed-root-order-delta'
    });
    expect(result.filter(value => value.decision === 'exact')).toHaveLength(5);
  });

  test('rejects an unreviewed representation delta', () => {
    const source = artifacts('same-root');
    const qualified = { ...artifacts('same-root'), details: new TextEncoder().encode('changed') };
    expect(() => compareQualifiedArtifactBytes(source, qualified, {
      sourceSha256: '32de7d5e7593fa4f8e752523816f44850e4c2298cb7289645fc2a3d906130ac1',
      qualifiedSha256: '32de7d5e7593fa4f8e752523816f44850e4c2298cb7289645fc2a3d906130ac1',
      attestation: new Uint8Array(),
      attestationSha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      fullEvidence: {
        rows: 0,
        bytes: 0,
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
      },
      provenance: 'none',
      policy: 'none',
      preservedBehavior: 'none'
    })).toThrow('details differs from the qualified baseline');
  });

  test('reports count differences by exact field', () => {
    const counts = (input: number): BrowserAlphaArtifactCounts => ({
      surfaceIndex: { input },
      rootPayload: {},
      morphology: {},
      analyzerSupport: {},
      annotations: {},
      details: { entries: 3 }
    } as unknown as BrowserAlphaArtifactCounts);
    const source = counts(2);
    const qualified = counts(1);
    expect(compareArtifactCounts(source, qualified)).toEqual([
      { path: 'surfaceIndex.input', source: 2, qualified: 1 }
    ]);
  });

  test('imports the source release path while reference modules are blocked', async () => {
    const script = [
      "Bun.plugin({name:'block-reference',setup(builder){",
      "builder.onResolve({filter:/(@ichiran\\/reference-postgres|packages\\/reference-postgres|(^|\\/)postgres$)/},",
      "input=>{throw new Error('source release loaded '+input.path)})}});",
      "await Promise.all([",
      "import('./packages/data/src/source-compiler/canonical-roots.ts?release-smoke=1'),",
      "import('./packages/data/src/source-compiler/generated-projection-stream.ts?release-smoke=1'),",
      "import('./packages/data/src/source-compiler/analyzer-support-stream.ts?release-smoke=1'),",
      "import('./packages/data/src/source-compiler/release-output.ts?release-smoke=1')]);",
      "console.log('source-release-import-ok');"
    ].join('');
    const child = Bun.spawn([process.execPath, '-e', script], {
      cwd: resolve(import.meta.dir, '../../..'),
      stdout: 'pipe',
      stderr: 'pipe',
      env: { ...process.env, ICHIRAN_DB_URL: 'definitely-not-a-valid-url' }
    });
    const [status, stdout, stderr] = await Promise.all([
      child.exited,
      new Response(child.stdout).text(),
      new Response(child.stderr).text()
    ]);
    expect(stderr).toBe('');
    expect(status).toBe(0);
    expect(stdout.trim()).toBe('source-release-import-ok');
  });

  test('does not let update mode disable comparison for the baseline source', async () => {
    const child = Bun.spawn([
      process.execPath,
      'scripts/source-compiler-release.ts',
      'update',
      '--out',
      'work/not-written',
      '--pack-version',
      'test',
      '--source-lock',
      'data/source-compiler-sources.lock.json',
      '--jmdict',
      'packages/data/JMdict_e.gz',
      '--jmdict-source-id',
      'edrdg-jmdict-e-2026-01-01'
    ], {
      cwd: resolve(import.meta.dir, '../../..'),
      stdout: 'pipe',
      stderr: 'pipe'
    });
    const [status, stderr] = await Promise.all([
      child.exited,
      new Response(child.stderr).text()
    ]);
    expect(status).not.toBe(0);
    expect(stderr).toContain('update requires a non-baseline JMdict path and source identity');
  });
});
