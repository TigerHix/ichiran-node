import { describe, expect, test } from 'bun:test';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { BrowserAlphaArtifactCounts } from '../src/browser-pack/release-orchestration.js';
import {
  parseGeneratedOrderAttestation,
  parseRootPayloadOrderAttestation,
  type GeneratedOrderReleaseGate
} from '../src/source-compiler/release-evidence.js';
import {
  artifactIdentities,
  compareArtifactCounts,
  compareQualifiedArtifactBytes,
  compareQualifiedArtifactCounts,
  parseSurfaceCompilerStats,
  type QualifiedArtifactBytes
} from '../src/source-compiler/release-comparison.js';
import { assertSourceCompilerReleaseMode } from '../src/source-compiler/source-lock.js';

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
    surfaceIndex: {
      input: 1, accepted: 0, direct: 0, morphology: 0, overlap: 0, omitted: 0,
      states: 0, edges: 0
    },
    rootPayload: { surfaces: 1, forms: 0, entries: 0, restrictions: 0 },
    morphology: {
      positions, rules: 0, templates: 0, suffixes: 0, rootKeys: 0, rootGroups: 0,
      patches: 0, tombstones: 0
    },
    analyzerSupport: {
      suffixKeys: support, suffixValues: 0, suffixClasses: 0, counterKeys: 0,
      counterVariants: 0, collisions: 0, generatedRules: 0, generatedAliases: 0
    },
    annotations: {
      blocks: annotations, splits: 0, hints: 0, generatedBlocks: 0, generatedRoots: 0,
      generatedRecords: 0, lookupOrderRecords: 0, lookupOrderRoots: 0,
      lookupOrderBytes: 0, lookupOrderExceptionSurfaces: 0,
      lookupOrderExceptionClasses: 0, lookupOrderExceptionLocators: 0,
      lookupOrderExceptionBytes: 0, generatedPhysicalGroups: 0, generatedFactPairs: 0,
      indexBytes: 0, uncompressedBytes: 0, compressedBytes: 0,
      annotationUncompressedBytes: 0, annotationCompressedBytes: 0,
      generatedUncompressedBytes: 0, generatedCompressedBytes: 0, totalBytes: 0,
      largestUncompressedBlock: 0, largestGeneratedBlock: 0,
      largestGeneratedCompressedBlock: 0
    },
    details: { entries: 3, forms: 0, senses: 0, glosses: 0, properties: 0 }
  };
}

function testReleaseGate(
  source: QualifiedArtifactBytes,
  qualified: QualifiedArtifactBytes,
  sourceCounts: BrowserAlphaArtifactCounts,
  qualifiedCounts: BrowserAlphaArtifactCounts
): GeneratedOrderReleaseGate {
  const sourceIdentities = artifactIdentities(source);
  const qualifiedIdentities = artifactIdentities(qualified);
  return {
    source: {
      analyzerSupport: {
        ...sourceIdentities.analyzerSupport,
        counts: { ...sourceCounts.analyzerSupport }
      },
      analyzerAnnotations: {
        ...sourceIdentities.analyzerAnnotations,
        counts: { ...sourceCounts.annotations }
      }
    },
    qualified: {
      analyzerSupport: {
        ...qualifiedIdentities.analyzerSupport,
        counts: { ...qualifiedCounts.analyzerSupport }
      },
      analyzerAnnotations: {
        ...qualifiedIdentities.analyzerAnnotations,
        counts: { ...qualifiedCounts.annotations }
      }
    }
  };
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
    expect(attestation.releaseGate.source.analyzerSupport.counts).not.toEqual({});
    expect(attestation.releaseGate.source.analyzerAnnotations.counts).not.toEqual({});
    const missingGate = structuredClone(value);
    delete missingGate.releaseGate;
    expect(() => parseGeneratedOrderAttestation(missingGate))
      .toThrow('Generated-order attestation omits its atomic release gate');
    const nullGate = structuredClone(value);
    nullGate.releaseGate = null;
    expect(() => parseGeneratedOrderAttestation(nullGate))
      .toThrow('Generated-order release gate must be an object');
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
    value.releaseGate = testReleaseGate(
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
    const base = counts(0, 0);
    const source = { ...base, surfaceIndex: { ...base.surfaceIndex, input: 2 } };
    const qualified = { ...base, surfaceIndex: { ...base.surfaceIndex, input: 1 } };
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

  test('does not let update mode disable comparison for renamed baseline bytes', () => {
    expect(() => assertSourceCompilerReleaseMode('update', {
      sha256: '92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7'
    })).toThrow('Update mode cannot use the qualified baseline JMdict identity');
    expect(() => assertSourceCompilerReleaseMode('baseline', {
      sha256: '34cc33abe2ae37a8572a9a45ce68c5e7fb6ccccd55c021366eb4fa6c49f6c90c'
    })).toThrow('Baseline mode requires the qualified baseline JMdict identity');
  });
});
