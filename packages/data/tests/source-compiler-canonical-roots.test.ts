import { beforeAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { buildDetailStore } from '../src/browser-pack/details.js';
import { buildMorphology } from '../src/browser-pack/morphology-compiler.js';
import { buildRootPayload } from '../src/browser-pack/root-payload.js';
import { compileCanonicalRoots, type CanonicalRootCompilation } from '../src/source-compiler/canonical-roots.js';
import { foldChronologicalConjugationErrata } from '../src/source-compiler/conjugation-errata.js';
import { conjugationPositionCompatibility } from '../src/source-compiler/compatibility.js';
import { canonicalEntriesDigest } from '../src/source-compiler/digest.js';
import { canonicalMorphologySource } from '../src/source-compiler/morphology-input.js';
import { canonicalDetailEntries, canonicalRootPayloadSource } from '../src/source-compiler/pack-input.js';

const paths = {
  jmdict: fileURLToPath(new URL('../JMdict_e.gz', import.meta.url)),
  extra: fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url)),
  municipality: fileURLToPath(new URL('../../../data/sources/jichitai.csv', import.meta.url)),
  ward: fileURLToPath(new URL('../../../data/sources/gyoseiku.csv', import.meta.url)),
  errata: fileURLToPath(new URL('../../../data/source-compiler-errata.json', import.meta.url)),
  compatibility: fileURLToPath(new URL('../../../data/source-compiler-compatibility.json', import.meta.url))
};

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

let compilation: CanonicalRootCompilation;

beforeAll(async () => {
  compilation = await compileCanonicalRoots(paths);
}, 30_000);

describe('complete canonical roots', () => {
  test('pins the chronological and compatibility ledger bytes', async () => {
    const errata = await readFile(paths.errata);
    const compatibility = await readFile(paths.compatibility);
    expect([errata.byteLength, sha256(errata)]).toEqual([
      186_000,
      '7f78b244955c14e23afc5474b03c66554cfba189bf0383856afd8a00bd279f24'
    ]);
    expect([compatibility.byteLength, sha256(compatibility)]).toEqual([
      7_952,
      '6e867889e87d43999163d3fd6fa4630a2c39253cc5d63b2484af3aad5e01c51e'
    ]);
  });

  test('compile from pinned source files with PostgreSQL unavailable', async () => {
    expect(compilation.jmdictEntries).toBe(214_698);
    expect(compilation.entries).toHaveLength(217_967);
    expect(compilation.custom.createdRoots).toHaveLength(3_270);
    expect(compilation.custom.updatedEntries).toHaveLength(220);
    expect(compilation.custom.edits).toHaveLength(3_703);
    expect(compilation.errata.counts).toEqual({
      declared: 601,
      applied: 508,
      noops: 93,
      demotedRoots: 1
    });
    expect(compilation.compatibility.rows).toHaveLength(11);
    expect(await canonicalEntriesDigest(compilation.entries)).toEqual({
      entries: 217_967,
      sha256: '6057e732f38eb3c35ca703ea4a6145ead929d4d62b186559ddfa6f65ec871f39'
    });
  });

  test('emits the qualified detail bytes exactly', () => {
    const details = buildDetailStore(canonicalDetailEntries(compilation.entries));
    expect(details.stats).toMatchObject({
      entryCount: 217_967,
      formCount: 492_913,
      senseCount: 251_648,
      glossCount: 434_112,
      propertyCount: 407_620,
      totalBytes: 13_555_874
    });
    expect(sha256(details.bytes)).toBe(
      '0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151'
    );
  });

  test('emits the approved deterministic direct-root rebaseline', () => {
    const root = buildRootPayload(canonicalRootPayloadSource(compilation.entries));
    expect(root.stats.counts).toMatchObject({
      surfaces: 443_275,
      forms: 476_178,
      entries: 217_967,
      restrictions: 6_332
    });
    expect(root.stats.directOrderProjection.sha256).toBe(
      '0302b424f6ee6ed1c2b5c28dd9c59a5d9980e0d3107219887ae24f20a83e7b07'
    );
    expect(root.bytes).toHaveLength(9_088_056);
    expect(sha256(root.bytes)).toBe(
      '19204bdae9ec44f7a5240aa7b74e83cf302a8f8da09b4a1748445ef0dd5dc8d2'
    );
  }, 30_000);

  test('emits the qualified morphology bytes exactly', () => {
    const fold = foldChronologicalConjugationErrata(
      compilation.entries,
      compilation.errata.conjugationRows
    );
    const source = canonicalMorphologySource(
      compilation.entries,
      conjugationPositionCompatibility(compilation.compatibility),
      fold.manualPatches
    );
    const morphology = buildMorphology(source);
    expect(morphology.stats).toMatchObject({
      bytes: 2_688_176,
      positions: 22,
      rules: 1_161,
      templates: 7_211,
      rootRows: 41_962,
      rootKeys: 40_882,
      rootGroups: 14_608,
      rootForms: 41_964,
      patches: 50,
      tombstones: 4
    });
    expect(sha256(morphology.bytes)).toBe(
      '1614d150f3609b9de4f93de5ad0e33e12aec41211dc9096870a8d019eab9c0f3'
    );
  }, 30_000);
});
