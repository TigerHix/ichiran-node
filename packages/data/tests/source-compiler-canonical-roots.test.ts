import { beforeAll, describe, expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { buildLexiconStore } from '../src/browser-pack/lexicon.js';
import { buildLocaleGlossStore } from '../src/browser-pack/locale-gloss.js';
import { buildMorphology } from '../src/browser-pack/morphology-compiler.js';
import { buildRootPayload } from '../src/browser-pack/root-payload.js';
import { compileCanonicalRoots, type CanonicalRootCompilation } from '../src/source-compiler/canonical-roots.js';
import { foldChronologicalConjugationErrata } from '../src/source-compiler/conjugation-errata.js';
import {
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility,
  omitsConjugationReadingLineage
} from '../src/source-compiler/compatibility.js';
import { emitConfiguredConjugations } from '../src/source-compiler/conjugation-emission-order.js';
import { conjugationSourceKey } from '../src/source-compiler/conjugation-emissions.js';
import { canonicalEntriesDigest } from '../src/source-compiler/digest.js';
import { canonicalMorphologySource } from '../src/source-compiler/morphology-input.js';
import {
  canonicalEnglishLocaleEntries,
  canonicalLexiconEntries,
  canonicalRootPayloadSource
} from '../src/source-compiler/pack-input.js';

const paths = {
  jmdict: fileURLToPath(new URL('../JMdict_e.gz', import.meta.url)),
  extra: fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url)),
  municipality: fileURLToPath(new URL('../../../data/sources/jichitai.csv', import.meta.url)),
  ward: fileURLToPath(new URL('../../../data/sources/gyoseiku.csv', import.meta.url)),
  errata: fileURLToPath(new URL('../../../data/source-compiler-errata.json', import.meta.url)),
  compatibility: fileURLToPath(new URL('../../../data/source-compiler-compatibility.json', import.meta.url))
};
const conjugationRules = {
  kwpos: fileURLToPath(new URL('../../../data/kwpos.csv', import.meta.url)),
  conjo: fileURLToPath(new URL('../../../data/conjo.csv', import.meta.url))
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
      23_683,
      '387982639a9a03ea7fb11feeef754b5aea59c673cec2757153f200ea071ee9bb'
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
    expect(compilation.errata.noopRowIds).toHaveLength(93);
    expect(new Set(compilation.errata.noopRowIds).size).toBe(93);
    expect(sha256(Buffer.from(compilation.errata.noopRowIds.join('\n')))).toBe(
      'ade55c5ea7902a6370849a5e380fa23a93b98f404b690e0cd8b21276eb5029c2'
    );
    expect(compilation.compatibility.rows).toHaveLength(25);
    expect(conjugationReadingLineageCompatibility(compilation.compatibility)).toHaveLength(7);
    expect(await canonicalEntriesDigest(compilation.entries)).toEqual({
      entries: 217_967,
      sha256: '6057e732f38eb3c35ca703ea4a6145ead929d4d62b186559ddfa6f65ec871f39'
    });
  });

  test('names the seven qualified コケる physical-lineage omissions', () => {
    const entry = compilation.entries.find(value => value.seq === 1_593_170);
    if (!entry) throw new Error('Missing qualified コケる root');
    const rows = conjugationReadingLineageCompatibility(compilation.compatibility);
    const emissions = emitConfiguredConjugations(entry, {
      positions: ['v1'],
      sourcesByPosition: new Map([[
        'v1',
        new Set([conjugationSourceKey('kana', 'コケる')])
      ]])
    });
    const omitted = emissions.flatMap(emission => emission.forms.filter(form => rows.some(row =>
      omitsConjugationReadingLineage(row, {
        rootSeq: emission.rootSeq,
        route: form.route,
        sourceText: form.sourceText,
        firstRule: form.firstRule,
        secondRule: form.secondRule
      }))));
    expect(rows.map(row => row.id)).toHaveLength(7);
    expect(omitted).toHaveLength(54);
  });

  test('emits deterministic language-neutral lexicon and English locale bytes', () => {
    const lexicon = buildLexiconStore(canonicalLexiconEntries(compilation.entries));
    expect(lexicon.stats).toMatchObject({
      entryCount: 217_967,
      formCount: 492_913,
      senseCount: 251_648,
      propertyCount: 401_254,
      totalBytes: 8_159_535
    });
    const lexiconSha256 = sha256(lexicon.bytes);
    expect(lexiconSha256).toBe(
      '74bb932decb2e24b8faa861cc015e4dd87e5d1ca6d549465c5276c5f297ed42d'
    );
    const english = buildLocaleGlossStore({
      locale: 'en',
      lexiconSha256,
      entries: canonicalEnglishLocaleEntries(compilation.entries)
    });
    expect(english.stats).toMatchObject({
      entryCount: 217_967,
      translatedEntryCount: 217_967,
      groupCount: 251_648,
      targetCount: 251_648,
      glossCount: 434_112,
      infoCount: 6_366,
      totalBytes: 7_108_607
    });
    expect(sha256(english.bytes)).toBe(
      '56812d5033e88ed386b2520eb8f90cc35b7840937a8e916a2326d156d1586a7c'
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
      compilation.errata.conjugationRows,
      { conjugationRules }
    );
    const source = canonicalMorphologySource(
      compilation.entries,
      conjugationPositionCompatibility(compilation.compatibility),
      fold.manualPatches
    );
    const morphology = buildMorphology(source, { conjugationRules });
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
