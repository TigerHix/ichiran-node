import { describe, expect, test } from 'bun:test';
import { gunzipSync } from 'node:zlib';
import {
  buildAnalyzerAnnotations
} from '../../data/src/browser-pack/analyzer-annotations.js';
import type {
  AnalyzerSupportGeneratedSource,
  AnalyzerSupportHintSource,
  AnalyzerSupportSplitSource
} from '../../data/src/browser-pack/analyzer-support.js';
import {
  ANALYZER_ANNOTATION_CACHE_BLOCKS,
  AnalyzerAnnotationDependencyCollector,
  AnalyzerAnnotationNotLoadedError,
  AnalyzerAnnotationsError,
  AnalyzerAnnotationsReader
} from '../src/analyzer-annotations.js';
import { crc32 } from '../src/crc32.js';

const splits: AnalyzerSupportSplitSource[] = [
  {
    definitionSeq: 1008450,
    route: 'kana',
    surface: 'では',
    kind: 'segsplit',
    parts: [
      {
        seq: 2028980,
        route: 'kana',
        text: 'で',
        best: null,
        ord: 0,
        common: 0,
        commonTags: 'ichi1',
        conjugatable: false,
        nokanji: true,
        generated: [{
          from: 2028980,
          via: false,
          pos: 'cop',
          type: 3,
          negative: false,
          formal: null
        }]
      },
      ':pscore'
    ],
    score: -5,
    primary: 1,
    connector: ' ',
    root: [2028980, 2028920]
  },
  {
    definitionSeq: 123,
    route: 'kanji',
    surface: '例',
    kind: 'split',
    parts: [':score'],
    score: 20,
    primary: 0,
    connector: '',
    root: []
  }
];

const hints: AnalyzerSupportHintSource[] = [
  {
    definitionSeq: 1008450,
    route: 'kana',
    surface: 'では',
    reading: 'では',
    hint: 'で\u200cは'
  }
];

const generated: AnalyzerSupportGeneratedSource = {
  ruleAliases: [0, 1, 2],
  aliasCount: 3,
  records: [
    {
      rootSeq: 10,
      firstAlias: 0,
      secondAlias: null,
      counts: [2, 1],
      physicalGroup: 1,
      members: [
        {
          property: { posId: 0, type: 7, negative: false, formal: false },
          memberOrd: 0,
          propOrd: 0,
          viaMemberOrd: null
        },
        {
          property: { posId: 0, type: 13, negative: null, formal: true },
          memberOrd: 0,
          propOrd: 1,
          viaMemberOrd: null
        },
        {
          property: { posId: 1, type: 9, negative: true, formal: null },
          memberOrd: 1,
          propOrd: 0,
          viaMemberOrd: null
        }
      ]
    },
    {
      rootSeq: 10,
      firstAlias: 1,
      secondAlias: 2,
      counts: null,
      physicalGroup: 1,
      members: [{
        property: { posId: 0, type: 7, negative: false, formal: false },
        memberOrd: 2,
        propOrd: 0,
        viaMemberOrd: 0
      }]
    },
    {
      rootSeq: 20,
      firstAlias: 2,
      secondAlias: null,
      counts: [4, 3],
      physicalGroup: null,
      members: null
    }
  ],
  semanticPaths: 3,
  matchedPaths: 5,
  countExceptions: 2,
  lookupOrders: [
    {
      rootSeq: 10, firstAlias: 0, secondAlias: null, rank: 0
    },
    {
      rootSeq: 20, firstAlias: null, secondAlias: null, rank: 17
    },
    {
      rootSeq: 30, firstAlias: null, secondAlias: null, rank: 38
    }
  ],
  lookupOrderSourceRows: 5,
  lookupOrderSourceSha256: 'fixture-order-source',
  lookupOrderSurfaces: 2,
  lookupOrderClasses: 5,
  lookupOrderEquivalenceClasses: 3,
  lookupOrderComponents: 3,
  lookupOrderCyclicComponents: 1,
  lookupOrderEdges: 2,
  lookupOrderMaxRank: 38,
  lookupOrderProjectionSha256: 'fixture-order',
  lookupOrderExceptions: [{
    route: 'kanji',
    surface: '例外',
    orders: [
      { rootSeq: 10, firstAlias: 0, secondAlias: null, rank: 1 },
      { rootSeq: 20, firstAlias: null, secondAlias: null, rank: 0 }
    ]
  }],
  lookupOrderExceptionClasses: 2,
  lookupOrderExceptionLocators: 2,
  physicalGroups: 1,
  physicalMembers: 4,
  propertyOverrides: 2,
  maxMemberOrd: 2,
  maxViaMemberOrd: 0,
  maxPropOrd: 1,
  projectionSha256: 'fixture'
};

function source(bytes: Uint8Array) {
  return {
    byteLength: bytes.byteLength,
    async read(offset: number, byteLength: number): Promise<Uint8Array> {
      return bytes.slice(offset, offset + byteLength);
    }
  };
}

async function decode(bytes: Uint8Array): Promise<Uint8Array> {
  return gunzipSync(bytes);
}

describe('seekable analyzer annotations', () => {
  test('retains a fixed LRU of decoded split and hint blocks', async () => {
    const values = Array.from(
      { length: ANALYZER_ANNOTATION_CACHE_BLOCKS + 1 },
      (_, index): AnalyzerSupportSplitSource => ({
        ...splits[1]!,
        definitionSeq: 10_000 + index,
        surface: `例${index}`
      })
    );
    let decoded = 0;
    const reader = await AnalyzerAnnotationsReader.open(
      source(buildAnalyzerAnnotations(values, [], generated).bytes),
      async bytes => {
        decoded++;
        return gunzipSync(bytes);
      }
    );

    for (let index = 0; index < ANALYZER_ANNOTATION_CACHE_BLOCKS; index++) {
      expect(await reader.split(10_000 + index, 'kanji', `例${index}`))
        .toEqual(values[index]);
    }
    expect(decoded).toBe(ANALYZER_ANNOTATION_CACHE_BLOCKS);

    // Promote the oldest entry before the next insertion evicts the second.
    expect(await reader.split(10_000, 'kanji', '例0')).toEqual(values[0]);
    expect(decoded).toBe(ANALYZER_ANNOTATION_CACHE_BLOCKS);
    expect(await reader.split(
      10_000 + ANALYZER_ANNOTATION_CACHE_BLOCKS,
      'kanji',
      `例${ANALYZER_ANNOTATION_CACHE_BLOCKS}`
    )).toEqual(values[ANALYZER_ANNOTATION_CACHE_BLOCKS]);
    expect(decoded).toBe(ANALYZER_ANNOTATION_CACHE_BLOCKS + 1);

    expect(await reader.split(10_000, 'kanji', '例0')).toEqual(values[0]);
    expect(decoded).toBe(ANALYZER_ANNOTATION_CACHE_BLOCKS + 1);
    expect(await reader.split(10_001, 'kanji', '例1')).toEqual(values[1]);
    expect(decoded).toBe(ANALYZER_ANNOTATION_CACHE_BLOCKS + 2);
  });

  test('rejects exception spans that cannot fit the on-disk count', () => {
    expect(() => buildAnalyzerAnnotations(splits, hints, {
      ...generated,
      lookupOrderExceptions: [{
        route: 'kanji',
        surface: '過大',
        orders: Array.from(
          { length: 0x1_0000 },
          () => generated.lookupOrderExceptions[0]!.orders[0]!
        )
      }],
      lookupOrderExceptionClasses: 2,
      lookupOrderExceptionLocators: 0x1_0000
    })).toThrow('locator count');
  });

  test('batch-preloads dependencies recorded by a discarded analyzer pass', async () => {
    const dependencies = new AnalyzerAnnotationDependencyCollector();
    expect(dependencies.split(123, 'kanji', '例')).toBeNull();
    expect(dependencies.hint(1008450, 'kana', 'では', 'では')).toBeNull();
    expect(dependencies.generated(10, [0])).toBeNull();
    expect(dependencies.generated(10, [1, 2])).toBeNull();
    expect(dependencies.generated(20, [2])).toBeNull();
    expect(dependencies.lookupOrder('kanji', '通常', 30, null)).toBe(0);

    expect([...dependencies.definitionSeqs]).toEqual([123, 1008450]);
    expect([...dependencies.generatedRootSeqs]).toEqual([10, 20, 30]);

    const reader = await AnalyzerAnnotationsReader.open(
      source(buildAnalyzerAnnotations(splits, hints, generated).bytes),
      decode
    );
    const preloaded = reader.createPreloaded();
    await preloaded.preloadDependencies(dependencies);

    expect(preloaded.split(123, 'kanji', '例')).toEqual(splits[1]);
    expect(preloaded.hint(1008450, 'kana', 'では', 'では')).toBe('で\u200cは');
    expect(preloaded.generated(10, [0])?.physicalGroup).toBe(1);
    expect(preloaded.generated(20, [2])?.nKanji).toBe(4);
    expect(preloaded.lookupOrder('kanji', '通常', 30, null)).toBe(38);
    expect(preloaded.lookupOrder('kanji', '例外', 20, null)).toBe(0);
    expect(preloaded.lookupOrder('kanji', '例外', 30, null)).toBeNull();
    expect(preloaded.loadedGeneratedRoots).toBe(3);
  });

  test('prewarms generated blocks and snapshots Reader caches into request views', async () => {
    let decoded = 0;
    const reader = await AnalyzerAnnotationsReader.open(
      source(buildAnalyzerAnnotations(splits, hints, generated).bytes),
      async bytes => {
        decoded++;
        return gunzipSync(bytes);
      }
    );

    expect(await reader.split(123, 'kanji', '例')).toEqual(splits[1]);
    await reader.preloadAllGenerated();
    expect(decoded).toBe(2);

    const first = reader.createPreloaded();
    expect(first.loadedBlocks).toBe(1);
    expect(first.loadedGeneratedBlocks).toBe(reader.manifest.generatedBlocks);
    expect(first.split(123, 'kanji', '例')).toEqual(splits[1]);
    expect(first.generated(10, [0])?.physicalGroup).toBe(1);
    expect(first.lookupOrder('kanji', '通常', 30, null)).toBe(38);
    expect(decoded).toBe(2);

    first.clear();
    expect(first.loadedBlocks).toBe(0);
    expect(first.loadedGeneratedBlocks).toBe(0);

    // Clearing one request view cannot clear either shared Reader cache.
    const second = reader.createPreloaded();
    expect(second.split(123, 'kanji', '例')).toEqual(splits[1]);
    expect(second.generated(20, [2])?.nKanji).toBe(4);
    expect(decoded).toBe(2);
  });

  test('round-trips deterministic per-definition blocks', async () => {
    const build = buildAnalyzerAnnotations(splits, hints, generated);
    const reversed = buildAnalyzerAnnotations(
      [...splits].reverse(),
      [...hints].reverse(),
      {
        ...generated,
        records: [...generated.records].reverse().map(record => ({
          ...record,
          members: record.members === null ? null : [...record.members].reverse()
        })),
        lookupOrderExceptions: [...generated.lookupOrderExceptions].reverse().map(exception => ({
          ...exception,
          orders: [...exception.orders].reverse()
        }))
      }
    );
    expect(reversed.bytes).toEqual(build.bytes);
    expect(build.stats.blocks).toBe(2);
    // Three semantic keys become five cold rows because one key carries three
    // distinct physical conj_prop members. Repeated low-22 keys are data.
    expect(build.stats.generatedRecords).toBe(5);
    expect(build.stats.lookupOrderRecords).toBe(3);
    expect(build.stats.lookupOrderSurfaces).toBe(2);
    expect(build.stats.lookupOrderExceptionSurfaces).toBe(1);
    expect(build.stats.lookupOrderExceptionClasses).toBe(2);
    expect(build.stats.lookupOrderExceptionLocators).toBe(2);

    const reader = await AnalyzerAnnotationsReader.open(source(build.bytes), decode);
    expect(await reader.split(123, 'kanji', '例')).toEqual(splits[1]);
    expect(await reader.split(1008450, 'kana', 'では', 'segsplit')).toEqual(splits[0]);
    expect(await reader.split(1008450, 'kana', 'では')).toBeNull();
    expect(await reader.hint(1008450, 'kana', 'では', 'では')).toBe('で\u200cは');
    expect(await reader.hint(999, 'kana', 'では', 'では')).toBeNull();
    expect(await reader.generated(10, [0])).toEqual({
      nKanji: 2,
      nKana: 1,
      physicalGroup: 1,
      members: [
        {
          property: { posId: 0, type: 7, negative: false, formal: false },
          memberOrd: 0,
          propOrd: 0,
          viaMemberOrd: null
        },
        {
          property: { posId: 0, type: 13, negative: null, formal: true },
          memberOrd: 0,
          propOrd: 1,
          viaMemberOrd: null
        },
        {
          property: { posId: 1, type: 9, negative: true, formal: null },
          memberOrd: 1,
          propOrd: 0,
          viaMemberOrd: null
        }
      ]
    });
    expect(await reader.generated(10, [1, 2])).toEqual({
      nKanji: null,
      nKana: null,
      physicalGroup: 1,
      members: [{
        property: { posId: 0, type: 7, negative: false, formal: false },
        memberOrd: 2,
        propOrd: 0,
        viaMemberOrd: 0
      }]
    });
    expect(await reader.generated(20, [2])).toEqual({
      nKanji: 4,
      nKana: 3,
      physicalGroup: null,
      members: null
    });
    expect(await reader.lookupOrder('kanji', '通常', 10, [0])).toBe(0);
    expect(await reader.lookupOrder('kanji', '通常', 20, null)).toBe(17);
    expect(await reader.lookupOrder('kanji', '通常', 30, null)).toBe(38);
    expect(await reader.lookupOrder('kanji', '通常', 10, [2])).toBeNull();
    expect(await reader.lookupOrder('kanji', '例外', 20, null)).toBe(0);
    expect(await reader.lookupOrder('kanji', '例外', 10, [0])).toBe(1);
    // Once the exact surface is exceptional, a missing local locator must not
    // silently fall back to its otherwise-present global level.
    expect(await reader.lookupOrder('kanji', '例外', 30, null)).toBeNull();
    expect(await reader.lookupOrder('kana', '例外', 30, null)).toBe(38);
    expect(await reader.generated(999, [0])).toBeNull();

    // Use a fresh Reader so this block still covers the explicit miss/retry
    // protocol rather than the cache snapshots exercised above.
    const lazyReader = await AnalyzerAnnotationsReader.open(source(build.bytes), decode);
    const preloaded = await lazyReader.preload([1008450, 1008450, 999]);
    expect(preloaded.split(1008450, 'kana', 'では', 'segsplit')).toEqual(splits[0]);
    expect(preloaded.hint(1008450, 'kana', 'では', 'では')).toBe('で\u200cは');
    expect(() => preloaded.split(123, 'kanji', '例'))
      .toThrow(AnalyzerAnnotationNotLoadedError);
    let missing: AnalyzerAnnotationNotLoadedError | null = null;
    try {
      preloaded.generated(10, [0]);
    } catch (error) {
      if (error instanceof AnalyzerAnnotationNotLoadedError) missing = error;
      else throw error;
    }
    expect(missing?.kind).toBe('generated');
    expect(missing?.blockIndex).toBe(0);
    await preloaded.preloadMissing(missing!);
    expect(preloaded.generated(10, [0])?.nKanji).toBe(2);
    expect(preloaded.generated(10, [1, 2])?.physicalGroup).toBe(1);
    // A miss pins the compact decoded block, so every root in that block is
    // synchronously available on the single retry.
    expect(preloaded.loadedGeneratedBlocks).toBe(1);
    expect(preloaded.loadedGeneratedRoots).toBe(1);
    expect(preloaded.generated(20, [2])?.nKanji).toBe(4);
    expect(preloaded.lookupOrder('kanji', '通常', 20, null)).toBe(17);
    expect(preloaded.lookupOrder('kanji', '通常', 30, null)).toBe(38);
    // Facts and orders for other roots in the compact block are read directly;
    // the preload must not eagerly expand those roots into JavaScript maps.
    expect(preloaded.loadedGeneratedRoots).toBe(1);
    preloaded.clear();
    expect(preloaded.loadedBlocks).toBe(0);
    expect(preloaded.loadedGeneratedRoots).toBe(0);
    expect(preloaded.loadedGeneratedBlocks).toBe(0);
    expect(() => preloaded.hint(1008450, 'kana', 'では', 'では'))
      .toThrow(AnalyzerAnnotationNotLoadedError);
  });

  test('validates the resident index and each decompressed block', async () => {
    const encoded = buildAnalyzerAnnotations(splits, hints, generated).bytes;
    const badIndex = encoded.slice();
    badIndex[96] ^= 1;
    await expect(AnalyzerAnnotationsReader.open(source(badIndex), decode))
      .rejects.toBeInstanceOf(AnalyzerAnnotationsError);

    const badException = encoded.slice();
    const exceptionView = new DataView(badException.buffer);
    const exceptionHeaderBytes = exceptionView.getUint16(10, true);
    const exceptionDataOffset = exceptionView.getUint32(44, true);
    const exceptionLocatorsOffset = exceptionView.getUint32(172, true);
    const secondPackedAt = exceptionLocatorsOffset + 8 + 4;
    const secondPacked = exceptionView.getUint32(secondPackedAt, true);
    exceptionView.setUint32(
      secondPackedAt,
      (secondPacked & 0x003f_ffff) | (1 << 22),
      true
    );
    exceptionView.setUint32(
      20,
      crc32(badException.subarray(exceptionHeaderBytes, exceptionDataOffset)),
      true
    );
    const exceptionHeader = badException.slice(0, exceptionHeaderBytes);
    new DataView(exceptionHeader.buffer).setUint32(16, 0, true);
    exceptionView.setUint32(16, crc32(exceptionHeader), true);
    await expect(AnalyzerAnnotationsReader.open(source(badException), decode))
      .rejects.toMatchObject({ code: 'corrupt-index' });

    const badFactPadding = encoded.slice();
    const paddingView = new DataView(badFactPadding.buffer);
    const paddingHeaderBytes = paddingView.getUint16(10, true);
    const paddingDataOffset = paddingView.getUint32(44, true);
    const factsEnd = paddingView.getUint32(92, true) + paddingView.getUint32(68, true) * 2;
    const exceptionEntriesOffset = paddingView.getUint32(168, true);
    expect(factsEnd).toBeLessThan(exceptionEntriesOffset);
    badFactPadding[factsEnd] = 1;
    paddingView.setUint32(
      20,
      crc32(badFactPadding.subarray(paddingHeaderBytes, paddingDataOffset)),
      true
    );
    const paddingHeader = badFactPadding.slice(0, paddingHeaderBytes);
    new DataView(paddingHeader.buffer).setUint32(16, 0, true);
    paddingView.setUint32(16, crc32(paddingHeader), true);
    await expect(AnalyzerAnnotationsReader.open(source(badFactPadding), decode))
      .rejects.toMatchObject({ code: 'corrupt-index' });

    const badDataPadding = encoded.slice();
    const dataPaddingView = new DataView(badDataPadding.buffer);
    const dataPaddingStart = dataPaddingView.getUint32(44, true)
      + dataPaddingView.getUint32(48, true);
    const paddedGeneratedDataOffset = dataPaddingView.getUint32(96, true);
    expect(dataPaddingStart).toBeLessThan(paddedGeneratedDataOffset);
    badDataPadding[dataPaddingStart] = 1;
    await expect(AnalyzerAnnotationsReader.open(source(badDataPadding), decode))
      .rejects.toMatchObject({ code: 'corrupt-index' });

    const badBlock = encoded.slice();
    const dataOffset = new DataView(badBlock.buffer).getUint32(44, true);
    badBlock[dataOffset] ^= 1;
    const reader = await AnalyzerAnnotationsReader.open(source(badBlock), decode);
    await expect(reader.split(123, 'kanji', '例'))
      .rejects.toBeInstanceOf(AnalyzerAnnotationsError);

    const badGenerated = encoded.slice();
    const generatedDataOffset = new DataView(badGenerated.buffer).getUint32(96, true);
    badGenerated[generatedDataOffset] ^= 1;
    const generatedReader = await AnalyzerAnnotationsReader.open(source(badGenerated), decode);
    await expect(generatedReader.generated(10, [0]))
      .rejects.toBeInstanceOf(AnalyzerAnnotationsError);

    const badTriState = encoded.slice();
    const view = new DataView(badTriState.buffer);
    const headerBytes = view.getUint16(10, true);
    const annotationDataOffset = view.getUint32(44, true);
    const generatedBlocksOffset = view.getUint32(84, true);
    const triGeneratedDataOffset = view.getUint32(96, true);
    const generatedRelative = view.getUint32(generatedBlocksOffset + 4, true);
    const generatedCompressed = view.getUint32(generatedBlocksOffset + 8, true);
    const generatedBlock = new Uint8Array(gunzipSync(badTriState.subarray(
      triGeneratedDataOffset + generatedRelative,
      triGeneratedDataOffset + generatedRelative + generatedCompressed
    )));
    const generatedView = new DataView(generatedBlock.buffer);
    const generatedRoots = generatedView.getUint32(0, true);
    const firstProperty = 12 + generatedRoots * 20 + 8;
    generatedView.setUint16(
      firstProperty,
      (generatedView.getUint16(firstProperty, true) & ~(3 << 11)) | (3 << 11),
      true
    );
    view.setUint32(generatedBlocksOffset + 16, crc32(generatedBlock), true);
    view.setUint32(20, crc32(badTriState.subarray(headerBytes, annotationDataOffset)), true);
    const repairedHeader = badTriState.slice(0, headerBytes);
    new DataView(repairedHeader.buffer).setUint32(16, 0, true);
    view.setUint32(16, crc32(repairedHeader), true);
    const badTriReader = await AnalyzerAnnotationsReader.open(
      source(badTriState),
      async (bytes, expectedBytes) => expectedBytes === generatedBlock.byteLength
        ? generatedBlock
        : gunzipSync(bytes)
    );
    await expect(badTriReader.generated(10, [0]))
      .rejects.toMatchObject({ code: 'corrupt-block' });
  });
});
