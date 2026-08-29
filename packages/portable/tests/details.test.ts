import { gunzipSync } from 'node:zlib';
import { describe, expect, test } from 'bun:test';
import { buildDetailStore, type DetailEntrySource } from '../../data/src/browser-pack/details.js';
import {
  DetailStoreError,
  DETAILS_HEADER_BYTES,
  memoryDetailSource,
  openDetailStore
} from '../src/details.js';

const FIXTURE: readonly DetailEntrySource[] = [
  {
    seq: 100,
    forms: [
      {
        route: 'kanji',
        text: '食べる',
        ord: 0,
        common: 1,
        commonTags: 'ichi1;news1',
        conjugatable: true,
        nokanji: false,
        best: 'たべる'
      },
      {
        route: 'kana',
        text: 'たべる',
        ord: 0,
        common: null,
        commonTags: '',
        conjugatable: true,
        nokanji: false,
        best: '食べる'
      }
    ],
    senses: [{
      ord: 0,
      glosses: [{ ord: 0, text: 'to eat' }, { ord: 1, text: 'consume' }],
      properties: [
        { tag: 'field', ord: 0, text: 'food' },
        { tag: 'pos', ord: 0, text: 'v1' }
      ]
    }]
  },
  {
    seq: 200,
    forms: [{
      route: 'kana',
      text: 'だけ',
      ord: 0,
      common: 0,
      commonTags: 'ichi1',
      conjugatable: false,
      nokanji: true,
      best: null
    }],
    senses: [
      {
        ord: 0,
        glosses: [{ ord: 0, text: 'first' }],
        properties: [{ tag: 'misc', ord: 0, text: 'uk' }]
      },
      {
        ord: 1,
        glosses: [{ ord: 0, text: '第二の意味' }],
        properties: [{ tag: 's_inf', ord: 0, text: 'context note' }]
      }
    ]
  },
  { seq: 300, forms: [], senses: [] }
];

const decodeGzip = async (bytes: Uint8Array): Promise<Uint8Array> => (
  new Uint8Array(gunzipSync(bytes))
);

describe('random-access detail store', () => {
  test('builds deterministically and reads exact entries', async () => {
    const first = buildDetailStore(FIXTURE, { targetBlockBytes: 1024 });
    const second = buildDetailStore([...FIXTURE].reverse(), { targetBlockBytes: 1024 });
    expect(first.bytes).toEqual(second.bytes);
    expect(first.stats.entryCount).toBe(3);
    expect(first.stats.formCount).toBe(3);
    expect(first.stats.senseCount).toBe(3);
    expect(first.stats.glossCount).toBe(4);
    expect(first.stats.propertyCount).toBe(4);

    const reader = await openDetailStore(memoryDetailSource(first.bytes), decodeGzip);
    expect(reader.manifest.entryCount).toBe(3);
    expect(await reader.entry(0)).toEqual(FIXTURE[0]);
    expect(await reader.entry(1)).toEqual(FIXTURE[1]);
    expect(await reader.entry(2)).toEqual(FIXTURE[2]);
  });

  test('loads only the index and requested compressed block', async () => {
    const build = buildDetailStore(FIXTURE, { targetBlockBytes: 1024 });
    const reads: Array<[number, number]> = [];
    const memory = memoryDetailSource(build.bytes);
    const source = {
      byteLength: memory.byteLength,
      async read(offset: number, byteLength: number) {
        reads.push([offset, byteLength]);
        return memory.read(offset, byteLength);
      }
    };
    const reader = await openDetailStore(source, decodeGzip);
    expect(reads).toHaveLength(2);
    await reader.entry(0);
    expect(reads).toHaveLength(3);
    await reader.entry(1);
    expect(reads).toHaveLength(3);
    reader.clearCache();
    await reader.entry(1);
    expect(reads).toHaveLength(4);
  });

  test('rejects header, index, and deferred block corruption', async () => {
    const build = buildDetailStore(FIXTURE, { targetBlockBytes: 1024 });
    const badHeader = build.bytes.slice();
    badHeader[0] ^= 0xff;
    await expect(openDetailStore(memoryDetailSource(badHeader), decodeGzip)).rejects.toMatchObject({
      code: 'invalid-header'
    });

    const badIndex = build.bytes.slice();
    badIndex[DETAILS_HEADER_BYTES] ^= 0xff;
    await expect(openDetailStore(memoryDetailSource(badIndex), decodeGzip)).rejects.toMatchObject({
      code: 'corrupt-index'
    });

    const reader = await openDetailStore(memoryDetailSource(build.bytes), async (bytes, expected) => {
      const decoded = await decodeGzip(bytes);
      const corrupt = decoded.slice();
      corrupt[Math.min(5, expected - 1)] ^= 0xff;
      return corrupt;
    });
    await expect(reader.entry(0)).rejects.toMatchObject({ code: 'corrupt-block' });
    await expect(reader.entry(3)).rejects.toBeInstanceOf(DetailStoreError);
    await expect(reader.entry(3)).rejects.toMatchObject({ code: 'out-of-range' });
  });
});
