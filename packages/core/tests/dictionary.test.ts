import { createHash } from 'node:crypto';
import { gunzipSync } from 'node:zlib';
import { describe, expect, test } from 'bun:test';
import { buildLexiconStore } from '../../data/src/browser-pack/lexicon.js';
import { buildLocaleGlossStore } from '../../data/src/browser-pack/locale-gloss.js';
import {
  DictionaryReader,
  DictionaryStoreError,
  LexiconStoreReader,
  LocaleGlossStoreReader,
  localizeEntry,
  memoryDictionarySource
} from '../src/dictionary.js';

const decodeGzip = async (bytes: Uint8Array): Promise<Uint8Array> => new Uint8Array(gunzipSync(bytes));

function fixtures() {
  const lexicon = buildLexiconStore([{
    seq: 100,
    forms: [{
      route: 'kanji', text: '食べる', ord: 0, common: 1, commonTags: 'ichi1',
      conjugatable: true, nokanji: false, best: 'たべる'
    }],
    senses: [
      { ord: 0, properties: [{ tag: 'pos', ord: 0, text: 'v1' }] },
      { ord: 1, properties: [{ tag: 'field', ord: 0, text: 'food' }] }
    ]
  }, {
    seq: 200,
    forms: [{
      route: 'kana', text: 'だけ', ord: 0, common: null, commonTags: '',
      conjugatable: false, nokanji: true, best: null
    }],
    senses: [{ ord: 0, properties: [] }]
  }], { targetBlockBytes: 1024 });
  const sha256 = createHash('sha256').update(lexicon.bytes).digest('hex');
  const en = buildLocaleGlossStore({ locale: 'en', lexiconSha256: sha256, entries: [{
    seq: 100,
    groups: [
      { targets: [0], glosses: [{ ord: 0, text: 'to eat' }], info: [{ ord: 0, text: 'transitive' }] },
      { targets: [1], glosses: [{ ord: 0, text: 'to consume' }], info: [] }
    ]
  }, {
    seq: 200,
    groups: [{ targets: [0], glosses: [{ ord: 0, text: 'only' }], info: [] }]
  }], targetBlockBytes: 1024 });
  const zh = buildLocaleGlossStore({ locale: 'zh-Hans', lexiconSha256: sha256, entries: [{
    seq: 100,
    groups: [{ targets: [0], glosses: [{ ord: 0, text: '吃' }], info: [] }]
  }, {
    seq: 200,
    groups: [{ targets: [], glosses: [{ ord: 0, text: '仅；只' }], info: [{ ord: 0, text: '未对齐' }] }]
  }], targetBlockBytes: 1024 });
  return { lexicon, en, zh, sha256 };
}

describe('locale-aware dictionary stores', () => {
  test('merges aligned glosses with per-field English fallback', async () => {
    const fixture = fixtures();
    const lexicon = await LexiconStoreReader.open(memoryDictionarySource(fixture.lexicon.bytes), decodeGzip);
    const en = await LocaleGlossStoreReader.open(memoryDictionarySource(fixture.en.bytes), decodeGzip, {
      locale: 'en', lexiconSha256: fixture.sha256, entryCount: 2
    });
    const zh = await LocaleGlossStoreReader.open(memoryDictionarySource(fixture.zh.bytes), decodeGzip, {
      locale: 'zh-Hans', lexiconSha256: fixture.sha256, entryCount: 2
    });
    const dictionary = new DictionaryReader(lexicon, zh, en);
    expect(await dictionary.entry(0)).toMatchObject({
      seq: 100,
      senses: [
        { ord: 0, glosses: [{ text: '吃' }], properties: [{ tag: 'pos' }, { tag: 's_inf', text: 'transitive' }] },
        { ord: 1, glosses: [{ text: 'to consume' }], properties: [{ tag: 'field' }] }
      ]
    });
  });

  test('exposes an unaligned entry-wide group once after base senses', async () => {
    const fixture = fixtures();
    const lexicon = await LexiconStoreReader.open(memoryDictionarySource(fixture.lexicon.bytes), decodeGzip);
    const en = await LocaleGlossStoreReader.open(memoryDictionarySource(fixture.en.bytes), decodeGzip, {
      locale: 'en', lexiconSha256: fixture.sha256, entryCount: 2
    });
    const zh = await LocaleGlossStoreReader.open(memoryDictionarySource(fixture.zh.bytes), decodeGzip, {
      locale: 'zh-Hans', lexiconSha256: fixture.sha256, entryCount: 2
    });
    expect(await new DictionaryReader(lexicon, zh, en).entry(1)).toMatchObject({
      senses: [
        { ord: 0, glosses: [{ text: 'only' }] },
        { ord: 1, glosses: [{ text: '仅；只' }], properties: [{ tag: 's_inf', text: '未对齐' }] }
      ]
    });
  });

  test('rejects a locale bound to a different lexicon', async () => {
    const fixture = fixtures();
    await expect(LocaleGlossStoreReader.open(memoryDictionarySource(fixture.zh.bytes), decodeGzip, {
      locale: 'zh-Hans', lexiconSha256: '00'.repeat(32), entryCount: 2
    })).rejects.toBeInstanceOf(DictionaryStoreError);
  });

  test('falls back entry-wide gloss and info independently', () => {
    const lexicon = { seq: 1, forms: [], senses: [{ ord: 0, properties: [] }] };
    const localized = localizeEntry(
      lexicon,
      { seq: 1, groups: [{ targets: [], glosses: [{ ord: 0, text: '整词' }], info: [] }] },
      { seq: 1, groups: [{ targets: [], glosses: [], info: [{ ord: 0, text: 'note' }] }] }
    );
    expect(localized.senses[1]).toEqual({
      ord: 1,
      glosses: [{ ord: 0, text: '整词' }],
      properties: [{ tag: 's_inf', ord: 0, text: 'note' }]
    });
  });

  test('rejects locale targets absent from the lexicon entry', () => {
    expect(() => localizeEntry(
      { seq: 1, forms: [], senses: [{ ord: 0, properties: [] }] },
      { seq: 1, groups: [{ targets: [9], glosses: [{ ord: 0, text: '错' }], info: [] }] },
      { seq: 1, groups: [] }
    )).toThrow(DictionaryStoreError);
  });
});
