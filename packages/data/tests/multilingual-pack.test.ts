import { createHash } from 'node:crypto';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { expect, test } from 'bun:test';

import {
  LEXICON_MAGIC,
  buildLexiconStore
} from '../src/browser-pack/lexicon.js';
import {
  LOCALE_GLOSS_LEXICON_SHA256_OFFSET,
  LOCALE_GLOSS_LOCALE_LENGTH_OFFSET,
  LOCALE_GLOSS_LOCALE_OFFSET,
  LOCALE_GLOSS_MAGIC,
  buildLocaleGlossStore
} from '../src/browser-pack/locale-gloss.js';
import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import { compileCanonicalRoots } from '../src/source-compiler/canonical-roots.js';
import {
  assertLocaleGlossEntriesMatchLexicon,
  canonicalEnglishLocaleEntries,
  canonicalLexiconEntries
} from '../src/source-compiler/pack-input.js';
import {
  loadTomoshiZhHans,
  projectTomoshiZhHans
} from '../src/source-compiler/tomoshi-zh-hans.js';
import {
  loadZhHansSenseInfo,
  parseZhHansSenseInfoCatalog,
  projectZhHansSenseInfo
} from '../src/source-compiler/zh-hans-sense-info.js';
import { buildZhHansSenseInfoWorklist } from '../src/source-compiler/zh-hans-sense-info-worklist.js';

const XML = `<entry>
<ent_seq>1157170</ent_seq>
<k_ele><keb>為る</keb></k_ele>
<r_ele><reb>する</reb><re_pri>ichi1</re_pri></r_ele>
<sense><pos>&vs-i;</pos><s_inf>action</s_inf><gloss>to do</gloss><gloss>to perform</gloss></sense>
<sense><pos>&vs-i;</pos><gloss>to cost</gloss></sense>
</entry>`;

function hex(bytes: Uint8Array): string {
  return [...bytes].map(byte => byte.toString(16).padStart(2, '0')).join('');
}

test('splits language-neutral lexicon structure from digest-bound English glosses', () => {
  const entries = [parseJmdictEntry(XML, 'fixture', 0)];
  const lexiconEntries = canonicalLexiconEntries(entries);
  const englishEntries = canonicalEnglishLocaleEntries(entries);
  expect(lexiconEntries[0]?.senses).toEqual([
    { ord: 0, properties: [{ tag: 'pos', ord: 0, text: 'vs-i' }] },
    { ord: 1, properties: [{ tag: 'pos', ord: 0, text: 'vs-i' }] }
  ]);
  expect(englishEntries[0]?.groups).toEqual([
    {
      targets: [0],
      glosses: [{ ord: 0, text: 'to do' }, { ord: 1, text: 'to perform' }],
      info: [{ ord: 0, text: 'action' }]
    },
    {
      targets: [1],
      glosses: [{ ord: 0, text: 'to cost' }],
      info: []
    }
  ]);
  assertLocaleGlossEntriesMatchLexicon(lexiconEntries, englishEntries, 'en');

  const lexicon = buildLexiconStore(lexiconEntries, { targetBlockBytes: 1024 });
  const lexiconSha256 = createHash('sha256').update(lexicon.bytes).digest('hex');
  const english = buildLocaleGlossStore({
    locale: 'en', lexiconSha256, entries: englishEntries, targetBlockBytes: 1024
  });

  expect(new TextDecoder().decode(lexicon.bytes.subarray(0, 8))).toBe(LEXICON_MAGIC);
  expect(new TextDecoder().decode(english.bytes.subarray(0, 8))).toBe(LOCALE_GLOSS_MAGIC);
  expect(hex(english.bytes.subarray(
    LOCALE_GLOSS_LEXICON_SHA256_OFFSET,
    LOCALE_GLOSS_LEXICON_SHA256_OFFSET + 32
  ))).toBe(lexiconSha256);
  const localeLength = english.bytes[LOCALE_GLOSS_LOCALE_LENGTH_OFFSET]!;
  expect(new TextDecoder().decode(english.bytes.subarray(
    LOCALE_GLOSS_LOCALE_OFFSET,
    LOCALE_GLOSS_LOCALE_OFFSET + localeLength
  ))).toBe('en');
  expect(buildLexiconStore(lexiconEntries, { targetBlockBytes: 1024 }).bytes)
    .toEqual(lexicon.bytes);
  expect(buildLocaleGlossStore({
    locale: 'en', lexiconSha256, entries: englishEntries, targetBlockBytes: 1024
  }).bytes).toEqual(english.bytes);
});

test('Tomoshi adapter accepts only senses whose captured English glosses still match', () => {
  const base = parseJmdictEntry(XML, 'fixture', 0);
  const missing = parseJmdictEntry(
    '<entry><ent_seq>2000000</ent_seq><r_ele><reb>なし</reb></r_ele><sense><gloss>none</gloss></sense></entry>',
    'fixture',
    1
  );
  const projection = projectTomoshiZhHans([base, missing], [{
    entryId: '1157170',
    entryData: JSON.stringify({
      id: '1157170',
      senses: [
        { glosses: [{ text: 'to do', lang: 'eng' }, { text: 'to perform', lang: 'eng' }] },
        { glosses: [{ text: 'changed upstream meaning', lang: 'eng' }] }
      ]
    }),
    zhData: JSON.stringify({
      senses: {
        0: { glosses: [{ text: '做；进行' }] },
        1: { glosses: [{ text: '花费' }] }
      }
    })
  }]);

  expect(projection.entries).toEqual([
    {
      seq: 1157170,
      groups: [{
        targets: [0],
        glosses: [{ ord: 0, text: '做；进行' }],
        info: []
      }]
    },
    { seq: 2000000, groups: [] }
  ]);
  expect(projection.stats).toMatchObject({
    translatedEntryCount: 1,
    fallbackEntryCount: 1,
    translatedSenseCount: 1,
    fallbackSenseCount: 2,
    mismatchedSenseCount: 1,
    glossCount: 1
  });
});

test('zh-Hans sense-info catalog merges translated notes without requiring a Chinese gloss', () => {
  const translated = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [{ source: 'action', target: '动作' }]
  });
  const base = parseJmdictEntry(XML, 'fixture', 0);
  const projection = projectZhHansSenseInfo([base], [{
    seq: 1157170,
    groups: [{
      targets: [1],
      glosses: [{ ord: 0, text: '花费' }],
      info: []
    }]
  }], translated);

  expect(projection.entries).toEqual([{
    seq: 1157170,
    groups: [
      { targets: [0], glosses: [], info: [{ ord: 0, text: '动作' }] },
      { targets: [1], glosses: [{ ord: 0, text: '花费' }], info: [] }
    ]
  }]);
  expect(projection.stats).toEqual({
    catalogTranslationCount: 1,
    patternPolicy: 'jmdict-s-inf-zh-Hans-patterns-v2',
    sourceInfoCount: 1,
    translatedInfoCount: 1,
    catalogTranslatedInfoCount: 1,
    patternTranslatedInfoCount: 0,
    fallbackInfoCount: 0,
    uniqueSourceInfoCount: 1,
    translatedUniqueInfoCount: 1,
    catalogTranslatedUniqueInfoCount: 1,
    patternTranslatedUniqueInfoCount: 0,
    unusedTranslationCount: 0,
    patternRuleCounts: expect.any(Object)
  });
});

test('zh-Hans sense-info catalog rejects duplicates and reports stale unused translations', () => {
  expect(() => parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [
      { source: 'action', target: '动作' },
      { source: 'action', target: '行为' }
    ]
  })).toThrow('unique and sorted');

  const base = parseJmdictEntry(XML, 'fixture', 0);
  const projection = projectZhHansSenseInfo([base], [{ seq: 1157170, groups: [] }], {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [{ source: 'removed upstream note', target: '已删除的上游注释' }]
  });
  expect(projection.stats).toMatchObject({
    sourceInfoCount: 1,
    translatedInfoCount: 0,
    catalogTranslatedInfoCount: 0,
    patternTranslatedInfoCount: 0,
    fallbackInfoCount: 1,
    unusedTranslationCount: 1
  });
});

test('zh-Hans sense-info worklist retains dictionary context for untranslated notes', () => {
  const base = parseJmdictEntry(XML, 'fixture', 0);
  expect(buildZhHansSenseInfoWorklist([base], {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: []
  })).toEqual([{
    source: 'action',
    occurrenceCount: 1,
    occurrences: [{
      seq: 1157170,
      sense: 0,
      info: 0,
      headwords: ['為る', 'する'],
      englishGlosses: ['to do', 'to perform']
    }]
  }]);
});

test('rejects a locale layer that targets a nonexistent base sense', () => {
  const lexicon = canonicalLexiconEntries([parseJmdictEntry(XML, 'fixture', 0)]);
  expect(() => assertLocaleGlossEntriesMatchLexicon(lexicon, [{
    seq: 1157170,
    groups: [{ targets: [9], glosses: [{ ord: 0, text: '错' }], info: [] }]
  }], 'zh-Hans')).toThrow('targets missing sense 9');
});

const repository = resolve(import.meta.dir, '../../..');
const currentJmdict = resolve(repository, 'work/m6-transition/JMdict_e-2026-09-02.gz');
const currentTomoshi = resolve(
  repository,
  'work/multilingual-sources/tomoshi-dict-open-2026-08-12.db'
);

test.skipIf(!existsSync(currentJmdict) || !existsSync(currentTomoshi))(
  'current pinned sources retain measured multilingual coverage and artifact sizes',
  async () => {
    const roots = await compileCanonicalRoots({
      jmdict: currentJmdict,
      jmdictSourceId: 'edrdg-jmdict-e-2026-09-02',
      extra: resolve(repository, 'data/sources/extra.xml'),
      municipality: resolve(repository, 'data/sources/jichitai.csv'),
      ward: resolve(repository, 'data/sources/gyoseiku.csv'),
      errata: resolve(repository, 'data/source-compiler-errata.json'),
      compatibility: resolve(repository, 'data/source-compiler-compatibility.json')
    });
    const zhHans = loadTomoshiZhHans(currentTomoshi, roots.entries, {
      exportVersion: '1',
      sourceSchemaVersion: '16',
      exportedAt: '2026-08-14T16:50:40+0900'
    });
    expect(zhHans.stats).toEqual({
      baseEntryCount: 221_951,
      baseSenseCount: 257_082,
      sourceEntryCount: 217_261,
      staleSourceEntryCount: 13,
      translatedEntryCount: 216_688,
      fallbackEntryCount: 5_263,
      translatedSenseCount: 250_617,
      fallbackSenseCount: 6_465,
      mismatchedSenseCount: 762,
      glossCount: 250_617
    });

    const lexicon = buildLexiconStore(canonicalLexiconEntries(roots.entries));
    const lexiconSha256 = createHash('sha256').update(lexicon.bytes).digest('hex');
    const english = buildLocaleGlossStore({
      locale: 'en',
      lexiconSha256,
      entries: canonicalEnglishLocaleEntries(roots.entries)
    });
    const localizedInfo = await loadZhHansSenseInfo(
      resolve(repository, 'data/locales/zh-Hans/sense-info.json'),
      roots.entries,
      zhHans.entries
    );
    expect(localizedInfo.stats).toEqual({
      catalogTranslationCount: 3_361,
      patternPolicy: 'jmdict-s-inf-zh-Hans-patterns-v2',
      sourceInfoCount: 6_834,
      translatedInfoCount: 6_825,
      catalogTranslatedInfoCount: 4_361,
      patternTranslatedInfoCount: 2_464,
      fallbackInfoCount: 9,
      uniqueSourceInfoCount: 5_347,
      translatedUniqueInfoCount: 5_338,
      catalogTranslatedUniqueInfoCount: 3_361,
      patternTranslatedUniqueInfoCount: 1_977,
      unusedTranslationCount: 0,
      patternRuleCounts: {
        'abbreviation-of': 178,
        'adverbially-as': 2,
        'after-expression': 2,
        'also-pronounced': 4,
        'also-read': 6,
        'also-written': 126,
        'as-expression': 226,
        'before-expression': 0,
        'contraction-of': 11,
        'emphatic-form-of': 25,
        'equivalent-expression': 8,
        'especially-as': 5,
        'especially-expression': 423,
        'example-expression': 45,
        'formerly-read': 1,
        'frequent-adverbially-as': 15,
        'frequent-pronounced': 1,
        'frequent-read': 5,
        'frequent-written': 19,
        'grammar-attachment': 304,
        'incorrect-variant-of': 11,
        'inflection-form-of': 27,
        'modern-pronounced': 1,
        'modern-read': 0,
        'more-emphatic-than': 29,
        'negative-context': 32,
        'nonstandard-variant-of': 17,
        'occasional-expression': 10,
        'occasional-pronounced': 3,
        'occasional-read': 5,
        'occasional-written': 43,
        'often-as': 123,
        'often-expression': 71,
        'place-dialect': 26,
        'prefix-pronounced': 1,
        'prefix-read': 0,
        pronounced: 2,
        read: 0,
        'short-for': 16,
        'sometimes-written': 14,
        'stronger-version-of': 1,
        'subject-reading': 6,
        'suffix-pronounced': 12,
        'suffix-read': 2,
        'usual-adverbially-as': 41,
        'usual-pronounced': 1,
        'usual-read': 4,
        'usual-written': 26,
        'usually-as': 262,
        'usually-expression': 269,
        'variant-of': 2,
        'with-expression': 1
      }
    });
    const chinese = buildLocaleGlossStore({
      locale: 'zh-Hans',
      lexiconSha256,
      entries: localizedInfo.entries
    });
    expect({
      lexicon: [lexicon.bytes.byteLength, gzipSync(lexicon.bytes, { level: 9 }).byteLength],
      en: [english.bytes.byteLength, gzipSync(english.bytes, { level: 9 }).byteLength],
      zhHans: [chinese.bytes.byteLength, gzipSync(chinese.bytes, { level: 9 }).byteLength]
    }).toEqual({
      lexicon: [8_348_675, 7_040_155],
      en: [7_302_742, 5_938_941],
      zhHans: [6_933_745, 5_558_721]
    });
  },
  60_000
);
