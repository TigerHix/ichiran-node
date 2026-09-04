import { expect, test } from 'bun:test';

import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import {
  parseZhHansSenseInfoCatalog,
  projectZhHansSenseInfo
} from '../src/source-compiler/zh-hans-sense-info.js';
import {
  ZH_HANS_SENSE_INFO_PATTERN_POLICY,
  translateZhHansSenseInfoPattern
} from '../src/source-compiler/zh-hans-sense-info-patterns.js';

test('translates only closed reading and writing grammars while preserving Japanese expressions', () => {
  expect(translateZhHansSenseInfoPattern('read びくう')).toEqual({
    policy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
    rule: 'read',
    target: '读作「びくう」'
  });
  expect(translateZhHansSenseInfoPattern('also read じんちゅう')).toMatchObject({
    rule: 'also-read', target: '也读作「じんちゅう」'
  });
  expect(translateZhHansSenseInfoPattern('oft. read づかい as a suffix')).toMatchObject({
    rule: 'suffix-read', target: '用作后缀时常读作「づかい」'
  });
  expect(translateZhHansSenseInfoPattern('usu. pronounced がたき when used as a suffix'))
    .toMatchObject({
      rule: 'suffix-pronounced', target: '用作后缀时通常读作「がたき」'
    });
  expect(translateZhHansSenseInfoPattern('sometimes read たまをのぼし or たまほめぼし'))
    .toMatchObject({
      rule: 'occasional-read', target: '有时读作「たまをのぼし」或「たまほめぼし」'
    });
  expect(translateZhHansSenseInfoPattern('は is pronounced as わ')).toMatchObject({
    rule: 'subject-reading', target: '「は」读作「わ」'
  });
  expect(translateZhHansSenseInfoPattern('嘈囃 is sometimes read むねやけ')).toMatchObject({
    rule: 'subject-reading', target: '「嘈囃」有时读作「むねやけ」'
  });
  expect(translateZhHansSenseInfoPattern('also written as 一国, 一克, 一剋')).toMatchObject({
    rule: 'also-written', target: '也写作「一国、一克、一剋」'
  });
  expect(translateZhHansSenseInfoPattern('usu. written as カジヤ')).toMatchObject({
    rule: 'usual-written', target: '通常写作「カジヤ」'
  });
  expect(translateZhHansSenseInfoPattern('also written as ナメる and 無礼る')).toMatchObject({
    rule: 'also-written', target: '也写作「ナメる」和「無礼る」'
  });
  expect(translateZhHansSenseInfoPattern('usu. as AをBにする')).toMatchObject({
    rule: 'usually-as', target: '通常作「AをBにする」'
  });
  expect(translateZhHansSenseInfoPattern('as ...を押して...')).toMatchObject({
    rule: 'as-expression', target: '作「…を押して…」'
  });
  expect(translateZhHansSenseInfoPattern('also written as 〆切(り)')).toMatchObject({
    rule: 'also-written', target: '也写作「〆切(り)」'
  });
  expect(translateZhHansSenseInfoPattern('esp. ポインタ')).toMatchObject({
    rule: 'especially-expression', target: '尤作「ポインタ」'
  });
  expect(translateZhHansSenseInfoPattern('short for 女子バスケットボール')).toMatchObject({
    rule: 'short-for', target: '「女子バスケットボール」的简称'
  });
  expect(translateZhHansSenseInfoPattern('incorrect variant of 時期尚早')).toMatchObject({
    rule: 'incorrect-variant-of', target: '「時期尚早」的误用变体'
  });
  expect(translateZhHansSenseInfoPattern('potential form of 取る')).toMatchObject({
    rule: 'inflection-form-of', target: '「取る」的可能形'
  });
  expect(translateZhHansSenseInfoPattern('e.g. やばい → やばたん')).toMatchObject({
    rule: 'example-expression', target: '例如「やばい → やばたん」'
  });
  expect(translateZhHansSenseInfoPattern('e.g. 暑い and 熱い, 油 and 脂')).toMatchObject({
    rule: 'example-expression', target: '例如「暑い」和「熱い」、「油」和「脂」'
  });
  expect(translateZhHansSenseInfoPattern('after -masu stem of a verb')).toMatchObject({
    rule: 'grammar-attachment', target: '接在动词ます形去掉「ます」后的词干之后'
  });
  expect(translateZhHansSenseInfoPattern('usu. with a negative nuance')).toMatchObject({
    rule: 'negative-context', target: '通常含否定语气'
  });
  expect(translateZhHansSenseInfoPattern('Chūgoku dialect')).toMatchObject({
    rule: 'place-dialect', target: '中国地区方言'
  });
});

test('declines prose or qualified statements outside the closed grammar', () => {
  for (const source of [
    'read びくう in medical science',
    'used when asking someone to read something one has written',
    'esp. in fiction',
    'from Star Wars',
    'from 朝 + 散歩',
    'pun on エンゲル係数',
    'after a number of people',
    'Tokyo dialect',
    'は is pronounced as わ; used during daytime',
    'read X',
    'read び\nくう',
    'as (日本',
    'from 日本+',
    'esp. 「日本」',
    'as 日本.'
  ]) expect(translateZhHansSenseInfoPattern(source)).toBeNull();
});

test('exact catalog wins over a pattern and provenance counters remain separate', () => {
  const entry = parseJmdictEntry(
    '<entry><ent_seq>1</ent_seq><r_ele><reb>しりぞく</reb></r_ele>'
    + '<sense><s_inf>also written as 退く</s_inf><s_inf>also read しりぞく</s_inf>'
    + '<s_inf>ambiguous prose</s_inf><gloss>to retreat</gloss></sense></entry>',
    'fixture',
    0
  );
  const catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [{ source: 'also written as 退く', target: '人工审校译文' }]
  });
  const projected = projectZhHansSenseInfo([entry], [{ seq: 1, groups: [] }], catalog);
  expect(projected.entries[0]?.groups[0]?.info).toEqual([
    { ord: 0, text: '人工审校译文' },
    { ord: 1, text: '也读作「しりぞく」' }
  ]);
  expect(projected.stats).toMatchObject({
    patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
    sourceInfoCount: 3,
    translatedInfoCount: 2,
    catalogTranslatedInfoCount: 1,
    patternTranslatedInfoCount: 1,
    fallbackInfoCount: 1,
    catalogTranslatedUniqueInfoCount: 1,
    patternTranslatedUniqueInfoCount: 1,
    unusedTranslationCount: 0,
    patternRuleCounts: { 'also-read': 1, 'also-written': 0 }
  });
});
