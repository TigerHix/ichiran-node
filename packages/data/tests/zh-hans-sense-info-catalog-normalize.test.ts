import { expect, test } from 'bun:test';

import { normalizeZhHansSenseInfoCatalog } from
  '../src/source-compiler/zh-hans-sense-info-catalog-normalize.js';
import { parseZhHansSenseInfoCatalog } from '../src/source-compiler/zh-hans-sense-info.js';

function catalog(translations: readonly { readonly source: string; readonly target: string }[]) {
  return parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations
  });
}

test('prunes only exact catalog translations duplicated by the direct policy', () => {
  const result = normalizeZhHansSenseInfoCatalog(catalog([
    { source: 'after an amount', target: '接在表示金额或数量的词之后' },
    { source: 'also read じんちゅう', target: '也读作「じんちゅう」' },
    { source: 'reviewed freeform note', target: '经审校的自由文本注释' }
  ]));

  expect(result.catalog.translations).toEqual([
    { source: 'reviewed freeform note', target: '经审校的自由文本注释' }
  ]);
  expect(result.prunedTranslations).toEqual([
    {
      source: 'after an amount',
      target: '接在表示数量、时长或金额的词之后',
      rule: 'grammar-attachment'
    },
    { source: 'also read じんちゅう', target: '也读作「じんちゅう」', rule: 'also-read' }
  ]);
  expect(result.reviewedTargetUpdates).toEqual([{
    source: 'after an amount',
    priorTarget: '接在表示金额或数量的词之后',
    reviewedTarget: '接在表示数量、时长或金额的词之后'
  }]);
  expect(result.stats).toMatchObject({
    patternPolicy: 'jmdict-s-inf-zh-Hans-patterns-v2',
    inputTranslationCount: 3,
    retainedTranslationCount: 1,
    prunedTranslationCount: 2,
    reviewedTargetUpdateCount: 1,
    prunedRuleCounts: { 'grammar-attachment': 1, 'also-read': 1 }
  });
});

test('hard-fails without a partial result when catalog and direct policy disagree', () => {
  expect(() => normalizeZhHansSenseInfoCatalog(catalog([
    { source: 'after an amount', target: '接在旧定义之后' },
    { source: 'also read じんちゅう', target: '另一条旧定义' },
    { source: 'reviewed freeform note', target: '经审校的自由文本注释' }
  ]))).toThrow(
    '2 catalog/direct-rule disagreement(s): "after an amount": '
    + 'catalog="接在旧定义之后", pattern="接在表示数量、时长或金额的词之后"; '
    + '"also read じんちゅう": catalog="另一条旧定义", pattern="也读作「じんちゅう」"'
  );
});
