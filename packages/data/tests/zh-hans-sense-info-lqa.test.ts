import { expect, test } from 'bun:test';

import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import { parseZhHansSenseInfoCatalog } from '../src/source-compiler/zh-hans-sense-info.js';
import {
  analyzeZhHansSenseInfoLqa,
  buildZhHansSenseInfoRuleOutput,
  parseZhHansSenseInfoRuleOutput,
  type ZhHansSenseInfoRuleMatch,
  type ZhHansSenseInfoRuleOutput
} from '../src/source-compiler/zh-hans-sense-info-lqa.js';

function entry(seq: number, note: string) {
  return parseJmdictEntry(`<entry>
<ent_seq>${seq}</ent_seq>
<r_ele><reb>ことば${seq}</reb></r_ele>
<sense><s_inf>${note}</s_inf><gloss>fixture gloss ${seq}</gloss></sense>
</entry>`, 'fixture', seq);
}

const catalog = parseZhHansSenseInfoCatalog({
  formatVersion: 1,
  locale: 'zh-Hans',
  sourceLocale: 'en',
  translations: [
    { source: 'obsolete catalog note', target: '过时目录注释' },
    { source: 'rare poetic term', target: '罕见诗语' }
  ]
});

const matches: ZhHansSenseInfoRuleMatch[] = [
  { source: 'after the -masu stem', ruleId: 'after-form', target: '接在连用形后' },
  { source: 'after the -masu stem', ruleId: 'broad-after', target: '用于连用形之后' },
  { source: 'rare poetic term', ruleId: 'register', target: '诗歌用语' },
  { source: 'stale note', ruleId: 'stale-rule', target: 'stale note' },
  { source: 'usually written using kana alone', ruleId: 'kana-a', target: '通常仅用假名书写' },
  { source: 'usually written using kana alone', ruleId: 'kana-b', target: '通常仅用假名书写' }
];

const rules: ZhHansSenseInfoRuleOutput = {
  formatVersion: 1,
  locale: 'zh-Hans',
  sourceLocale: 'en',
  matches
};

const entries = [
  entry(1, 'after the -masu stem'),
  entry(2, 'after the -masu stem'),
  entry(3, 'before a noun'),
  entry(4, 'usually written using kana alone'),
  entry(5, 'rare poetic term')
];

test('sense-info LQA separates shipped catalog coverage from rule suggestions', () => {
  const report = analyzeZhHansSenseInfoLqa(entries, catalog, rules);

  expect(report.coverage).toEqual({
    source: { unique: 4, occurrences: 5 },
    catalog: { unique: 1, occurrences: 1, uniqueRatio: 0.25, occurrenceRatio: 0.2 },
    deterministicRules: {
      unique: 1,
      occurrences: 1,
      uniqueRatio: 0.25,
      occurrenceRatio: 0.2
    },
    effectiveLocalized: {
      unique: 2,
      occurrences: 2,
      uniqueRatio: 0.5,
      occurrenceRatio: 0.4
    },
    ruleCollisions: {
      unique: 1,
      occurrences: 2,
      uniqueRatio: 0.25,
      occurrenceRatio: 0.4
    },
    unmatched: {
      unique: 1,
      occurrences: 1,
      uniqueRatio: 0.25,
      occurrenceRatio: 0.2
    },
    untranslatedRemainder: {
      unique: 2,
      occurrences: 3,
      uniqueRatio: 0.5,
      occurrenceRatio: 0.6
    }
  });
  expect(report.untranslatedRemainder.map(item => [
    item.source,
    item.resolution,
    item.risk
  ])).toEqual([
    ['after the -masu stem', 'rule-collision', 'high'],
    ['before a noun', 'unmatched', 'medium']
  ]);
  expect(report.riskyUnmatchedBoilerplate.map(item => item.source)).toEqual(['before a noun']);
  expect(report.agentQueues.translator).toHaveLength(2);
});

test('LQA can evaluate the production closed-pattern policy without promoting it to catalog', () => {
  const patternEntries = [
    entry(10, 'also written as 退く'),
    entry(11, 'used when asking someone to read something one has written')
  ];
  const output = buildZhHansSenseInfoRuleOutput(patternEntries);
  expect(output.matches).toEqual([{
    source: 'also written as 退く',
    target: '也写作「退く」',
    ruleId: 'also-written'
  }]);
  const emptyCatalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: []
  });
  const report = analyzeZhHansSenseInfoLqa(patternEntries, emptyCatalog, output);
  expect(report.coverage.catalog.unique).toBe(0);
  expect(report.coverage.deterministicRules.unique).toBe(1);
  expect(report.coverage.unmatched.unique).toBe(1);
  expect(report.untranslatedRemainder.map(item => item.source)).toEqual([
    'used when asking someone to read something one has written'
  ]);
});

test('sense-info LQA exposes clusters and reviewer-oriented rule diagnostics', () => {
  const report = analyzeZhHansSenseInfoLqa(entries, catalog, rules);

  expect(report.clusters.find(cluster => cluster.id === 'after')).toMatchObject({
    uniqueSourceCount: 1,
    occurrenceCount: 2,
    ruleCollisionUniqueCount: 1
  });
  expect(report.diagnostics.ruleCollisions).toEqual([{
    source: 'after the -masu stem',
    occurrenceCount: 2,
    suggestions: [
      { target: '接在连用形后', ruleIds: ['after-form'] },
      { target: '用于连用形之后', ruleIds: ['broad-after'] }
    ]
  }]);
  expect(report.diagnostics.overlappingRules).toEqual([{
    source: 'usually written using kana alone',
    target: '通常仅用假名书写',
    ruleIds: ['kana-a', 'kana-b']
  }]);
  expect(report.diagnostics.catalogRuleDisagreements).toEqual([{
    source: 'rare poetic term',
    catalogTarget: '罕见诗语',
    ruleSuggestions: [{ target: '诗歌用语', ruleIds: ['register'] }]
  }]);
  expect(report.diagnostics.staleCatalogSources).toEqual(['obsolete catalog note']);
  expect(report.diagnostics.staleRuleSources).toEqual([{
    source: 'stale note',
    ruleId: 'stale-rule',
    target: 'stale note'
  }]);
  expect(report.diagnostics.suspiciousTargets).toContainEqual({
    source: 'stale note',
    target: 'stale note',
    producer: 'rule:stale-rule',
    reasons: ['identity-translation', 'ascii-only-target']
  });
  expect(report.agentQueues.reviewer.some(item => item.kind === 'rule-collision')).toBe(true);
  expect(report.agentQueues.reviewer.some(item =>
    item.kind === 'catalog-rule-disagreement')).toBe(true);
});

test('deterministic-rule output parser rejects extra, duplicate, and unsorted rows', () => {
  expect(() => parseZhHansSenseInfoRuleOutput({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    matches: [{ source: 'a', target: '甲', ruleId: 'a' }],
    draft: true
  })).toThrow('unknown fields');
  expect(() => parseZhHansSenseInfoRuleOutput({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    matches: [
      { source: 'b', target: '乙', ruleId: 'b' },
      { source: 'a', target: '甲', ruleId: 'a' }
    ]
  })).toThrow('unique and sorted');
  expect(() => parseZhHansSenseInfoRuleOutput({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    matches: [
      { source: 'a', target: '甲', ruleId: 'a' },
      { source: 'a', target: '甲', ruleId: 'a' }
    ]
  })).toThrow('unique and sorted');
});

test('LQA ordering does not depend on entry or rule evaluation order', () => {
  const reversedRules: ZhHansSenseInfoRuleOutput = { ...rules, matches: [...matches].reverse() };
  expect(analyzeZhHansSenseInfoLqa([...entries].reverse(), catalog, reversedRules))
    .toEqual(analyzeZhHansSenseInfoLqa(entries, catalog, rules));
});
