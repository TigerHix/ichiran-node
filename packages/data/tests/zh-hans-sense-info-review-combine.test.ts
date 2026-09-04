import { createHash } from 'node:crypto';
import { expect, test } from 'bun:test';

import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import {
  combineZhHansSenseInfoReviewPairs,
  type ZhHansSenseInfoReviewPair
} from '../src/source-compiler/zh-hans-sense-info-review-combine.js';
import {
  ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY,
  buildZhHansSenseInfoContextIndex,
  mergeZhHansSenseInfoReviews,
  parseZhHansSenseInfoCandidateArtifact,
  parseZhHansSenseInfoReviewArtifact
} from '../src/source-compiler/zh-hans-sense-info-review.js';
import { parseZhHansSenseInfoCatalog } from '../src/source-compiler/zh-hans-sense-info.js';
import { ZH_HANS_SENSE_INFO_PATTERN_POLICY } from '../src/source-compiler/zh-hans-sense-info-patterns.js';

const JMDICT_SHA = 'a'.repeat(64);
const CATALOG_SHA = 'b'.repeat(64);
const entries = [
  parseJmdictEntry(
    '<entry><ent_seq>1</ent_seq><r_ele><reb>ほうりつ</reb></r_ele>'
    + '<sense><s_inf>formal legal phrase</s_inf><gloss>legal phrase</gloss></sense></entry>',
    'fixture', 1
  ),
  parseJmdictEntry(
    '<entry><ent_seq>2</ent_seq><r_ele><reb>しご</reb></r_ele>'
    + '<sense><s_inf>rare poetic term</s_inf><gloss>poetic term</gloss></sense></entry>',
    'fixture', 2
  )
];
const contexts = buildZhHansSenseInfoContextIndex(entries);

function bytes(value: unknown): Uint8Array {
  return new TextEncoder().encode(`${JSON.stringify(value, null, 2)}\n`);
}

function sha256(value: Uint8Array): string {
  return createHash('sha256').update(value).digest('hex');
}

function pair(
  source: string,
  target: string,
  run: string,
  catalogSha256 = CATALOG_SHA
): ZhHansSenseInfoReviewPair {
  const candidateArtifact = parseZhHansSenseInfoCandidateArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: {
      jmdict: { id: 'fixture-jmdict', sha256: JMDICT_SHA },
      catalog: { id: 'fixture-catalog', sha256: catalogSha256 },
      patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY
    },
    origin: { kind: 'native' },
    translator: {
      kind: 'codex',
      provider: 'openai',
      model: 'gpt-fixture',
      runId: `translator-${run}`,
      generatedAt: '2026-09-04T12:00:00.000Z',
      sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
    },
    candidates: [{
      source,
      target,
      catalogAction: 'add',
      uncertainty: { level: 'low', rationale: 'Fixture confidence.' },
      contexts: contexts.get(source)
    }]
  });
  const candidateSha256 = sha256(bytes(candidateArtifact));
  const reviewArtifact = parseZhHansSenseInfoReviewArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-decisions',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    candidateSha256,
    origin: { kind: 'native' },
    reviewer: {
      kind: 'codex',
      provider: 'openai',
      model: 'gpt-review-fixture',
      runId: `reviewer-${run}`,
      reviewedAt: '2026-09-04T13:00:00.000Z',
      sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
    },
    decisions: [{ source, decision: 'approve', rationale: 'Fixture approval.' }]
  });
  return { candidateBytes: bytes(candidateArtifact), reviewBytes: bytes(reviewArtifact) };
}

test('combines disjoint reviewed batches with sorted rows and aggregate origins', () => {
  const second = pair('rare poetic term', '罕见诗语', 'second');
  const first = pair('formal legal phrase', '正式法律用语', 'first');
  const combined = combineZhHansSenseInfoReviewPairs([second, first]);
  expect(combined.candidateArtifact.candidates.map(item => item.source)).toEqual([
    'formal legal phrase',
    'rare poetic term'
  ]);
  expect(combined.reviewArtifact.decisions.map(item => item.source)).toEqual([
    'formal legal phrase',
    'rare poetic term'
  ]);
  expect(combined.candidateArtifact.origin).toMatchObject({
    kind: 'combined',
    batches: expect.any(Array)
  });
  expect(combined.candidateArtifact.translator).toMatchObject({
    kind: 'combined',
    runs: [
      { runId: 'translator-first' },
      { runId: 'translator-second' }
    ]
  });
  expect(combined.reviewArtifact.reviewer).toMatchObject({
    kind: 'combined',
    reviewers: [
      { runId: 'reviewer-first' },
      { runId: 'reviewer-second' }
    ]
  });
  expect(combined.reviewArtifact.candidateSha256).toBe(sha256(combined.candidateBytes));

  const catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: []
  });
  const merged = mergeZhHansSenseInfoReviews({
    entries,
    catalog,
    jmdictIdentity: { id: 'fixture-jmdict', sha256: JMDICT_SHA },
    catalogIdentity: { id: 'fixture-catalog', sha256: CATALOG_SHA },
    candidateArtifact: combined.candidateArtifact,
    candidateSha256: combined.reviewArtifact.candidateSha256,
    reviewArtifact: combined.reviewArtifact
  });
  expect(merged.catalog.translations).toHaveLength(2);
});

test('combine rejects digest bindings, duplicate sources/runs, and mixed source identities', () => {
  const first = pair('formal legal phrase', '正式法律用语', 'first');
  const second = pair('rare poetic term', '罕见诗语', 'second');
  const mismatchedReview = JSON.parse(new TextDecoder().decode(second.reviewBytes));
  mismatchedReview.candidateSha256 = 'c'.repeat(64);
  expect(() => combineZhHansSenseInfoReviewPairs([
    first,
    { ...second, reviewBytes: bytes(mismatchedReview) }
  ])).toThrow('wrong candidate digest');
  expect(() => combineZhHansSenseInfoReviewPairs([first, first])).toThrow(
    'Duplicate translator/reviewer run ID'
  );
  expect(() => combineZhHansSenseInfoReviewPairs([
    first,
    pair('formal legal phrase', '正式法律用语', 'other')
  ])).toThrow('Duplicate source');
  expect(() => combineZhHansSenseInfoReviewPairs([
    first,
    pair('rare poetic term', '罕见诗语', 'other', 'd'.repeat(64))
  ])).toThrow('do not share corpus, catalog, and policy');
});
