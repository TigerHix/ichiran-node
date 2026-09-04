import { createHash } from 'node:crypto';
import { expect, test } from 'bun:test';

import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import {
  adaptRichZhHansSenseInfoAddBatch,
  adaptRichZhHansSenseInfoCatalogRevisions
} from '../src/source-compiler/zh-hans-sense-info-review-adapter.js';
import {
  ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY,
  mergeZhHansSenseInfoReviews
} from '../src/source-compiler/zh-hans-sense-info-review.js';
import { parseZhHansSenseInfoCatalog } from '../src/source-compiler/zh-hans-sense-info.js';
import { ZH_HANS_SENSE_INFO_PATTERN_POLICY } from '../src/source-compiler/zh-hans-sense-info-patterns.js';

const JMDICT_SHA = 'b'.repeat(64);
const CATALOG_SHA = 'c'.repeat(64);
const CANDIDATE_INPUT_SHA = 'd'.repeat(64);
const REVIEW_INPUT_SHA = 'e'.repeat(64);
const identities = {
  jmdict: { id: 'fixture-jmdict', sha256: JMDICT_SHA },
  catalog: { id: 'fixture-catalog', sha256: CATALOG_SHA }
};
const generatedFrom = {
  ...identities,
  deterministicRules: {
    builtInPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
    additionalOutput: null
  }
};
const metadata = {
  translator: {
    kind: 'codex' as const,
    provider: 'openai' as const,
    model: 'gpt-translator-fixture',
    runId: 'translator-run',
    generatedAt: '2026-09-04T12:00:00.000Z',
    sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
  },
  reviewer: {
    kind: 'codex' as const,
    provider: 'openai' as const,
    model: 'gpt-reviewer-fixture',
    runId: 'reviewer-run',
    reviewedAt: '2026-09-04T13:00:00.000Z',
    sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
  }
};

function entry(seq: number, note: string, gloss: string) {
  return parseJmdictEntry(
    `<entry><ent_seq>${seq}</ent_seq><k_ele><keb>言葉${seq}</keb></k_ele>`
    + `<r_ele><reb>ことば${seq}</reb></r_ele><sense><s_inf>${note}</s_inf>`
    + `<gloss>${gloss}</gloss></sense></entry>`,
    'fixture',
    seq
  );
}

const entries = [
  entry(1, 'formal legal phrase', 'legal term'),
  entry(2, 'rare poetic term', 'poetic word')
];
function context(index: number) {
  const item = entries[index]!;
  return [{
    seq: item.seq,
    sense: 0,
    info: 0,
    headwords: [`言葉${item.seq}`, `ことば${item.seq}`],
    englishGlosses: item.senses[0]!.glosses
  }];
}

function emptyCatalog() {
  return parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: []
  });
}

function richAddCandidate() {
  return {
    formatVersion: 1,
    kind: 'codex-zh-Hans-sense-info-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    status: 'private-candidates-not-production',
    inputPolicy: 'Codex translation from canonical English s_inf plus complete JMdict contexts; '
      + 'no Apple Translation, external machine translation, or draft output',
    selection: {},
    generatedFrom,
    selfLqa: {},
    candidates: [{
      source: 'formal legal phrase',
      target: '正式法律用语',
      cluster: 'fixture',
      contexts: context(0),
      rationale: 'Fixture candidate rationale.',
      uncertainty: {
        level: 'low',
        needsIndependentSemanticReview: true,
        note: 'Fixture uncertainty note.'
      }
    }]
  };
}

function richAddReview() {
  return {
    formatVersion: 1,
    kind: 'zh-Hans-candidate-semantic-review',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    status: 'independent-review',
    reviewer: 'Codex independent contextual review; no external translation service',
    input: {
      path: 'fixture-rich-candidates.json',
      sha256: CANDIDATE_INPUT_SHA,
      candidateCount: 1,
      generatedFrom
    },
    criteria: ['fixture criterion'],
    summary: { approve: 0, revise: 1, reject: 0, total: 1 },
    decisions: [{
      source: 'formal legal phrase',
      cluster: 'fixture',
      candidateTarget: '正式法律用语',
      decision: 'revise',
      revisedTarget: '法律正式用语',
      reason: 'Fixture reviewer rationale.',
      confidence: 'high'
    }]
  };
}

test('adapts a rich add batch without changing decisions or losing raw input digests', () => {
  const adapted = adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: richAddCandidate(),
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(adapted.candidateArtifact.origin).toEqual({
    kind: 'adapted',
    sourceKind: 'codex-zh-Hans-sense-info-candidates',
    sha256: CANDIDATE_INPUT_SHA
  });
  expect(adapted.reviewArtifact.origin).toMatchObject({ sha256: REVIEW_INPUT_SHA });
  expect(adapted.reviewArtifact.decisions).toEqual([{
    source: 'formal legal phrase',
    decision: 'revise',
    target: '法律正式用语',
    rationale: 'Fixture reviewer rationale.'
  }]);
  expect(adapted.reviewArtifact.candidateSha256).toBe(
    createHash('sha256').update(adapted.candidateBytes).digest('hex')
  );
  expect(adapted.receipt).toMatchObject({
    mode: 'add',
    sourceDecisionCounts: { approve: 0, revise: 1, reject: 0 },
    emittedCandidateCount: 1,
    nonMutatingDecisions: [],
    excludedDecisions: []
  });

  const merged = mergeZhHansSenseInfoReviews({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    candidateArtifact: adapted.candidateArtifact,
    candidateSha256: adapted.reviewArtifact.candidateSha256,
    reviewArtifact: adapted.reviewArtifact
  });
  expect(merged.catalog.translations).toEqual([
    { source: 'formal legal phrase', target: '法律正式用语' }
  ]);
});

test('strictly adapts the register/freeform candidate and alternate reviewer shapes', () => {
  const richCandidates = {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    status: 'candidate-only',
    translator: 'Codex contextual pass; no external translation service',
    generatedFrom: {
      lqaReport: { path: 'work/private-lqa.json', sha256: 'f'.repeat(64) },
      sourceIdentities: generatedFrom
    },
    selection: {
      register: 'fixture register selection',
      namedEntity: 'fixture named-entity selection',
      freeform: 'fixture freeform selection',
      candidateCount: 1,
      occurrenceContextCount: 1,
      clusterCounts: { register: 1 }
    },
    selfReview: {
      candidatesSortedBySource: true,
      duplicateSourceCount: 0,
      identityTargetCount: 0,
      missingJapaneseTokenCount: 0,
      unbalancedTargetDelimiterCount: 0,
      latinTargetCount: 0,
      latinTargetsReviewed: [],
      mediumConfidenceCount: 1,
      flaggedCandidateCount: 1,
      terminologyReview: {},
      semanticReview: 'Independent review required.'
    },
    candidates: [{
      source: 'rare poetic term',
      cluster: 'register',
      sourceRisk: 'medium',
      target: '罕见诗语',
      translatorConfidence: 'medium',
      translatorRationale: 'Fixture contextual rationale.',
      uncertaintyFlags: ['register choice'],
      occurrenceCount: 1,
      occurrences: context(1)
    }]
  };
  const richReview = {
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    status: 'independent-review-complete',
    reviewer: 'Codex independent contextual LQA; no Apple or external translation service',
    reviewOf: { path: 'work/private-candidates.json', sha256: CANDIDATE_INPUT_SHA },
    closure: {
      inputCandidateCount: 1,
      inputUniqueSourceCount: 1,
      expectedUniqueSourceCount: 1,
      decisionCount: 1,
      decisionUniqueSourceCount: 1,
      exactSourceClosure: true,
      decisionsSortedBySource: true,
      sortComparator: 'source code-point order',
      reviewedOccurrenceContextCount: 1
    },
    summary: { approve: 1, revise: 0, reject: 0 },
    reviewPolicy: ['Review all attached contexts.'],
    decisions: [{
      source: 'rare poetic term',
      cluster: 'register',
      candidateTarget: '罕见诗语',
      decision: 'approve',
      reason: 'Fixture approval.',
      confidence: 'high',
      occurrenceCount: 1,
      reviewedOccurrenceCount: 1
    }]
  };
  const adapted = adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview,
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(adapted.candidateArtifact.candidates).toEqual([expect.objectContaining({
    source: 'rare poetic term',
    target: '罕见诗语',
    catalogAction: 'add',
    uncertainty: {
      level: 'medium',
      rationale: 'Fixture contextual rationale. Flags: register choice.'
    }
  })]);
  expect(adapted.reviewArtifact.decisions).toEqual([{
    source: 'rare poetic term',
    decision: 'approve',
    rationale: 'Fixture approval.'
  }]);

  const contextCountReview = structuredClone(richReview) as any;
  contextCountReview.reviewer =
    'Codex independent contextual LQA; no Apple or external machine translation service';
  contextCountReview.decisions[0].contextCount = 1;
  contextCountReview.decisions[0].reviewedContextCount = 1;
  delete contextCountReview.decisions[0].occurrenceCount;
  delete contextCountReview.decisions[0].reviewedOccurrenceCount;
  expect(adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: contextCountReview,
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  }).reviewArtifact.decisions).toHaveLength(1);

  const staleClosure = structuredClone(richReview) as any;
  staleClosure.closure.decisionCount = 0;
  expect(() => adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: staleClosure,
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  })).toThrow('closure or counts');
});

test('validates and receipts explicit aggregate batch-closure proofs', () => {
  const richCandidates = {
    ...richAddCandidate(),
    aggregateTwoBatchClosure: {
      artifacts: [
        { file: 'work/prior.json', sourceCount: 1 },
        { file: 'work/final.json', sourceCount: 1 }
      ],
      currentUnresolvedSourceCount: 2,
      currentUnresolvedOccurrenceCount: 2,
      currentUnresolvedClusterCounts: { fixture: 2 },
      priorBatchSourceCount: 1,
      finalBatchSourceCount: 1,
      uniqueCoveredSourceCount: 2,
      coveredSourceSha256: 'f'.repeat(64),
      missingSources: [],
      unexpectedSources: [],
      remainingUncoveredSourceCount: 0,
      complete: true
    }
  };
  const adapted = adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(adapted.receipt.aggregateClosures).toEqual([{
    field: 'aggregateTwoBatchClosure',
    ...richCandidates.aggregateTwoBatchClosure
  }]);

  const stale = structuredClone(richCandidates);
  stale.aggregateTwoBatchClosure.uniqueCoveredSourceCount = 3;
  expect(() => adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: stale,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  })).toThrow('closure counts are inconsistent');

  const arbitrary = { ...richCandidates, aggregateBatchNotes: {} };
  expect(() => adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: arbitrary,
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  })).toThrow('unknown fields');
});

test('validates legacy freeform and generic cluster aggregate closures', () => {
  const freeformClosure = {
    currentUnresolvedCount: 3,
    expectedOriginalUnresolvedCount: 3,
    priorRegisterFreeformCount: 1,
    priorEtymologyFreeformCount: 1,
    finalCount: 1,
    uniqueCoveredCount: 3,
    aggregateSourceSha256: 'e'.repeat(64),
    remainingCount: 0,
    complete: true,
    missingSources: [],
    unexpectedSources: [],
    crossArtifactOverlaps: []
  };
  const freeform = adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: { ...richAddCandidate(), aggregateFreeformClosure: freeformClosure },
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(freeform.receipt.aggregateClosures).toEqual([{
    field: 'aggregateFreeformClosure',
    ...freeformClosure
  }]);

  const genericClosure = {
    cluster: 'freeform',
    currentUnresolvedSourceCount: 2,
    expectedOriginalUnresolvedSourceCount: 2,
    priorArtifacts: [{ file: 'work/prior.json', sourceCount: 1 }],
    finalArtifact: { file: 'work/final.json', sourceCount: 1 },
    uniqueCoveredSourceCount: 2,
    coveredSourceSha256: 'd'.repeat(64),
    remainingUncoveredSourceCount: 0,
    complete: true,
    missingSources: [],
    unexpectedSources: [],
    crossArtifactOverlaps: []
  };
  const generic = adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: { ...richAddCandidate(), aggregateClusterClosure: genericClosure },
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(generic.receipt.aggregateClosures).toEqual([{
    field: 'aggregateClusterClosure',
    ...genericClosure
  }]);

  const stale = structuredClone(freeformClosure);
  stale.finalCount = 2;
  expect(() => adaptRichZhHansSenseInfoAddBatch({
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: { ...richAddCandidate(), aggregateFreeformClosure: stale },
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  })).toThrow('closure counts are inconsistent');
});

function richCatalogReview() {
  const checks = {
    contextCount: 1,
    japaneseFragmentsPreserved: true,
    missingJapaneseFragments: [],
    standardizedPunctuation: true
  };
  return {
    formatVersion: 1,
    kind: 'codex-zh-Hans-existing-sense-info-catalog-review',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    status: 'private-review-not-production',
    reviewPolicy: 'Independent Codex semantic/style review against every current JMdict occurrence; '
      + 'no Apple Translation or external machine translation',
    generatedFrom: identities,
    summary: {},
    selfLqa: {},
    reviews: [
      {
        source: 'formal legal phrase',
        currentTarget: '正式法律用语',
        decision: 'approve',
        proposedTarget: null,
        reviewerConfidence: 'high',
        reasons: ['Retain fixture translation.'],
        contexts: context(0),
        checks
      },
      {
        source: 'rare poetic term',
        currentTarget: '罕见诗语',
        decision: 'revise',
        proposedTarget: '古旧诗语',
        reviewerConfidence: 'high',
        reasons: ['Apply fixture revision.'],
        contexts: context(1),
        checks
      }
    ]
  };
}

test('adapts only explicit catalog revisions and receipts non-mutating approvals', () => {
  const catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [
      { source: 'formal legal phrase', target: '正式法律用语' },
      { source: 'rare poetic term', target: '罕见诗语' }
    ]
  });
  const adapted = adaptRichZhHansSenseInfoCatalogRevisions({
    entries,
    catalog,
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richReview: richCatalogReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(adapted.candidateArtifact.candidates).toHaveLength(1);
  expect(adapted.candidateArtifact.candidates[0]).toMatchObject({
    source: 'rare poetic term',
    target: '罕见诗语',
    catalogAction: 'revise',
    priorTarget: '罕见诗语'
  });
  expect(adapted.reviewArtifact.decisions).toEqual([{
    source: 'rare poetic term',
    decision: 'revise',
    target: '古旧诗语',
    rationale: 'Apply fixture revision.'
  }]);
  expect(adapted.receipt).toMatchObject({
    mode: 'revisions',
    sourceDecisionCounts: { approve: 1, revise: 1, reject: 0 },
    emittedCandidateCount: 1,
    nonMutatingDecisions: [{
      source: 'formal legal phrase',
      decision: 'approve',
      rationale: 'Retain fixture translation.'
    }],
    excludedDecisions: []
  });
  const merged = mergeZhHansSenseInfoReviews({
    entries,
    catalog,
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    candidateArtifact: adapted.candidateArtifact,
    candidateSha256: adapted.reviewArtifact.candidateSha256,
    reviewArtifact: adapted.reviewArtifact
  });
  expect(merged.catalog.translations).toEqual([
    { source: 'formal legal phrase', target: '正式法律用语' },
    { source: 'rare poetic term', target: '古旧诗语' }
  ]);
});

test('receipts a catalog revision that the direct policy now resolves', () => {
  const directEntry = entry(3, 'after an amount', 'following an amount');
  const directContext = [{
    seq: 3,
    sense: 0,
    info: 0,
    headwords: ['言葉3', 'ことば3'],
    englishGlosses: directEntry.senses[0]!.glosses
  }];
  const rich = richCatalogReview() as any;
  rich.reviews.unshift({
    source: 'after an amount',
    currentTarget: '接在表示金额或数量的词之后',
    decision: 'revise',
    proposedTarget: '接在表示数量、时长或金额的词之后',
    reviewerConfidence: 'high',
    reasons: ['Use the reviewed finite-rule wording.'],
    contexts: directContext,
    checks: {
      contextCount: 1,
      japaneseFragmentsPreserved: true,
      missingJapaneseFragments: [],
      standardizedPunctuation: true
    }
  });
  const catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [
      { source: 'after an amount', target: '接在表示金额或数量的词之后' },
      { source: 'formal legal phrase', target: '正式法律用语' },
      { source: 'rare poetic term', target: '罕见诗语' }
    ]
  });
  const adapted = adaptRichZhHansSenseInfoCatalogRevisions({
    entries: [...entries, directEntry],
    catalog,
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richReview: rich,
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  });
  expect(adapted.candidateArtifact.candidates.map(item => item.source)).toEqual([
    'rare poetic term'
  ]);
  expect(adapted.receipt.excludedDecisions).toEqual([{
    source: 'after an amount',
    decision: 'revise',
    rationale: 'Use the reviewed finite-rule wording.',
    reason: 'direct-rule-resolved'
  }]);
});

test('adapter rejects digest, context, identity, and run-separation failures', () => {
  const base = {
    entries,
    catalog: emptyCatalog(),
    jmdictIdentity: identities.jmdict,
    catalogIdentity: identities.catalog,
    richCandidates: richAddCandidate(),
    richCandidateSha256: CANDIDATE_INPUT_SHA,
    richReview: richAddReview(),
    richReviewSha256: REVIEW_INPUT_SHA,
    metadata
  };
  const wrongDigest = structuredClone(base) as any;
  wrongDigest.richReview.input.sha256 = 'f'.repeat(64);
  expect(() => adaptRichZhHansSenseInfoAddBatch(wrongDigest)).toThrow('different candidate');

  const staleContext = structuredClone(base) as any;
  staleContext.richCandidates.candidates[0].contexts[0].englishGlosses = ['changed context'];
  expect(() => adaptRichZhHansSenseInfoAddBatch(staleContext)).toThrow('stale or incomplete');

  const staleIdentity = structuredClone(base) as any;
  staleIdentity.jmdictIdentity = { ...staleIdentity.jmdictIdentity, sha256: 'f'.repeat(64) };
  expect(() => adaptRichZhHansSenseInfoAddBatch(staleIdentity)).toThrow('stale JMdict');

  const sameRun = structuredClone(base) as any;
  sameRun.metadata.reviewer.runId = sameRun.metadata.translator.runId;
  expect(() => adaptRichZhHansSenseInfoAddBatch(sameRun)).toThrow('distinct run IDs');
});
