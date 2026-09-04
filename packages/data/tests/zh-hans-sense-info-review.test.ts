import { expect, test } from 'bun:test';

import { parseJmdictEntry } from '../src/source-compiler/jmdict.js';
import {
  ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY,
  assertCodexCandidateArtifactPath,
  assertPlausibleZhHansSenseInfoTarget,
  buildZhHansSenseInfoContextIndex,
  emptyZhHansSenseInfoReviewProvenance,
  mergeZhHansSenseInfoReviews,
  parseZhHansSenseInfoCandidateArtifact,
  parseZhHansSenseInfoReviewArtifact,
  parseZhHansSenseInfoReviewProvenance,
  type MergeZhHansSenseInfoReviewOptions,
  type ZhHansSenseInfoCandidateArtifact,
  type ZhHansSenseInfoReviewArtifact
} from '../src/source-compiler/zh-hans-sense-info-review.js';
import { parseZhHansSenseInfoCatalog } from '../src/source-compiler/zh-hans-sense-info.js';
import { ZH_HANS_SENSE_INFO_PATTERN_POLICY } from '../src/source-compiler/zh-hans-sense-info-patterns.js';

const CANDIDATE_SHA = 'a'.repeat(64);
const JMDICT_SHA = 'b'.repeat(64);
const CATALOG_SHA = 'c'.repeat(64);
const GENERATED_AT = '2026-09-04T12:00:00.000Z';
const REVIEWED_AT = '2026-09-04T13:00:00.000Z';

function entry(seq: number, notes: readonly string[], gloss: string) {
  return parseJmdictEntry(
    `<entry><ent_seq>${seq}</ent_seq><k_ele><keb>言葉${seq}</keb></k_ele>`
    + `<r_ele><reb>ことば${seq}</reb></r_ele><sense>`
    + notes.map(note => `<s_inf>${note}</s_inf>`).join('')
    + `<gloss>${gloss}</gloss></sense></entry>`,
    'fixture',
    seq
  );
}

const entries = [
  entry(1, ['dated legal expression'], 'old legal wording'),
  entry(2, ['dated legal expression'], 'archaic legal phrase'),
  entry(3, ['rare poetic term'], 'rare poetic word'),
  entry(4, ['used in formal correspondence'], 'formal correspondence usage')
];
const contexts = buildZhHansSenseInfoContextIndex(entries);

function candidateArtifact(): ZhHansSenseInfoCandidateArtifact {
  return parseZhHansSenseInfoCandidateArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: {
      jmdict: { id: 'fixture-jmdict', sha256: JMDICT_SHA },
      catalog: { id: 'fixture-catalog', sha256: CATALOG_SHA },
      patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY
    },
    origin: { kind: 'native' },
    translator: {
      kind: 'codex',
      provider: 'openai',
      model: 'gpt-fixture',
      runId: 'fixture-run',
      generatedAt: GENERATED_AT,
      sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
    },
    candidates: [
      {
        source: 'dated legal expression',
        target: '法律旧语',
        catalogAction: 'add',
        uncertainty: { level: 'low', rationale: 'Standard register wording.' },
        contexts: contexts.get('dated legal expression')
      },
      {
        source: 'rare poetic term',
        target: '旧诗语',
        catalogAction: 'revise',
        priorTarget: '罕见诗语',
        uncertainty: { level: 'medium', rationale: 'Register nuance needs review.' },
        contexts: contexts.get('rare poetic term')
      },
      {
        source: 'used in formal correspondence',
        target: '用于正式通信',
        catalogAction: 'add',
        uncertainty: { level: 'low', rationale: 'Literal usage note.' },
        contexts: contexts.get('used in formal correspondence')
      }
    ]
  });
}

function reviewArtifact(): ZhHansSenseInfoReviewArtifact {
  return parseZhHansSenseInfoReviewArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-decisions',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    candidateSha256: CANDIDATE_SHA,
    origin: { kind: 'native' },
    reviewer: {
      kind: 'codex',
      provider: 'openai',
      model: 'gpt-review-fixture',
      runId: 'fixture-review-run',
      reviewedAt: REVIEWED_AT,
      sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
    },
    decisions: [
      { source: 'dated legal expression', decision: 'approve', rationale: 'Accurate.' },
      {
        source: 'rare poetic term',
        decision: 'revise',
        target: '古旧诗语',
        rationale: 'This better preserves the register.'
      },
      {
        source: 'used in formal correspondence',
        decision: 'reject',
        rationale: 'Needs more domain context.'
      }
    ]
  });
}

function options(): MergeZhHansSenseInfoReviewOptions {
  return {
    entries,
    catalog: parseZhHansSenseInfoCatalog({
      formatVersion: 1,
      locale: 'zh-Hans',
      sourceLocale: 'en',
      translations: [{ source: 'rare poetic term', target: '罕见诗语' }]
    }),
    catalogIdentity: { id: 'fixture-catalog', sha256: CATALOG_SHA },
    jmdictIdentity: { id: 'fixture-jmdict', sha256: JMDICT_SHA },
    candidateArtifact: candidateArtifact(),
    candidateSha256: CANDIDATE_SHA,
    reviewArtifact: reviewArtifact(),
    provenance: emptyZhHansSenseInfoReviewProvenance()
  };
}

test('strictly parses Codex candidates, tagged reviewers, and provenance', () => {
  const candidates = candidateArtifact();
  const review = reviewArtifact();
  expect(candidates.candidates).toHaveLength(3);
  expect(candidates.candidates[0]?.contexts).toHaveLength(2);
  expect(review.decisions.map(item => item.decision)).toEqual(['approve', 'revise', 'reject']);

  const merged = mergeZhHansSenseInfoReviews(options());
  expect(parseZhHansSenseInfoReviewProvenance(
    JSON.parse(JSON.stringify(merged.provenance))
  )).toEqual(merged.provenance);
  expect(merged.provenance.formatVersion).toBe(2);
  expect(Object.keys(merged.provenance.batches)).toEqual([CANDIDATE_SHA]);
  expect(JSON.stringify(merged.provenance)).not.toContain('"contexts"');

  const migrated = parseZhHansSenseInfoReviewProvenance({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-provenance',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    records: [{
      source: candidates.candidates[0]!.source,
      candidateSha256: CANDIDATE_SHA,
      candidateTarget: candidates.candidates[0]!.target,
      catalogAction: 'add',
      priorTarget: null,
      decision: 'approve',
      finalTarget: candidates.candidates[0]!.target,
      generatedFrom: {
        ...candidates.generatedFrom,
        patternPolicy: 'jmdict-s-inf-zh-Hans-patterns-v1'
      },
      candidateOrigin: candidates.origin,
      reviewOrigin: review.origin,
      translator: candidates.translator,
      uncertainty: candidates.candidates[0]!.uncertainty,
      contexts: candidates.candidates[0]!.contexts,
      reviewer: review.reviewer,
      rationale: review.decisions[0]!.rationale
    }]
  });
  expect(migrated).toMatchObject({
    formatVersion: 2,
    batches: {
      [CANDIDATE_SHA]: {
        generatedFrom: { patternPolicy: 'jmdict-s-inf-zh-Hans-patterns-v1' }
      }
    },
    decisions: [{ source: 'dated legal expression', batchSha256: CANDIDATE_SHA }]
  });
  expect(JSON.stringify(migrated)).not.toContain('"contexts"');
  const unsupportedProvenancePolicy = structuredClone(migrated) as any;
  unsupportedProvenancePolicy.batches[CANDIDATE_SHA].generatedFrom.patternPolicy =
    'jmdict-s-inf-zh-Hans-patterns-v0';
  expect(() => parseZhHansSenseInfoReviewProvenance(unsupportedProvenancePolicy))
    .toThrow('unsupported historical pattern policy');
  const historicalCandidate = structuredClone(candidates) as any;
  historicalCandidate.generatedFrom.patternPolicy = 'jmdict-s-inf-zh-Hans-patterns-v1';
  expect(() => parseZhHansSenseInfoCandidateArtifact(historicalCandidate))
    .toThrow('unsupported pattern policy');

  const unknownField = structuredClone(candidates) as unknown as Record<string, unknown>;
  unknownField.generator = 'not allowed';
  expect(() => parseZhHansSenseInfoCandidateArtifact(unknownField)).toThrow('unknown fields');

  const external = structuredClone(candidates) as any;
  external.translator.kind = 'apple';
  expect(() => parseZhHansSenseInfoCandidateArtifact(external)).toThrow('Codex/OpenAI');
  const externalReviewer = structuredClone(review) as any;
  externalReviewer.reviewer.provider = 'apple';
  expect(() => parseZhHansSenseInfoReviewArtifact(externalReviewer)).toThrow('OpenAI');
  const humanReview = structuredClone(review) as any;
  humanReview.reviewer = {
    kind: 'human',
    id: 'reviewer-1',
    displayName: 'Fixture Reviewer',
    reviewedAt: REVIEWED_AT
  };
  expect(parseZhHansSenseInfoReviewArtifact(humanReview).reviewer.kind).toBe('human');

  const unsorted = structuredClone(candidates) as any;
  unsorted.candidates.reverse();
  expect(() => parseZhHansSenseInfoCandidateArtifact(unsorted)).toThrow('unique and sorted');
  const duplicateDecision = structuredClone(review) as any;
  duplicateDecision.decisions[1].source = duplicateDecision.decisions[0].source;
  expect(() => parseZhHansSenseInfoReviewArtifact(duplicateDecision)).toThrow('unique and sorted');

  const malformedDate = structuredClone(candidates) as any;
  malformedDate.translator.generatedAt = '2026-09-04';
  expect(() => parseZhHansSenseInfoCandidateArtifact(malformedDate)).toThrow('canonical ISO-8601');
  const malformedContext = structuredClone(candidates) as any;
  malformedContext.candidates[0].contexts[0].contextSha256 = 'A'.repeat(64);
  expect(() => parseZhHansSenseInfoCandidateArtifact(malformedContext)).toThrow('lowercase SHA-256');
});

test('merges only approved/revised pairs and keeps workflow metadata out of the catalog', () => {
  const merged = mergeZhHansSenseInfoReviews(options());
  expect(merged.catalog.translations).toEqual([
    { source: 'dated legal expression', target: '法律旧语' },
    { source: 'rare poetic term', target: '古旧诗语' }
  ]);
  expect(merged.stats).toEqual({
    candidates: 3,
    approved: 1,
    revised: 1,
    rejected: 1,
    catalogAdded: 1,
    catalogRevised: 1
  });
  expect(merged.provenance.decisions).toHaveLength(3);
  expect(merged.provenance.decisions[2]).toMatchObject({
    source: 'used in formal correspondence',
    decision: 'reject',
    finalTarget: null
  });
  expect(merged.provenance.batches[CANDIDATE_SHA]).toMatchObject({
    reviewer: { kind: 'codex', provider: 'openai', runId: 'fixture-review-run' },
    translator: { kind: 'codex', provider: 'openai' }
  });
  expect(Object.keys(merged.catalog.translations[0]!).sort()).toEqual(['source', 'target']);
});

test('validates promoted targets while allowing review to repair or reject suspicious drafts', () => {
  const repaired = options();
  repaired.entries = [entry(5, ['Foucauldian term'], 'term used in Foucauldian theory')];
  const repairContexts = buildZhHansSenseInfoContextIndex(repaired.entries);
  repaired.catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: []
  });
  repaired.candidateArtifact = {
    ...repaired.candidateArtifact,
    candidates: [{
      ...repaired.candidateArtifact.candidates[0]!,
      source: 'Foucauldian term',
      target: 'Foucault 理论用语',
      contexts: repairContexts.get('Foucauldian term')!
    }]
  };
  repaired.reviewArtifact = {
    ...repaired.reviewArtifact,
    decisions: [{
      source: 'Foucauldian term',
      decision: 'revise',
      target: '福柯理论用语',
      rationale: 'Replace the repairable Latin draft with the reviewed name.'
    }]
  };

  const repairedResult = mergeZhHansSenseInfoReviews(repaired);
  expect(repairedResult.catalog.translations).toEqual([{
    source: 'Foucauldian term',
    target: '福柯理论用语'
  }]);
  expect(repairedResult.provenance.decisions.find(
    record => record.source === 'Foucauldian term'
  )).toMatchObject({
    candidateTarget: 'Foucault 理论用语',
    decision: 'revise',
    finalTarget: '福柯理论用语'
  });

  const rejectedIdentity = options();
  const rejectedCandidates = [...rejectedIdentity.candidateArtifact.candidates];
  rejectedCandidates[2] = {
    ...rejectedCandidates[2]!,
    target: rejectedCandidates[2]!.source
  };
  rejectedIdentity.candidateArtifact = {
    ...rejectedIdentity.candidateArtifact,
    candidates: rejectedCandidates
  };
  expect(mergeZhHansSenseInfoReviews(rejectedIdentity).provenance.decisions.find(
    record => record.source === 'used in formal correspondence'
  )).toMatchObject({
    candidateTarget: 'used in formal correspondence',
    decision: 'reject',
    finalTarget: null
  });

  for (const target of ['TODO中文', '中文（']) {
    const malformed = options();
    const candidates = [...malformed.candidateArtifact.candidates];
    candidates[2] = { ...candidates[2]!, target };
    malformed.candidateArtifact = { ...malformed.candidateArtifact, candidates };
    expect(() => mergeZhHansSenseInfoReviews(malformed)).toThrow('Suspicious zh-Hans target');
  }
});

test('rejects missing, incomplete, or stale review bindings', () => {
  const missingDecision = options();
  missingDecision.reviewArtifact = {
    ...missingDecision.reviewArtifact,
    decisions: missingDecision.reviewArtifact.decisions.slice(0, 2)
  };
  expect(() => mergeZhHansSenseInfoReviews(missingDecision)).toThrow('cover every candidate');

  const wrongDigest = options();
  wrongDigest.candidateSha256 = 'd'.repeat(64);
  expect(() => mergeZhHansSenseInfoReviews(wrongDigest)).toThrow('different candidate');

  const sameRun = options();
  sameRun.reviewArtifact = {
    ...sameRun.reviewArtifact,
    reviewer: {
      ...sameRun.reviewArtifact.reviewer as Extract<
        typeof sameRun.reviewArtifact.reviewer,
        { kind: 'codex' }
      >,
      runId: sameRun.candidateArtifact.translator.runId
    }
  };
  expect(() => mergeZhHansSenseInfoReviews(sameRun)).toThrow('distinct run IDs');

  const staleIdentity = options();
  staleIdentity.jmdictIdentity = { id: 'fixture-jmdict', sha256: 'd'.repeat(64) };
  expect(() => mergeZhHansSenseInfoReviews(staleIdentity)).toThrow('stale JMdict');

  const staleContext = options();
  const candidates = [...staleContext.candidateArtifact.candidates];
  candidates[0] = { ...candidates[0]!, contexts: candidates[0]!.contexts.slice(0, 1) };
  staleContext.candidateArtifact = { ...staleContext.candidateArtifact, candidates };
  expect(() => mergeZhHansSenseInfoReviews(staleContext)).toThrow('stale or incomplete');

  const missingSource = options();
  const renamed = missingSource.candidateArtifact.candidates.map((item, index) => index === 0
    ? { ...item, source: 'absent corpus source' }
    : item);
  missingSource.candidateArtifact = { ...missingSource.candidateArtifact, candidates: renamed };
  const decisions = missingSource.reviewArtifact.decisions.map((item, index) => index === 0
    ? { ...item, source: 'absent corpus source' }
    : item);
  missingSource.reviewArtifact = { ...missingSource.reviewArtifact, decisions };
  expect(() => mergeZhHansSenseInfoReviews(missingSource)).toThrow('missing from the current corpus');
});

test('rejects direct-rule sources, catalog conflicts, and non-explicit overwrites', () => {
  const ruleEntries = [entry(10, ['also written as 退く'], 'to retreat')];
  const ruleContexts = buildZhHansSenseInfoContextIndex(ruleEntries);
  const value = options();
  value.entries = ruleEntries;
  value.candidateArtifact = {
    ...value.candidateArtifact,
    candidates: [{
      source: 'also written as 退く',
      target: '也可写作「退く」',
      catalogAction: 'add',
      uncertainty: { level: 'none', rationale: 'Closed wording.' },
      contexts: ruleContexts.get('also written as 退く')!
    }]
  };
  value.reviewArtifact = {
    ...value.reviewArtifact,
    decisions: [{ source: 'also written as 退く', decision: 'approve', rationale: 'Accurate.' }]
  };
  expect(() => mergeZhHansSenseInfoReviews(value)).toThrow('resolved by a direct rule');

  const overwrite = options();
  overwrite.catalog = parseZhHansSenseInfoCatalog({
    formatVersion: 1,
    locale: 'zh-Hans',
    sourceLocale: 'en',
    translations: [
      { source: 'dated legal expression', target: '既有译文' },
      { source: 'rare poetic term', target: '罕见诗语' }
    ]
  });
  expect(() => mergeZhHansSenseInfoReviews(overwrite)).toThrow('overwrite an existing');

  const stalePrior = options();
  const revised = [...stalePrior.candidateArtifact.candidates];
  revised[1] = { ...revised[1]!, catalogAction: 'revise', priorTarget: '错误旧译' };
  stalePrior.candidateArtifact = { ...stalePrior.candidateArtifact, candidates: revised };
  expect(() => mergeZhHansSenseInfoReviews(stalePrior)).toThrow('stale prior target');

  const replay = options();
  const first = mergeZhHansSenseInfoReviews(replay);
  replay.provenance = first.provenance;
  expect(() => mergeZhHansSenseInfoReviews(replay)).toThrow('already ingested');

  const legacyReplay = options();
  const legacyFirst = mergeZhHansSenseInfoReviews(legacyReplay);
  legacyReplay.provenance = parseZhHansSenseInfoReviewProvenance({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-provenance',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    records: legacyFirst.provenance.decisions.map(decision => {
      const batch = legacyFirst.provenance.batches[decision.batchSha256]!;
      const sourceCandidate = legacyReplay.candidateArtifact.candidates.find(
        candidate => candidate.source === decision.source
      )!;
      return {
        source: decision.source,
        candidateSha256: decision.batchSha256,
        candidateTarget: decision.candidateTarget,
        catalogAction: decision.catalogAction,
        priorTarget: decision.priorTarget,
        decision: decision.decision,
        finalTarget: decision.finalTarget,
        generatedFrom: batch.generatedFrom,
        candidateOrigin: batch.candidateOrigin,
        reviewOrigin: batch.reviewOrigin,
        translator: batch.translator,
        uncertainty: decision.uncertainty,
        contexts: sourceCandidate.contexts,
        reviewer: batch.reviewer,
        rationale: decision.rationale
      };
    })
  });
  expect(() => mergeZhHansSenseInfoReviews(legacyReplay)).toThrow('already ingested');
});

test('rejects suspicious translations and legacy Apple/external-MT draft paths', () => {
  for (const target of ['contains english', '中文（', 'TODO中文', 'dated legal expression']) {
    const invalid = options();
    const candidates = [...invalid.candidateArtifact.candidates];
    candidates[0] = { ...candidates[0]!, target };
    invalid.candidateArtifact = { ...invalid.candidateArtifact, candidates };
    expect(() => mergeZhHansSenseInfoReviews(invalid)).toThrow('Suspicious zh-Hans target');
  }
  expect(() => assertCodexCandidateArtifactPath(
    'work/zh-hans-sense-info-drafts.apple.json'
  )).toThrow('non-Codex');
  expect(() => assertCodexCandidateArtifactPath('work/batch-external-mt.json')).toThrow('non-Codex');
  expect(() => assertCodexCandidateArtifactPath('work/codex-candidates.json')).not.toThrow();
  expect(() => assertPlausibleZhHansSenseInfoTarget(
    '接在变量 n 之后',
    'after variable n'
  )).not.toThrow();
  expect(() => assertPlausibleZhHansSenseInfoTarget(
    '源自杂志 Punch',
    "from the magazine 'Punch'"
  )).not.toThrow();
});
