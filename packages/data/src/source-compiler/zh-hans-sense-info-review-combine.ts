import { createHash } from 'node:crypto';

import {
  parseZhHansSenseInfoCandidateArtifact,
  parseZhHansSenseInfoReviewArtifact,
  type ZhHansSenseInfoCandidateArtifact,
  type ZhHansSenseInfoCodexReviewer,
  type ZhHansSenseInfoHumanReviewer,
  type ZhHansSenseInfoReviewArtifact,
  type ZhHansSenseInfoTranslator
} from './zh-hans-sense-info-review.js';

const SHA256 = /^[0-9a-f]{64}$/;

export interface ZhHansSenseInfoReviewPair {
  readonly candidateBytes: Uint8Array;
  readonly reviewBytes: Uint8Array;
}

export interface CombinedZhHansSenseInfoReview {
  readonly candidateArtifact: ZhHansSenseInfoCandidateArtifact;
  readonly candidateBytes: Uint8Array;
  readonly reviewArtifact: ZhHansSenseInfoReviewArtifact;
  readonly reviewBytes: Uint8Array;
}

interface VerifiedReviewPair {
  readonly candidateArtifact: ZhHansSenseInfoCandidateArtifact;
  readonly candidateSha256: string;
  readonly reviewArtifact: ZhHansSenseInfoReviewArtifact;
  readonly reviewSha256: string;
}

function digest(value: string, label: string): string {
  if (!SHA256.test(value)) throw new Error(`${label} must be a lowercase SHA-256 digest`);
  return value;
}

function hash(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function serialize(value: unknown): Uint8Array {
  return new TextEncoder().encode(`${JSON.stringify(value, null, 2)}\n`);
}

function parseJson(bytes: Uint8Array, label: string): unknown {
  try {
    return JSON.parse(new TextDecoder().decode(bytes));
  } catch {
    throw new Error(`${label} is not valid JSON`);
  }
}

function translatorRuns(
  artifact: ZhHansSenseInfoCandidateArtifact
): readonly ZhHansSenseInfoTranslator[] {
  return artifact.translator.kind === 'combined'
    ? artifact.translator.runs
    : [artifact.translator];
}

type ReviewerContributor = ZhHansSenseInfoHumanReviewer | ZhHansSenseInfoCodexReviewer;

function reviewerContributors(
  artifact: ZhHansSenseInfoReviewArtifact
): readonly ReviewerContributor[] {
  return artifact.reviewer.kind === 'combined'
    ? artifact.reviewer.reviewers
    : [artifact.reviewer];
}

function reviewerKey(reviewer: ReviewerContributor): string {
  return reviewer.kind === 'codex' ? `codex:${reviewer.runId}` : `human:${reviewer.id}`;
}

function assertPairCoverage(pair: VerifiedReviewPair, index: number): void {
  if (pair.reviewArtifact.candidateSha256 !== pair.candidateSha256) {
    throw new Error(`Combined input pair ${index} review has the wrong candidate digest`);
  }
  const candidates = pair.candidateArtifact.candidates;
  const decisions = pair.reviewArtifact.decisions;
  if (candidates.length !== decisions.length
    || candidates.some((item, itemIndex) => item.source !== decisions[itemIndex]?.source)) {
    throw new Error(`Combined input pair ${index} review does not cover its candidates`);
  }
}

/**
 * Mechanically combine independently reviewed batches that target the same
 * pinned corpus, catalog, and direct-rule policy. No translation is selected or
 * rewritten here; source collisions fail closed.
 */
export function combineZhHansSenseInfoReviewPairs(
  inputPairs: readonly ZhHansSenseInfoReviewPair[]
): CombinedZhHansSenseInfoReview {
  if (inputPairs.length < 2) throw new Error('At least two review pairs are required to combine');
  const pairs = inputPairs.map((pair, index) => {
    const candidateSha256 = digest(
      hash(pair.candidateBytes),
      `Combined input pair ${index} candidate digest`
    );
    const reviewSha256 = digest(hash(pair.reviewBytes), `Combined input pair ${index} review digest`);
    const candidateArtifact = parseZhHansSenseInfoCandidateArtifact(
      parseJson(pair.candidateBytes, `Combined input pair ${index} candidate`)
    );
    const reviewArtifact = parseZhHansSenseInfoReviewArtifact(
      parseJson(pair.reviewBytes, `Combined input pair ${index} review`)
    );
    const parsed = { candidateSha256, reviewSha256, candidateArtifact, reviewArtifact };
    assertPairCoverage(parsed, index);
    return parsed;
  });
  const generatedFrom = JSON.stringify(pairs[0]!.candidateArtifact.generatedFrom);
  if (pairs.some(pair => JSON.stringify(pair.candidateArtifact.generatedFrom) !== generatedFrom)) {
    throw new Error('Combined review pairs do not share corpus, catalog, and policy identities');
  }

  const allRunIds = new Set<string>();
  const translators: ZhHansSenseInfoTranslator[] = [];
  const reviewers = new Map<string, ReviewerContributor>();
  for (const [index, pair] of pairs.entries()) {
    for (const translator of translatorRuns(pair.candidateArtifact)) {
      if (allRunIds.has(translator.runId)) {
        throw new Error(`Duplicate translator/reviewer run ID: ${translator.runId}`);
      }
      allRunIds.add(translator.runId);
      translators.push(translator);
    }
    for (const reviewer of reviewerContributors(pair.reviewArtifact)) {
      if (reviewer.kind === 'codex') {
        if (allRunIds.has(reviewer.runId)) {
          throw new Error(`Duplicate translator/reviewer run ID: ${reviewer.runId}`);
        }
        allRunIds.add(reviewer.runId);
      }
      const key = reviewerKey(reviewer);
      const existing = reviewers.get(key);
      if (existing && JSON.stringify(existing) !== JSON.stringify(reviewer)) {
        throw new Error(`Conflicting reviewer identity in combined pair ${index}: ${key}`);
      }
      reviewers.set(key, reviewer);
    }
  }
  translators.sort((left, right) => left.runId < right.runId ? -1 : 1);
  const sortedReviewers = [...reviewers].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0).map(([, reviewer]) => reviewer);

  const candidates = pairs.flatMap(pair => pair.candidateArtifact.candidates).sort((left, right) =>
    left.source < right.source ? -1 : left.source > right.source ? 1 : 0);
  const decisions = pairs.flatMap(pair => pair.reviewArtifact.decisions).sort((left, right) =>
    left.source < right.source ? -1 : left.source > right.source ? 1 : 0);
  for (let index = 1; index < candidates.length; index++) {
    if (candidates[index - 1]!.source === candidates[index]!.source) {
      throw new Error(`Duplicate source across combined batches: ${candidates[index]!.source}`);
    }
  }
  if (candidates.some((item, index) => item.source !== decisions[index]?.source)) {
    throw new Error('Combined decisions do not align with combined candidates');
  }
  const batches = pairs.map(pair => ({
    candidateSha256: pair.candidateSha256,
    reviewSha256: pair.reviewSha256
  })).sort((left, right) => left.candidateSha256 < right.candidateSha256 ? -1 : 1);
  for (let index = 1; index < batches.length; index++) {
    if (batches[index - 1]!.candidateSha256 === batches[index]!.candidateSha256) {
      throw new Error('Duplicate candidate artifact digest across combined batches');
    }
  }
  const origin = { kind: 'combined' as const, batches };
  const candidateArtifact = parseZhHansSenseInfoCandidateArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: pairs[0]!.candidateArtifact.generatedFrom,
    origin,
    translator: { kind: 'combined', runs: translators },
    candidates
  });
  const candidateBytes = serialize(candidateArtifact);
  const reviewArtifact = parseZhHansSenseInfoReviewArtifact({
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-decisions',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    candidateSha256: hash(candidateBytes),
    origin,
    reviewer: { kind: 'combined', reviewers: sortedReviewers },
    decisions
  });
  return {
    candidateArtifact,
    candidateBytes,
    reviewArtifact,
    reviewBytes: serialize(reviewArtifact)
  };
}
