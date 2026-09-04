import { createHash } from 'node:crypto';

import type { CanonicalEntry } from './model.js';
import {
  ZH_HANS_SENSE_INFO_PATTERN_POLICY,
  translateZhHansSenseInfoPattern
} from './zh-hans-sense-info-patterns.js';
import type {
  ZhHansSenseInfoCatalog,
  ZhHansSenseInfoTranslation
} from './zh-hans-sense-info.js';

export const ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY =
  'codex-original-no-apple-or-external-mt-v1';
export const ZH_HANS_SENSE_INFO_HISTORICAL_PATTERN_POLICY =
  'jmdict-s-inf-zh-Hans-patterns-v1';

export interface ZhHansSenseInfoSourceIdentity {
  readonly id: string;
  readonly sha256: string;
}

export interface ZhHansSenseInfoCandidateContext {
  readonly seq: number;
  readonly sense: number;
  readonly info: number;
  readonly contextSha256: string;
}

export interface ZhHansSenseInfoTranslator {
  readonly kind: 'codex';
  readonly provider: 'openai';
  readonly model: string;
  readonly runId: string;
  readonly generatedAt: string;
  readonly sourcePolicy: typeof ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY;
}

export interface ZhHansSenseInfoCombinedTranslators {
  readonly kind: 'combined';
  readonly runs: readonly ZhHansSenseInfoTranslator[];
}

export type ZhHansSenseInfoTranslatorAttribution =
  | ZhHansSenseInfoTranslator
  | ZhHansSenseInfoCombinedTranslators;

export type ZhHansSenseInfoArtifactOrigin = {
  readonly kind: 'native';
} | {
  readonly kind: 'adapted';
  readonly sourceKind: string;
  readonly sha256: string;
} | {
  readonly kind: 'combined';
  readonly batches: readonly {
    readonly candidateSha256: string;
    readonly reviewSha256: string;
  }[];
};

export interface ZhHansSenseInfoUncertainty {
  readonly level: 'none' | 'low' | 'medium' | 'high';
  readonly rationale: string;
}

export type ZhHansSenseInfoCandidate = {
  readonly source: string;
  readonly target: string;
  readonly uncertainty: ZhHansSenseInfoUncertainty;
  readonly contexts: readonly ZhHansSenseInfoCandidateContext[];
} & ({
  readonly catalogAction: 'add';
} | {
  readonly catalogAction: 'revise';
  readonly priorTarget: string;
});

export interface ZhHansSenseInfoCandidateArtifact {
  readonly formatVersion: 1;
  readonly kind: 'zh-hans-sense-info-codex-candidates';
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  readonly generatedFrom: {
    readonly jmdict: ZhHansSenseInfoSourceIdentity;
    readonly catalog: ZhHansSenseInfoSourceIdentity;
    readonly patternPolicy: typeof ZH_HANS_SENSE_INFO_PATTERN_POLICY;
  };
  /** Raw rich-source digest when a shape-only adapter produced this artifact. */
  readonly origin: ZhHansSenseInfoArtifactOrigin;
  readonly translator: ZhHansSenseInfoTranslatorAttribution;
  readonly candidates: readonly ZhHansSenseInfoCandidate[];
}

export type ZhHansSenseInfoReviewerDecision = {
  readonly source: string;
  readonly rationale: string;
} & ({
  readonly decision: 'approve' | 'reject';
} | {
  readonly decision: 'revise';
  readonly target: string;
});

export interface ZhHansSenseInfoHumanReviewer {
  readonly kind: 'human';
  readonly id: string;
  readonly displayName: string;
  readonly reviewedAt: string;
}

export interface ZhHansSenseInfoCodexReviewer {
  readonly kind: 'codex';
  readonly provider: 'openai';
  readonly model: string;
  readonly runId: string;
  readonly reviewedAt: string;
  readonly sourcePolicy: typeof ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY;
}

export type ZhHansSenseInfoReviewer =
  | ZhHansSenseInfoHumanReviewer
  | ZhHansSenseInfoCodexReviewer
  | {
    /** Mechanical attribution container; not an additional semantic reviewer. */
    readonly kind: 'combined';
    readonly reviewers: readonly (ZhHansSenseInfoHumanReviewer | ZhHansSenseInfoCodexReviewer)[];
  };

export interface ZhHansSenseInfoReviewArtifact {
  readonly formatVersion: 1;
  readonly kind: 'zh-hans-sense-info-review-decisions';
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  readonly candidateSha256: string;
  /** Raw rich-source digest when a shape-only adapter produced this artifact. */
  readonly origin: ZhHansSenseInfoArtifactOrigin;
  readonly reviewer: ZhHansSenseInfoReviewer;
  readonly decisions: readonly ZhHansSenseInfoReviewerDecision[];
}

export interface ZhHansSenseInfoReviewProvenanceBatch {
  readonly generatedFrom: {
    readonly jmdict: ZhHansSenseInfoSourceIdentity;
    readonly catalog: ZhHansSenseInfoSourceIdentity;
    readonly patternPolicy:
      | typeof ZH_HANS_SENSE_INFO_PATTERN_POLICY
      | typeof ZH_HANS_SENSE_INFO_HISTORICAL_PATTERN_POLICY;
  };
  readonly candidateOrigin: ZhHansSenseInfoArtifactOrigin;
  readonly reviewOrigin: ZhHansSenseInfoArtifactOrigin;
  readonly translator: ZhHansSenseInfoTranslatorAttribution;
  readonly reviewer: ZhHansSenseInfoReviewer;
}

export interface ZhHansSenseInfoReviewProvenanceDecision {
  readonly source: string;
  /** Digest-keyed reference to the immutable strict candidate artifact. */
  readonly batchSha256: string;
  readonly candidateTarget: string;
  readonly catalogAction: 'add' | 'revise';
  readonly priorTarget: string | null;
  readonly decision: 'approve' | 'revise' | 'reject';
  readonly finalTarget: string | null;
  readonly uncertainty: ZhHansSenseInfoUncertainty;
  readonly rationale: string;
}

export interface ZhHansSenseInfoReviewProvenance {
  readonly formatVersion: 2;
  readonly kind: 'zh-hans-sense-info-review-provenance';
  readonly locale: 'zh-Hans';
  readonly sourceLocale: 'en';
  /** Shared batch metadata keyed by the strict candidate artifact SHA-256. */
  readonly batches: Readonly<Record<string, ZhHansSenseInfoReviewProvenanceBatch>>;
  /** Contexts are reproduced from the digest-bound candidate artifact and canonical corpus. */
  readonly decisions: readonly ZhHansSenseInfoReviewProvenanceDecision[];
}

export interface MergeZhHansSenseInfoReviewOptions {
  readonly entries: readonly CanonicalEntry[];
  readonly catalog: ZhHansSenseInfoCatalog;
  readonly catalogIdentity: ZhHansSenseInfoSourceIdentity;
  readonly jmdictIdentity: ZhHansSenseInfoSourceIdentity;
  readonly candidateArtifact: ZhHansSenseInfoCandidateArtifact;
  readonly candidateSha256: string;
  readonly reviewArtifact: ZhHansSenseInfoReviewArtifact;
  readonly provenance?: ZhHansSenseInfoReviewProvenance;
}

export interface MergeZhHansSenseInfoReviewResult {
  readonly catalog: ZhHansSenseInfoCatalog;
  readonly provenance: ZhHansSenseInfoReviewProvenance;
  readonly stats: {
    readonly candidates: number;
    readonly approved: number;
    readonly revised: number;
    readonly rejected: number;
    readonly catalogAdded: number;
    readonly catalogRevised: number;
  };
}

const SHA256 = /^[0-9a-f]{64}$/;
const HAN = /\p{Script=Han}/u;
const ASCII_WORD = /[A-Za-z]+/g;
const CONTROL_OR_REPLACEMENT = /[\u0000-\u001f\u007f-\u009f\ufffd]/u;
const FORBIDDEN_CANDIDATE_BASENAME = /^zh-hans-sense-info-drafts(?:[.-].*)?\.json$/i;

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function exactKeys(
  value: Record<string, unknown>,
  expected: readonly string[],
  label: string
): void {
  const keys = Object.keys(value);
  const unknown = keys.filter(key => !expected.includes(key));
  const missing = expected.filter(key => !keys.includes(key));
  if (unknown.length > 0) throw new Error(`${label} has unknown fields: ${unknown.join(', ')}`);
  if (missing.length > 0) throw new Error(`${label} is missing fields: ${missing.join(', ')}`);
}

function nonemptyText(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0 || value !== value.trim()) {
    throw new Error(`${label} must be trimmed, non-empty text`);
  }
  return value;
}

function sha256Text(value: unknown, label: string): string {
  const text = nonemptyText(value, label);
  if (!SHA256.test(text)) throw new Error(`${label} must be a lowercase SHA-256 digest`);
  return text;
}

function positiveInteger(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
}

function canonicalTimestamp(value: unknown, label: string): string {
  const text = nonemptyText(value, label);
  if (Number.isNaN(Date.parse(text)) || new Date(text).toISOString() !== text) {
    throw new Error(`${label} must be a canonical ISO-8601 timestamp`);
  }
  return text;
}

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function parseIdentity(value: unknown, label: string): ZhHansSenseInfoSourceIdentity {
  const identity = record(value, label);
  exactKeys(identity, ['id', 'sha256'], label);
  return {
    id: nonemptyText(identity.id, `${label} id`),
    sha256: sha256Text(identity.sha256, `${label} sha256`)
  };
}

function parseGeneratedFrom(
  value: unknown,
  label: string
): ZhHansSenseInfoCandidateArtifact['generatedFrom'] {
  const generatedFrom = record(value, label);
  exactKeys(generatedFrom, ['jmdict', 'catalog', 'patternPolicy'], label);
  if (generatedFrom.patternPolicy !== ZH_HANS_SENSE_INFO_PATTERN_POLICY) {
    throw new Error(`${label} has an unsupported pattern policy`);
  }
  return {
    jmdict: parseIdentity(generatedFrom.jmdict, `${label} jmdict`),
    catalog: parseIdentity(generatedFrom.catalog, `${label} catalog`),
    patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY
  };
}

function parseProvenanceGeneratedFrom(
  value: unknown,
  label: string
): ZhHansSenseInfoReviewProvenanceBatch['generatedFrom'] {
  const generatedFrom = record(value, label);
  exactKeys(generatedFrom, ['jmdict', 'catalog', 'patternPolicy'], label);
  if (generatedFrom.patternPolicy !== ZH_HANS_SENSE_INFO_PATTERN_POLICY
    && generatedFrom.patternPolicy !== ZH_HANS_SENSE_INFO_HISTORICAL_PATTERN_POLICY) {
    throw new Error(`${label} has an unsupported historical pattern policy`);
  }
  return {
    jmdict: parseIdentity(generatedFrom.jmdict, `${label} jmdict`),
    catalog: parseIdentity(generatedFrom.catalog, `${label} catalog`),
    patternPolicy: generatedFrom.patternPolicy
  };
}

function parseTranslator(value: unknown, label: string): ZhHansSenseInfoTranslator {
  const translator = record(value, label);
  exactKeys(
    translator,
    ['kind', 'provider', 'model', 'runId', 'generatedAt', 'sourcePolicy'],
    label
  );
  if (translator.kind !== 'codex' || translator.provider !== 'openai') {
    throw new Error(`${label} must identify the Codex/OpenAI translator`);
  }
  if (translator.sourcePolicy !== ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY) {
    throw new Error(`${label} must attest the Codex-only source policy`);
  }
  return {
    kind: 'codex',
    provider: 'openai',
    model: nonemptyText(translator.model, `${label} model`),
    runId: nonemptyText(translator.runId, `${label} runId`),
    generatedAt: canonicalTimestamp(translator.generatedAt, `${label} generatedAt`),
    sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
  };
}

function parseTranslatorAttribution(
  value: unknown,
  label: string
): ZhHansSenseInfoTranslatorAttribution {
  const row = record(value, label);
  if (row.kind !== 'combined') return parseTranslator(value, label);
  exactKeys(row, ['kind', 'runs'], label);
  if (!Array.isArray(row.runs) || row.runs.length === 0) {
    throw new Error(`${label} combined runs must be a non-empty array`);
  }
  let previous = '';
  const runs = row.runs.map((item, index) => {
    const run = parseTranslator(item, `${label} run ${index}`);
    if (run.runId <= previous) throw new Error(`${label} runs must be sorted and unique by runId`);
    previous = run.runId;
    return run;
  });
  return { kind: 'combined', runs };
}

function parseOrigin(value: unknown, label: string): ZhHansSenseInfoArtifactOrigin {
  const origin = record(value, label);
  if (origin.kind === 'native') {
    exactKeys(origin, ['kind'], label);
    return { kind: 'native' };
  }
  if (origin.kind === 'adapted') {
    exactKeys(origin, ['kind', 'sourceKind', 'sha256'], label);
    return {
      kind: 'adapted',
      sourceKind: nonemptyText(origin.sourceKind, `${label} sourceKind`),
      sha256: sha256Text(origin.sha256, `${label} sha256`)
    };
  }
  if (origin.kind === 'combined') {
    exactKeys(origin, ['kind', 'batches'], label);
    if (!Array.isArray(origin.batches) || origin.batches.length < 2) {
      throw new Error(`${label} combined batches must contain at least two inputs`);
    }
    let previous = '';
    const batches = origin.batches.map((item, index) => {
      const batch = record(item, `${label} batch ${index}`);
      exactKeys(batch, ['candidateSha256', 'reviewSha256'], `${label} batch ${index}`);
      const candidateSha256 = sha256Text(
        batch.candidateSha256,
        `${label} batch ${index} candidateSha256`
      );
      if (candidateSha256 <= previous) {
        throw new Error(`${label} batches must be sorted and unique by candidate digest`);
      }
      previous = candidateSha256;
      return {
        candidateSha256,
        reviewSha256: sha256Text(batch.reviewSha256, `${label} batch ${index} reviewSha256`)
      };
    });
    return { kind: 'combined', batches };
  }
  throw new Error(`${label} has an invalid kind`);
}

function parseUncertainty(value: unknown, label: string): ZhHansSenseInfoUncertainty {
  const uncertainty = record(value, label);
  exactKeys(uncertainty, ['level', 'rationale'], label);
  if (!['none', 'low', 'medium', 'high'].includes(String(uncertainty.level))) {
    throw new Error(`${label} has an invalid level`);
  }
  return {
    level: uncertainty.level as ZhHansSenseInfoUncertainty['level'],
    rationale: nonemptyText(uncertainty.rationale, `${label} rationale`)
  };
}

function contextOrder(
  left: ZhHansSenseInfoCandidateContext,
  right: ZhHansSenseInfoCandidateContext
): number {
  return left.seq - right.seq
    || left.sense - right.sense
    || left.info - right.info
    || compareText(left.contextSha256, right.contextSha256);
}

function parseContexts(
  value: unknown,
  label: string
): readonly ZhHansSenseInfoCandidateContext[] {
  if (!Array.isArray(value) || value.length === 0) {
    throw new Error(`${label} must be a non-empty array`);
  }
  let previous: ZhHansSenseInfoCandidateContext | undefined;
  return value.map((item, index) => {
    const context = record(item, `${label} ${index}`);
    exactKeys(context, ['seq', 'sense', 'info', 'contextSha256'], `${label} ${index}`);
    const parsed: ZhHansSenseInfoCandidateContext = {
      seq: positiveInteger(context.seq, `${label} ${index} seq`),
      sense: positiveInteger(context.sense, `${label} ${index} sense`),
      info: positiveInteger(context.info, `${label} ${index} info`),
      contextSha256: sha256Text(context.contextSha256, `${label} ${index} contextSha256`)
    };
    if (previous && contextOrder(previous, parsed) >= 0) {
      throw new Error(`${label} must be unique and sorted by seq, sense, info, and digest`);
    }
    previous = parsed;
    return parsed;
  });
}

function parseCandidate(value: unknown, index: number): ZhHansSenseInfoCandidate {
  const label = `zh-Hans sense-info candidate ${index}`;
  const candidate = record(value, label);
  if (candidate.catalogAction === 'add') {
    exactKeys(candidate, ['source', 'target', 'catalogAction', 'uncertainty', 'contexts'], label);
    return {
      source: nonemptyText(candidate.source, `${label} source`),
      target: nonemptyText(candidate.target, `${label} target`),
      catalogAction: 'add',
      uncertainty: parseUncertainty(candidate.uncertainty, `${label} uncertainty`),
      contexts: parseContexts(candidate.contexts, `${label} contexts`)
    };
  }
  if (candidate.catalogAction === 'revise') {
    exactKeys(
      candidate,
      ['source', 'target', 'catalogAction', 'priorTarget', 'uncertainty', 'contexts'],
      label
    );
    return {
      source: nonemptyText(candidate.source, `${label} source`),
      target: nonemptyText(candidate.target, `${label} target`),
      catalogAction: 'revise',
      priorTarget: nonemptyText(candidate.priorTarget, `${label} priorTarget`),
      uncertainty: parseUncertainty(candidate.uncertainty, `${label} uncertainty`),
      contexts: parseContexts(candidate.contexts, `${label} contexts`)
    };
  }
  throw new Error(`${label} has an invalid catalogAction`);
}

export function parseZhHansSenseInfoCandidateArtifact(
  value: unknown
): ZhHansSenseInfoCandidateArtifact {
  const artifact = record(value, 'zh-Hans sense-info candidate artifact');
  exactKeys(
    artifact,
    [
      'formatVersion', 'kind', 'locale', 'sourceLocale', 'generatedFrom', 'origin',
      'translator', 'candidates'
    ],
    'zh-Hans sense-info candidate artifact'
  );
  if (artifact.formatVersion !== 1
    || artifact.kind !== 'zh-hans-sense-info-codex-candidates') {
    throw new Error('Unsupported zh-Hans sense-info candidate artifact format');
  }
  if (artifact.locale !== 'zh-Hans' || artifact.sourceLocale !== 'en') {
    throw new Error('Candidate artifact must translate en to zh-Hans');
  }
  if (!Array.isArray(artifact.candidates) || artifact.candidates.length === 0) {
    throw new Error('Candidate artifact candidates must be a non-empty array');
  }
  let previousSource = '';
  const candidates = artifact.candidates.map((item, index) => {
    const candidate = parseCandidate(item, index);
    if (candidate.source <= previousSource) {
      throw new Error('Candidate artifact candidates must be unique and sorted by source');
    }
    previousSource = candidate.source;
    return candidate;
  });
  return {
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: parseGeneratedFrom(artifact.generatedFrom, 'candidate generatedFrom'),
    origin: parseOrigin(artifact.origin, 'candidate origin'),
    translator: parseTranslatorAttribution(artifact.translator, 'candidate translator'),
    candidates
  };
}

function parseReviewer(value: unknown, label: string): ZhHansSenseInfoReviewer {
  const reviewer = record(value, label);
  if (reviewer.kind === 'human') {
    exactKeys(reviewer, ['kind', 'id', 'displayName', 'reviewedAt'], label);
    return {
      kind: 'human',
      id: nonemptyText(reviewer.id, `${label} id`),
      displayName: nonemptyText(reviewer.displayName, `${label} displayName`),
      reviewedAt: canonicalTimestamp(reviewer.reviewedAt, `${label} reviewedAt`)
    };
  }
  if (reviewer.kind === 'codex') {
    exactKeys(
      reviewer,
      ['kind', 'provider', 'model', 'runId', 'reviewedAt', 'sourcePolicy'],
      label
    );
    if (reviewer.provider !== 'openai') {
      throw new Error(`${label} Codex reviewer provider must be OpenAI`);
    }
    if (reviewer.sourcePolicy !== ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY) {
      throw new Error(`${label} must attest the Codex-only source policy`);
    }
    return {
      kind: 'codex',
      provider: 'openai',
      model: nonemptyText(reviewer.model, `${label} model`),
      runId: nonemptyText(reviewer.runId, `${label} runId`),
      reviewedAt: canonicalTimestamp(reviewer.reviewedAt, `${label} reviewedAt`),
      sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
    };
  }
  if (reviewer.kind === 'combined') {
    exactKeys(reviewer, ['kind', 'reviewers'], label);
    if (!Array.isArray(reviewer.reviewers) || reviewer.reviewers.length === 0) {
      throw new Error(`${label} combined reviewers must contain at least one contributor`);
    }
    let previous = '';
    const reviewers = reviewer.reviewers.map((item, index) => {
      const parsed = parseReviewer(item, `${label} contributor ${index}`);
      if (parsed.kind === 'combined') throw new Error(`${label} cannot nest combined reviewers`);
      const key = parsed.kind === 'codex' ? `codex:${parsed.runId}` : `human:${parsed.id}`;
      if (key <= previous) throw new Error(`${label} contributors must be sorted and unique`);
      previous = key;
      return parsed;
    });
    return { kind: 'combined', reviewers };
  }
  throw new Error(`${label} must identify a human or Codex/OpenAI reviewer`);
}

function parseDecision(value: unknown, index: number): ZhHansSenseInfoReviewerDecision {
  const label = `zh-Hans sense-info review decision ${index}`;
  const decision = record(value, label);
  if (decision.decision === 'revise') {
    exactKeys(decision, ['source', 'decision', 'target', 'rationale'], label);
    return {
      source: nonemptyText(decision.source, `${label} source`),
      decision: 'revise',
      target: nonemptyText(decision.target, `${label} target`),
      rationale: nonemptyText(decision.rationale, `${label} rationale`)
    };
  }
  if (decision.decision === 'approve' || decision.decision === 'reject') {
    exactKeys(decision, ['source', 'decision', 'rationale'], label);
    return {
      source: nonemptyText(decision.source, `${label} source`),
      decision: decision.decision,
      rationale: nonemptyText(decision.rationale, `${label} rationale`)
    };
  }
  throw new Error(`${label} has an invalid decision`);
}

export function parseZhHansSenseInfoReviewArtifact(
  value: unknown
): ZhHansSenseInfoReviewArtifact {
  const artifact = record(value, 'zh-Hans sense-info review artifact');
  exactKeys(
    artifact,
    [
      'formatVersion', 'kind', 'locale', 'sourceLocale', 'candidateSha256', 'origin',
      'reviewer', 'decisions'
    ],
    'zh-Hans sense-info review artifact'
  );
  if (artifact.formatVersion !== 1
    || artifact.kind !== 'zh-hans-sense-info-review-decisions') {
    throw new Error('Unsupported zh-Hans sense-info review artifact format');
  }
  if (artifact.locale !== 'zh-Hans' || artifact.sourceLocale !== 'en') {
    throw new Error('Review artifact must review en to zh-Hans candidates');
  }
  if (!Array.isArray(artifact.decisions) || artifact.decisions.length === 0) {
    throw new Error('Review artifact decisions must be a non-empty array');
  }
  let previousSource = '';
  const decisions = artifact.decisions.map((item, index) => {
    const decision = parseDecision(item, index);
    if (decision.source <= previousSource) {
      throw new Error('Review artifact decisions must be unique and sorted by source');
    }
    previousSource = decision.source;
    return decision;
  });
  return {
    formatVersion: 1,
    kind: 'zh-hans-sense-info-review-decisions',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    candidateSha256: sha256Text(artifact.candidateSha256, 'review candidateSha256'),
    origin: parseOrigin(artifact.origin, 'review origin'),
    reviewer: parseReviewer(artifact.reviewer, 'reviewer'),
    decisions
  };
}

function parseNullableText(value: unknown, label: string): string | null {
  return value === null ? null : nonemptyText(value, label);
}

interface LegacyZhHansSenseInfoReviewProvenanceRecord {
  readonly source: string;
  readonly candidateSha256: string;
  readonly candidateTarget: string;
  readonly catalogAction: 'add' | 'revise';
  readonly priorTarget: string | null;
  readonly decision: 'approve' | 'revise' | 'reject';
  readonly finalTarget: string | null;
  readonly generatedFrom: ZhHansSenseInfoReviewProvenanceBatch['generatedFrom'];
  readonly candidateOrigin: ZhHansSenseInfoArtifactOrigin;
  readonly reviewOrigin: ZhHansSenseInfoArtifactOrigin;
  readonly translator: ZhHansSenseInfoTranslatorAttribution;
  readonly uncertainty: ZhHansSenseInfoUncertainty;
  readonly contexts: readonly ZhHansSenseInfoCandidateContext[];
  readonly reviewer: ZhHansSenseInfoReviewer;
  readonly rationale: string;
}

function parseLegacyProvenanceRecord(
  value: unknown,
  index: number
): LegacyZhHansSenseInfoReviewProvenanceRecord {
  const label = `zh-Hans sense-info provenance record ${index}`;
  const item = record(value, label);
  exactKeys(item, [
    'source', 'candidateSha256', 'candidateTarget', 'catalogAction', 'priorTarget',
    'decision', 'finalTarget', 'generatedFrom', 'candidateOrigin', 'reviewOrigin',
    'translator', 'uncertainty', 'contexts', 'reviewer', 'rationale'
  ], label);
  if (item.catalogAction !== 'add' && item.catalogAction !== 'revise') {
    throw new Error(`${label} has an invalid catalogAction`);
  }
  if (!['approve', 'revise', 'reject'].includes(String(item.decision))) {
    throw new Error(`${label} has an invalid decision`);
  }
  const decision = item.decision as LegacyZhHansSenseInfoReviewProvenanceRecord['decision'];
  const priorTarget = parseNullableText(item.priorTarget, `${label} priorTarget`);
  const finalTarget = parseNullableText(item.finalTarget, `${label} finalTarget`);
  if ((item.catalogAction === 'add') !== (priorTarget === null)) {
    throw new Error(`${label} priorTarget must be null only for an add action`);
  }
  if ((decision === 'reject') !== (finalTarget === null)) {
    throw new Error(`${label} finalTarget must be null only for a rejection`);
  }
  return {
    source: nonemptyText(item.source, `${label} source`),
    candidateSha256: sha256Text(item.candidateSha256, `${label} candidateSha256`),
    candidateTarget: nonemptyText(item.candidateTarget, `${label} candidateTarget`),
    catalogAction: item.catalogAction,
    priorTarget,
    decision,
    finalTarget,
    generatedFrom: parseProvenanceGeneratedFrom(item.generatedFrom, `${label} generatedFrom`),
    candidateOrigin: parseOrigin(item.candidateOrigin, `${label} candidateOrigin`),
    reviewOrigin: parseOrigin(item.reviewOrigin, `${label} reviewOrigin`),
    translator: parseTranslatorAttribution(item.translator, `${label} translator`),
    uncertainty: parseUncertainty(item.uncertainty, `${label} uncertainty`),
    contexts: parseContexts(item.contexts, `${label} contexts`),
    reviewer: parseReviewer(item.reviewer, `${label} reviewer`),
    rationale: nonemptyText(item.rationale, `${label} rationale`)
  };
}

function legacyProvenanceOrder(
  left: LegacyZhHansSenseInfoReviewProvenanceRecord,
  right: LegacyZhHansSenseInfoReviewProvenanceRecord
): number {
  return compareText(left.candidateSha256, right.candidateSha256)
    || compareText(left.source, right.source);
}

function parseProvenanceBatch(
  value: unknown,
  candidateSha256: string
): ZhHansSenseInfoReviewProvenanceBatch {
  const label = `zh-Hans sense-info provenance batch ${candidateSha256}`;
  const batch = record(value, label);
  exactKeys(batch, [
    'generatedFrom', 'candidateOrigin', 'reviewOrigin', 'translator', 'reviewer'
  ], label);
  return {
    generatedFrom: parseProvenanceGeneratedFrom(batch.generatedFrom, `${label} generatedFrom`),
    candidateOrigin: parseOrigin(batch.candidateOrigin, `${label} candidateOrigin`),
    reviewOrigin: parseOrigin(batch.reviewOrigin, `${label} reviewOrigin`),
    translator: parseTranslatorAttribution(batch.translator, `${label} translator`),
    reviewer: parseReviewer(batch.reviewer, `${label} reviewer`)
  };
}

function parseProvenanceDecision(
  value: unknown,
  index: number
): ZhHansSenseInfoReviewProvenanceDecision {
  const label = `zh-Hans sense-info provenance decision ${index}`;
  const item = record(value, label);
  exactKeys(item, [
    'source', 'batchSha256', 'candidateTarget', 'catalogAction', 'priorTarget',
    'decision', 'finalTarget', 'uncertainty', 'rationale'
  ], label);
  if (item.catalogAction !== 'add' && item.catalogAction !== 'revise') {
    throw new Error(`${label} has an invalid catalogAction`);
  }
  if (!['approve', 'revise', 'reject'].includes(String(item.decision))) {
    throw new Error(`${label} has an invalid decision`);
  }
  const decision = item.decision as ZhHansSenseInfoReviewProvenanceDecision['decision'];
  const priorTarget = parseNullableText(item.priorTarget, `${label} priorTarget`);
  const finalTarget = parseNullableText(item.finalTarget, `${label} finalTarget`);
  if ((item.catalogAction === 'add') !== (priorTarget === null)) {
    throw new Error(`${label} priorTarget must be null only for an add action`);
  }
  if ((decision === 'reject') !== (finalTarget === null)) {
    throw new Error(`${label} finalTarget must be null only for a rejection`);
  }
  return {
    source: nonemptyText(item.source, `${label} source`),
    batchSha256: sha256Text(item.batchSha256, `${label} batchSha256`),
    candidateTarget: nonemptyText(item.candidateTarget, `${label} candidateTarget`),
    catalogAction: item.catalogAction,
    priorTarget,
    decision,
    finalTarget,
    uncertainty: parseUncertainty(item.uncertainty, `${label} uncertainty`),
    rationale: nonemptyText(item.rationale, `${label} rationale`)
  };
}

function provenanceDecisionOrder(
  left: ZhHansSenseInfoReviewProvenanceDecision,
  right: ZhHansSenseInfoReviewProvenanceDecision
): number {
  return compareText(left.batchSha256, right.batchSha256)
    || compareText(left.source, right.source);
}

function sameProvenanceBatch(
  left: ZhHansSenseInfoReviewProvenanceBatch,
  right: ZhHansSenseInfoReviewProvenanceBatch
): boolean {
  return JSON.stringify(left) === JSON.stringify(right);
}

function normalizedProvenance(
  batches: ReadonlyMap<string, ZhHansSenseInfoReviewProvenanceBatch>,
  decisions: readonly ZhHansSenseInfoReviewProvenanceDecision[]
): ZhHansSenseInfoReviewProvenance {
  const sortedBatches = Object.fromEntries(
    [...batches].sort(([left], [right]) => compareText(left, right))
  );
  return {
    formatVersion: 2,
    kind: 'zh-hans-sense-info-review-provenance',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    batches: sortedBatches,
    decisions: [...decisions].sort(provenanceDecisionOrder)
  };
}

function parseLegacyProvenance(
  artifact: Record<string, unknown>
): ZhHansSenseInfoReviewProvenance {
  exactKeys(
    artifact,
    ['formatVersion', 'kind', 'locale', 'sourceLocale', 'records'],
    'zh-Hans sense-info review provenance'
  );
  if (!Array.isArray(artifact.records)) {
    throw new Error('Review provenance records must be an array');
  }
  let previous: LegacyZhHansSenseInfoReviewProvenanceRecord | undefined;
  const batches = new Map<string, ZhHansSenseInfoReviewProvenanceBatch>();
  const decisions: ZhHansSenseInfoReviewProvenanceDecision[] = [];
  for (let index = 0; index < artifact.records.length; index++) {
    const parsed = parseLegacyProvenanceRecord(artifact.records[index], index);
    if (previous && legacyProvenanceOrder(previous, parsed) >= 0) {
      throw new Error('Review provenance records must be unique and sorted by digest and source');
    }
    previous = parsed;
    const batch: ZhHansSenseInfoReviewProvenanceBatch = {
      generatedFrom: parsed.generatedFrom,
      candidateOrigin: parsed.candidateOrigin,
      reviewOrigin: parsed.reviewOrigin,
      translator: parsed.translator,
      reviewer: parsed.reviewer
    };
    const existingBatch = batches.get(parsed.candidateSha256);
    if (existingBatch && !sameProvenanceBatch(existingBatch, batch)) {
      throw new Error(`Legacy provenance repeats a candidate digest with different batch metadata: ${parsed.candidateSha256}`);
    }
    batches.set(parsed.candidateSha256, batch);
    decisions.push({
      source: parsed.source,
      batchSha256: parsed.candidateSha256,
      candidateTarget: parsed.candidateTarget,
      catalogAction: parsed.catalogAction,
      priorTarget: parsed.priorTarget,
      decision: parsed.decision,
      finalTarget: parsed.finalTarget,
      uncertainty: parsed.uncertainty,
      rationale: parsed.rationale
    });
  }
  return normalizedProvenance(batches, decisions);
}

function parseCurrentProvenance(
  artifact: Record<string, unknown>
): ZhHansSenseInfoReviewProvenance {
  exactKeys(
    artifact,
    ['formatVersion', 'kind', 'locale', 'sourceLocale', 'batches', 'decisions'],
    'zh-Hans sense-info review provenance'
  );
  const batchRows = record(artifact.batches, 'zh-Hans sense-info provenance batches');
  const batchKeys = Object.keys(batchRows);
  for (let index = 0; index < batchKeys.length; index++) {
    sha256Text(batchKeys[index], `zh-Hans sense-info provenance batch key ${index}`);
    if (index > 0 && batchKeys[index - 1]! >= batchKeys[index]!) {
      throw new Error('Review provenance batch keys must be unique and sorted by digest');
    }
  }
  const batches = new Map(batchKeys.map(key => [key, parseProvenanceBatch(batchRows[key], key)]));
  if (!Array.isArray(artifact.decisions)) {
    throw new Error('Review provenance decisions must be an array');
  }
  let previous: ZhHansSenseInfoReviewProvenanceDecision | undefined;
  const usedBatches = new Set<string>();
  const decisions = artifact.decisions.map((item, index) => {
    const parsed = parseProvenanceDecision(item, index);
    if (previous && provenanceDecisionOrder(previous, parsed) >= 0) {
      throw new Error('Review provenance decisions must be unique and sorted by digest and source');
    }
    previous = parsed;
    if (!batches.has(parsed.batchSha256)) {
      throw new Error(`Review provenance decision references an unknown batch: ${parsed.source}`);
    }
    usedBatches.add(parsed.batchSha256);
    return parsed;
  });
  if (usedBatches.size !== batches.size) {
    throw new Error('Review provenance contains batch metadata with no decisions');
  }
  return normalizedProvenance(batches, decisions);
}

export function parseZhHansSenseInfoReviewProvenance(
  value: unknown
): ZhHansSenseInfoReviewProvenance {
  const artifact = record(value, 'zh-Hans sense-info review provenance');
  if (artifact.kind !== 'zh-hans-sense-info-review-provenance') {
    throw new Error('Unsupported zh-Hans sense-info review provenance format');
  }
  if (artifact.locale !== 'zh-Hans' || artifact.sourceLocale !== 'en') {
    throw new Error('Review provenance must describe en to zh-Hans decisions');
  }
  if (artifact.formatVersion === 1) return parseLegacyProvenance(artifact);
  if (artifact.formatVersion === 2) return parseCurrentProvenance(artifact);
  throw new Error('Unsupported zh-Hans sense-info review provenance format');
}

export function emptyZhHansSenseInfoReviewProvenance(): ZhHansSenseInfoReviewProvenance {
  return normalizedProvenance(new Map(), []);
}

function contextDigest(value: {
  readonly source: string;
  readonly seq: number;
  readonly sense: number;
  readonly info: number;
  readonly headwords: readonly string[];
  readonly englishGlosses: readonly string[];
}): string {
  return createHash('sha256').update(JSON.stringify(value)).digest('hex');
}

/**
 * Bind translation candidates to every current occurrence and its relevant
 * JMdict context. Any upstream wording, location, headword, or gloss change
 * produces a different reference and forces re-review.
 */
export function buildZhHansSenseInfoContextIndex(
  entries: readonly CanonicalEntry[]
): ReadonlyMap<string, readonly ZhHansSenseInfoCandidateContext[]> {
  const result = new Map<string, ZhHansSenseInfoCandidateContext[]>();
  for (const entry of entries) {
    const headwords = [...new Set([
      ...entry.kanji.map(form => form.text),
      ...entry.kana.map(form => form.text)
    ])];
    for (const sense of entry.senses) {
      for (const property of sense.properties) {
        if (property.tag !== 's_inf') continue;
        const context = {
          source: property.text,
          seq: entry.seq,
          sense: sense.ordinal,
          info: property.ordinal,
          headwords,
          englishGlosses: sense.glosses
        };
        const refs = result.get(property.text) ?? [];
        refs.push({
          seq: entry.seq,
          sense: sense.ordinal,
          info: property.ordinal,
          contextSha256: contextDigest(context)
        });
        result.set(property.text, refs);
      }
    }
  }
  for (const refs of result.values()) refs.sort(contextOrder);
  return result;
}

function identitiesEqual(
  left: ZhHansSenseInfoSourceIdentity,
  right: ZhHansSenseInfoSourceIdentity
): boolean {
  return left.id === right.id && left.sha256 === right.sha256;
}

function contextsEqual(
  left: readonly ZhHansSenseInfoCandidateContext[],
  right: readonly ZhHansSenseInfoCandidateContext[]
): boolean {
  return left.length === right.length && left.every((value, index) => {
    const other = right[index];
    return other !== undefined
      && value.seq === other.seq
      && value.sense === other.sense
      && value.info === other.info
      && value.contextSha256 === other.contextSha256;
  });
}

function assertBalanced(text: string, open: string, close: string): boolean {
  let depth = 0;
  for (const character of text) {
    if (character === open) depth++;
    if (character === close && --depth < 0) return false;
  }
  return depth === 0;
}

/**
 * Candidate drafts are retained as review provenance, so they need only be
 * safe to parse and display. Semantic target checks belong to the value a
 * reviewer actually promotes.
 */
export function assertStructurallySafeZhHansSenseInfoCandidateTarget(
  target: string,
  source: string
): void {
  if (target.length === 0 || target !== target.trim()) {
    throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: not trimmed text`);
  }
  if (CONTROL_OR_REPLACEMENT.test(target) || /\b(?:TODO|FIXME)\b/.test(target)) {
    throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: invalid text`);
  }
  for (const [open, close] of [['「', '」'], ['『', '』'], ['（', '）'], ['(', ')']]) {
    if (!assertBalanced(target, open!, close!)) {
      throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: unbalanced delimiters`);
    }
  }
}

export function assertPlausibleZhHansSenseInfoTarget(target: string, source: string): void {
  assertStructurallySafeZhHansSenseInfoCandidateTarget(target, source);
  if (target === source) {
    throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: equals source`);
  }
  const suspiciousAscii = [...target.matchAll(ASCII_WORD)].map(match => match[0]).find(token =>
    /[a-z]/.test(token)
    && !new RegExp(
      `(^|[^A-Za-z])${token.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}([^A-Za-z]|$)`
    ).test(source));
  if (suspiciousAscii !== undefined) {
    throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: lowercase ASCII prose`);
  }
  if (!HAN.test(target)) {
    throw new Error(`Suspicious zh-Hans target for ${JSON.stringify(source)}: lacks Han text`);
  }
}

/** Reject known legacy Apple/external-MT draft paths before the CLI reads bytes. */
export function assertCodexCandidateArtifactPath(path: string): void {
  const normalized = path.replaceAll('\\', '/');
  const basename = normalized.slice(normalized.lastIndexOf('/') + 1);
  if (FORBIDDEN_CANDIDATE_BASENAME.test(basename)
    || /(?:^|[._-])apple(?:[._-]|$)/i.test(basename)
    || /external[._-]?mt/i.test(basename)) {
    throw new Error(`Refusing non-Codex translation draft path: ${path}`);
  }
}

function defaultProvenance(
  value: ZhHansSenseInfoReviewProvenance | undefined
): ZhHansSenseInfoReviewProvenance {
  return value ?? emptyZhHansSenseInfoReviewProvenance();
}

function assertReviewCoverage(
  candidates: readonly ZhHansSenseInfoCandidate[],
  decisions: readonly ZhHansSenseInfoReviewerDecision[]
): void {
  if (candidates.length !== decisions.length) {
    throw new Error('Review decisions must cover every candidate exactly once');
  }
  for (let index = 0; index < candidates.length; index++) {
    if (candidates[index]!.source !== decisions[index]!.source) {
      throw new Error('Review decisions must cover every candidate exactly once');
    }
  }
}

export function mergeZhHansSenseInfoReviews(
  options: MergeZhHansSenseInfoReviewOptions
): MergeZhHansSenseInfoReviewResult {
  const candidate = options.candidateArtifact;
  const review = options.reviewArtifact;
  const provenance = defaultProvenance(options.provenance);

  if (!SHA256.test(options.candidateSha256)) {
    throw new Error('Actual candidate digest must be a lowercase SHA-256 digest');
  }
  if (review.candidateSha256 !== options.candidateSha256) {
    throw new Error('Review artifact is bound to a different candidate artifact digest');
  }
  if (!identitiesEqual(candidate.generatedFrom.jmdict, options.jmdictIdentity)) {
    throw new Error('Candidate artifact has stale JMdict identity');
  }
  if (!identitiesEqual(candidate.generatedFrom.catalog, options.catalogIdentity)) {
    throw new Error('Candidate artifact has stale catalog identity');
  }
  if (candidate.generatedFrom.patternPolicy !== ZH_HANS_SENSE_INFO_PATTERN_POLICY) {
    throw new Error('Candidate artifact has stale pattern policy');
  }
  if (candidate.translator.kind !== 'combined' && review.reviewer.kind !== 'combined') {
    if (Date.parse(review.reviewer.reviewedAt) < Date.parse(candidate.translator.generatedAt)) {
      throw new Error('Review predates candidate generation');
    }
  }
  const translatorRunIds = new Set(candidate.translator.kind === 'combined'
    ? candidate.translator.runs.map(run => run.runId)
    : [candidate.translator.runId]);
  const reviewerRunIds = review.reviewer.kind === 'combined'
    ? review.reviewer.reviewers.flatMap(item => item.kind === 'codex' ? [item.runId] : [])
    : review.reviewer.kind === 'codex' ? [review.reviewer.runId] : [];
  if (reviewerRunIds.some(runId => translatorRunIds.has(runId))) {
    throw new Error('Codex translator and reviewer must use distinct run IDs');
  }
  assertReviewCoverage(candidate.candidates, review.decisions);

  const contexts = buildZhHansSenseInfoContextIndex(options.entries);
  const catalog = new Map(options.catalog.translations.map(item => [item.source, item.target]));
  const existingRecords = new Set(
    provenance.decisions.map(item => `${item.batchSha256}\u0000${item.source}`)
  );
  const batches = new Map(Object.entries(provenance.batches));
  const decisions: ZhHansSenseInfoReviewProvenanceDecision[] = [...provenance.decisions];
  const incomingBatch: ZhHansSenseInfoReviewProvenanceBatch = {
    generatedFrom: candidate.generatedFrom,
    candidateOrigin: candidate.origin,
    reviewOrigin: review.origin,
    translator: candidate.translator,
    reviewer: review.reviewer
  };
  const existingBatch = batches.get(options.candidateSha256);
  if (existingBatch && !sameProvenanceBatch(existingBatch, incomingBatch)) {
    throw new Error('Candidate digest is already associated with different review batch metadata');
  }
  batches.set(options.candidateSha256, incomingBatch);
  const stats = {
    candidates: candidate.candidates.length,
    approved: 0,
    revised: 0,
    rejected: 0,
    catalogAdded: 0,
    catalogRevised: 0
  };

  for (let index = 0; index < candidate.candidates.length; index++) {
    const item = candidate.candidates[index]!;
    const decision = review.decisions[index]!;
    const currentContexts = contexts.get(item.source);
    if (!currentContexts) {
      throw new Error(`Candidate source is missing from the current corpus: ${item.source}`);
    }
    if (!contextsEqual(item.contexts, currentContexts)) {
      throw new Error(`Candidate context is stale or incomplete: ${item.source}`);
    }
    if (translateZhHansSenseInfoPattern(item.source) !== null) {
      throw new Error(`Candidate source is already resolved by a direct rule: ${item.source}`);
    }
    assertStructurallySafeZhHansSenseInfoCandidateTarget(item.target, item.source);

    const existingTarget = catalog.get(item.source);
    if (item.catalogAction === 'add') {
      if (existingTarget !== undefined) {
        throw new Error(`Candidate would overwrite an existing catalog source: ${item.source}`);
      }
    } else {
      if (existingTarget === undefined) {
        throw new Error(`Catalog revision source is missing: ${item.source}`);
      }
      if (existingTarget !== item.priorTarget) {
        throw new Error(`Catalog revision has a stale prior target: ${item.source}`);
      }
    }

    const replayKey = `${options.candidateSha256}\u0000${item.source}`;
    if (existingRecords.has(replayKey)) {
      throw new Error(`Review decision was already ingested: ${item.source}`);
    }
    existingRecords.add(replayKey);

    const finalTarget = decision.decision === 'reject'
      ? null
      : decision.decision === 'revise' ? decision.target : item.target;
    if (finalTarget !== null) {
      assertPlausibleZhHansSenseInfoTarget(finalTarget, item.source);
      if (decision.decision === 'revise' && decision.target === item.target) {
        throw new Error(`Reviewer revision does not change the candidate target: ${item.source}`);
      }
      if (item.catalogAction === 'revise' && finalTarget === item.priorTarget) {
        throw new Error(`Catalog revision is a no-op: ${item.source}`);
      }
      catalog.set(item.source, finalTarget);
      if (item.catalogAction === 'add') stats.catalogAdded++;
      else stats.catalogRevised++;
    }
    if (decision.decision === 'approve') stats.approved++;
    else if (decision.decision === 'revise') stats.revised++;
    else stats.rejected++;

    decisions.push({
      source: item.source,
      batchSha256: options.candidateSha256,
      candidateTarget: item.target,
      catalogAction: item.catalogAction,
      priorTarget: item.catalogAction === 'revise' ? item.priorTarget : null,
      decision: decision.decision,
      finalTarget,
      uncertainty: item.uncertainty,
      rationale: decision.rationale
    });
  }

  const translations: ZhHansSenseInfoTranslation[] = [...catalog].map(([source, target]) => ({
    source,
    target
  })).sort((left, right) => compareText(left.source, right.source));
  return {
    catalog: {
      formatVersion: 1,
      locale: 'zh-Hans',
      sourceLocale: 'en',
      translations
    },
    provenance: normalizedProvenance(batches, decisions),
    stats
  };
}
