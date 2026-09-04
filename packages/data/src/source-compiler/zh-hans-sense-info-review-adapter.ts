import { createHash } from 'node:crypto';

import type { CanonicalEntry } from './model.js';
import {
  buildZhHansSenseInfoContextIndex,
  parseZhHansSenseInfoCandidateArtifact,
  parseZhHansSenseInfoReviewArtifact,
  type ZhHansSenseInfoCandidateArtifact,
  type ZhHansSenseInfoCodexReviewer,
  type ZhHansSenseInfoReviewArtifact,
  type ZhHansSenseInfoReviewerDecision,
  type ZhHansSenseInfoSourceIdentity,
  type ZhHansSenseInfoTranslator,
  type ZhHansSenseInfoUncertainty
} from './zh-hans-sense-info-review.js';
import {
  ZH_HANS_SENSE_INFO_PATTERN_POLICY,
  translateZhHansSenseInfoPattern
} from './zh-hans-sense-info-patterns.js';
import type { ZhHansSenseInfoCatalog } from './zh-hans-sense-info.js';

const RICH_ADD_CANDIDATE_KIND = 'codex-zh-Hans-sense-info-candidates';
const RICH_ADD_REVIEW_KIND = 'zh-Hans-candidate-semantic-review';
const RICH_REVISION_REVIEW_KIND = 'codex-zh-Hans-existing-sense-info-catalog-review';
const RICH_ALTERNATE_CANDIDATE_KIND = 'codex-zh-Hans-register-freeform-candidates';
const RICH_ALTERNATE_REVIEW_KIND = 'codex-zh-Hans-candidate-review';
const RICH_ADD_INPUT_POLICY =
  'Codex translation from canonical English s_inf plus complete JMdict contexts; '
  + 'no Apple Translation, external machine translation, or draft output';
const RICH_ADD_REVIEWER =
  'Codex independent contextual review; no external translation service';
const RICH_REVISION_POLICY =
  'Independent Codex semantic/style review against every current JMdict occurrence; '
  + 'no Apple Translation or external machine translation';
const SHA256 = /^[0-9a-f]{64}$/;

interface RichContext {
  readonly seq: number;
  readonly sense: number;
  readonly info: number;
  readonly headwords: readonly string[];
  readonly englishGlosses: readonly string[];
}

interface RichAddCandidate {
  readonly source: string;
  readonly target: string;
  readonly rationale: string;
  readonly uncertainty: ZhHansSenseInfoUncertainty;
  readonly contexts: readonly RichContext[];
}

interface RichAddCandidateArtifact {
  readonly sourceKind: string;
  readonly generatedFrom: {
    readonly jmdict: ZhHansSenseInfoSourceIdentity;
    readonly catalog: ZhHansSenseInfoSourceIdentity;
  };
  readonly candidates: readonly RichAddCandidate[];
  readonly aggregateClosure: ZhHansSenseInfoAggregateClosure | null;
}

interface RichAddReviewArtifact {
  readonly sourceKind: string;
  readonly candidateSha256: string;
  readonly generatedFrom: RichAddCandidateArtifact['generatedFrom'] | null;
  readonly decisions: readonly (ZhHansSenseInfoReviewerDecision & {
    readonly candidateTarget: string;
    readonly reviewedContextCount?: number;
  })[];
}

interface RichRevisionReview {
  readonly source: string;
  readonly currentTarget: string;
  readonly decision: 'approve' | 'revise';
  readonly proposedTarget: string | null;
  readonly rationale: string;
  readonly confidence: 'high' | 'medium' | 'low';
  readonly contexts: readonly RichContext[];
}

interface RichRevisionReviewArtifact {
  readonly sourceKind: typeof RICH_REVISION_REVIEW_KIND;
  readonly generatedFrom: RichAddCandidateArtifact['generatedFrom'];
  readonly reviews: readonly RichRevisionReview[];
}

export interface ZhHansSenseInfoNonMutatingReview {
  readonly source: string;
  readonly decision: 'approve';
  readonly rationale: string;
}

export interface ZhHansSenseInfoBatchAggregateClosure {
  readonly field: string;
  readonly artifacts: readonly {
    readonly file: string;
    readonly sourceCount: number;
  }[];
  readonly currentUnresolvedSourceCount: number;
  readonly currentUnresolvedOccurrenceCount: number;
  readonly currentUnresolvedClusterCounts: Readonly<Record<string, number>>;
  readonly priorBatchSourceCount: number;
  readonly finalBatchSourceCount: number;
  readonly uniqueCoveredSourceCount: number;
  readonly coveredSourceSha256: string;
  readonly missingSources: readonly string[];
  readonly unexpectedSources: readonly string[];
  readonly remainingUncoveredSourceCount: number;
  readonly complete: boolean;
}

export interface ZhHansSenseInfoFreeformAggregateClosure {
  readonly field: 'aggregateFreeformClosure';
  readonly currentUnresolvedCount: number;
  readonly expectedOriginalUnresolvedCount: number;
  readonly priorRegisterFreeformCount: number;
  readonly priorEtymologyFreeformCount: number;
  readonly finalCount: number;
  readonly uniqueCoveredCount: number;
  readonly aggregateSourceSha256: string;
  readonly remainingCount: number;
  readonly complete: boolean;
  readonly missingSources: readonly string[];
  readonly unexpectedSources: readonly string[];
  readonly crossArtifactOverlaps: readonly string[];
}

/**
 * Stable authoring shape for future cluster-completion batches. Rich source
 * artifacts may use this explicit field instead of minting another ad-hoc
 * top-level closure name. This metadata is receipted but never enters a pack.
 */
export interface ZhHansSenseInfoClusterAggregateClosure {
  readonly field: 'aggregateClusterClosure';
  readonly cluster: string;
  readonly currentUnresolvedSourceCount: number;
  readonly expectedOriginalUnresolvedSourceCount: number;
  readonly priorArtifacts: readonly {
    readonly file: string;
    readonly sourceCount: number;
  }[];
  readonly finalArtifact: {
    readonly file: string;
    readonly sourceCount: number;
  };
  readonly uniqueCoveredSourceCount: number;
  readonly coveredSourceSha256: string;
  readonly remainingUncoveredSourceCount: number;
  readonly complete: boolean;
  readonly missingSources: readonly string[];
  readonly unexpectedSources: readonly string[];
  readonly crossArtifactOverlaps: readonly string[];
}

export type ZhHansSenseInfoAggregateClosure =
  | ZhHansSenseInfoBatchAggregateClosure
  | ZhHansSenseInfoFreeformAggregateClosure
  | ZhHansSenseInfoClusterAggregateClosure;

export interface ZhHansSenseInfoExcludedReview {
  readonly source: string;
  readonly decision: 'approve' | 'revise' | 'reject';
  readonly rationale: string;
  readonly reason: 'direct-rule-resolved';
}

export interface ZhHansSenseInfoAdaptationReceipt {
  readonly formatVersion: 1;
  readonly kind: 'zh-hans-sense-info-adaptation-receipt';
  readonly mode: 'add' | 'revisions';
  readonly inputs: {
    readonly candidates: {
      readonly sourceKind: string;
      readonly sha256: string;
    } | null;
    readonly review: {
      readonly sourceKind: string;
      readonly sha256: string;
    };
  };
  readonly outputs: {
    readonly candidateSha256: string;
    readonly reviewSha256: string;
  };
  readonly sourceDecisionCounts: {
    readonly approve: number;
    readonly revise: number;
    readonly reject: number;
  };
  readonly emittedCandidateCount: number;
  readonly aggregateClosures: readonly ZhHansSenseInfoAggregateClosure[];
  readonly nonMutatingDecisions: readonly ZhHansSenseInfoNonMutatingReview[];
  readonly excludedDecisions: readonly ZhHansSenseInfoExcludedReview[];
}

export interface AdaptedZhHansSenseInfoReview {
  readonly candidateArtifact: ZhHansSenseInfoCandidateArtifact;
  readonly candidateBytes: Uint8Array;
  readonly reviewArtifact: ZhHansSenseInfoReviewArtifact;
  readonly reviewBytes: Uint8Array;
  readonly receipt: ZhHansSenseInfoAdaptationReceipt;
}

export interface ZhHansSenseInfoAdapterMetadata {
  readonly translator: ZhHansSenseInfoTranslator;
  readonly reviewer: ZhHansSenseInfoCodexReviewer;
}

function object(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function exactKeys(value: Record<string, unknown>, keys: readonly string[], label: string): void {
  const actual = Object.keys(value);
  const missing = keys.filter(key => !actual.includes(key));
  const unknown = actual.filter(key => !keys.includes(key));
  if (missing.length > 0) throw new Error(`${label} is missing fields: ${missing.join(', ')}`);
  if (unknown.length > 0) throw new Error(`${label} has unknown fields: ${unknown.join(', ')}`);
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0 || value !== value.trim()) {
    throw new Error(`${label} must be trimmed, non-empty text`);
  }
  return value;
}

function digest(value: unknown, label: string): string {
  const result = text(value, label);
  if (!SHA256.test(result)) throw new Error(`${label} must be a lowercase SHA-256 digest`);
  return result;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
}

function boolean(value: unknown, label: string): boolean {
  if (typeof value !== 'boolean') throw new Error(`${label} must be boolean`);
  return value;
}

function stringArray(value: unknown, label: string): readonly string[] {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return value.map((item, index) => text(item, `${label} ${index}`));
}

function sourceIdentity(value: unknown, label: string): ZhHansSenseInfoSourceIdentity {
  const row = object(value, label);
  exactKeys(row, ['id', 'sha256'], label);
  return { id: text(row.id, `${label} id`), sha256: digest(row.sha256, `${label} sha256`) };
}

function generatedFrom(
  value: unknown,
  label: string,
  withRules: boolean
): RichAddCandidateArtifact['generatedFrom'] {
  const row = object(value, label);
  exactKeys(row, withRules
    ? ['jmdict', 'catalog', 'deterministicRules']
    : ['jmdict', 'catalog'], label);
  if (withRules) {
    const rules = object(row.deterministicRules, `${label} deterministicRules`);
    exactKeys(rules, ['builtInPolicy', 'additionalOutput'], `${label} deterministicRules`);
    if (rules.builtInPolicy !== ZH_HANS_SENSE_INFO_PATTERN_POLICY
      || rules.additionalOutput !== null) {
      throw new Error(`${label} must use only the current built-in deterministic policy`);
    }
  }
  return {
    jmdict: sourceIdentity(row.jmdict, `${label} jmdict`),
    catalog: sourceIdentity(row.catalog, `${label} catalog`)
  };
}

function richContext(value: unknown, label: string): RichContext {
  const row = object(value, label);
  exactKeys(row, ['seq', 'sense', 'info', 'headwords', 'englishGlosses'], label);
  return {
    seq: integer(row.seq, `${label} seq`),
    sense: integer(row.sense, `${label} sense`),
    info: integer(row.info, `${label} info`),
    headwords: stringArray(row.headwords, `${label} headwords`),
    englishGlosses: stringArray(row.englishGlosses, `${label} englishGlosses`)
  };
}

function richContexts(value: unknown, label: string): readonly RichContext[] {
  if (!Array.isArray(value) || value.length === 0) {
    throw new Error(`${label} must be a non-empty array`);
  }
  return value.map((item, index) => richContext(item, `${label} ${index}`));
}

function uncertainty(value: unknown, label: string): ZhHansSenseInfoUncertainty {
  const row = object(value, label);
  exactKeys(row, ['level', 'needsIndependentSemanticReview', 'note'], label);
  if (!['low', 'medium', 'high'].includes(String(row.level))) {
    throw new Error(`${label} has an invalid level`);
  }
  if (typeof row.needsIndependentSemanticReview !== 'boolean') {
    throw new Error(`${label} needsIndependentSemanticReview must be boolean`);
  }
  return {
    level: row.level as ZhHansSenseInfoUncertainty['level'],
    rationale: text(row.note, `${label} note`)
  };
}

function assertCommonEnvelope(
  row: Record<string, unknown>,
  expectedKind: string,
  expectedStatus: string,
  label: string
): void {
  if (row.formatVersion !== 1 || row.kind !== expectedKind) {
    throw new Error(`${label} has an unsupported format or kind`);
  }
  if (row.locale !== 'zh-Hans' || row.sourceLocale !== 'en' || row.status !== expectedStatus) {
    throw new Error(`${label} has an unsupported locale or status`);
  }
}

const AGGREGATE_CLOSURE_BATCH_COUNTS: Readonly<Record<string, number>> = {
  Two: 2,
  Three: 3,
  Four: 4,
  Five: 5,
  Six: 6,
  Seven: 7,
  Eight: 8,
  Nine: 9,
  Ten: 10
};

function aggregateClosureKey(row: Record<string, unknown>, baseKeys: readonly string[]): string | null {
  const extras = Object.keys(row).filter(key => !baseKeys.includes(key));
  if (extras.length === 0) return null;
  if (extras.length !== 1 || (!/^aggregate(?:Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten)BatchClosure$/
    .test(extras[0]!) && extras[0] !== 'aggregateFreeformClosure'
      && extras[0] !== 'aggregateClusterClosure')) {
    throw new Error(`Rich candidate artifact has unknown fields: ${extras.join(', ')}`);
  }
  return extras[0]!;
}

function artifactReference(
  value: unknown,
  label: string
): { readonly file: string; readonly sourceCount: number } {
  const row = object(value, label);
  exactKeys(row, ['file', 'sourceCount'], label);
  return {
    file: text(row.file, `${label} file`),
    sourceCount: integer(row.sourceCount, `${label} sourceCount`)
  };
}

function assertDistinctArtifacts(
  artifacts: readonly { readonly file: string }[],
  label: string
): void {
  const seen = new Set<string>();
  for (const artifact of artifacts) {
    if (seen.has(artifact.file)) throw new Error(`${label} repeats artifact ${artifact.file}`);
    seen.add(artifact.file);
  }
}

function parseFreeformAggregateClosure(
  value: unknown,
  candidateCount: number
): ZhHansSenseInfoFreeformAggregateClosure {
  const label = 'rich candidate aggregateFreeformClosure';
  const row = object(value, label);
  exactKeys(row, [
    'currentUnresolvedCount', 'expectedOriginalUnresolvedCount',
    'priorRegisterFreeformCount', 'priorEtymologyFreeformCount', 'finalCount',
    'uniqueCoveredCount', 'aggregateSourceSha256', 'remainingCount', 'complete',
    'missingSources', 'unexpectedSources', 'crossArtifactOverlaps'
  ], label);
  const parsed: ZhHansSenseInfoFreeformAggregateClosure = {
    field: 'aggregateFreeformClosure',
    currentUnresolvedCount: integer(row.currentUnresolvedCount, `${label} currentUnresolvedCount`),
    expectedOriginalUnresolvedCount: integer(
      row.expectedOriginalUnresolvedCount,
      `${label} expectedOriginalUnresolvedCount`
    ),
    priorRegisterFreeformCount: integer(
      row.priorRegisterFreeformCount,
      `${label} priorRegisterFreeformCount`
    ),
    priorEtymologyFreeformCount: integer(
      row.priorEtymologyFreeformCount,
      `${label} priorEtymologyFreeformCount`
    ),
    finalCount: integer(row.finalCount, `${label} finalCount`),
    uniqueCoveredCount: integer(row.uniqueCoveredCount, `${label} uniqueCoveredCount`),
    aggregateSourceSha256: digest(row.aggregateSourceSha256, `${label} aggregateSourceSha256`),
    remainingCount: integer(row.remainingCount, `${label} remainingCount`),
    complete: boolean(row.complete, `${label} complete`),
    missingSources: stringArray(row.missingSources, `${label} missingSources`),
    unexpectedSources: stringArray(row.unexpectedSources, `${label} unexpectedSources`),
    crossArtifactOverlaps: stringArray(
      row.crossArtifactOverlaps,
      `${label} crossArtifactOverlaps`
    )
  };
  if (parsed.currentUnresolvedCount !== parsed.expectedOriginalUnresolvedCount
    || parsed.finalCount !== candidateCount
    || parsed.priorRegisterFreeformCount + parsed.priorEtymologyFreeformCount
      + parsed.finalCount !== parsed.uniqueCoveredCount
    || parsed.uniqueCoveredCount + parsed.remainingCount !== parsed.currentUnresolvedCount
    || parsed.complete !== (parsed.remainingCount === 0
      && parsed.missingSources.length === 0 && parsed.unexpectedSources.length === 0
      && parsed.crossArtifactOverlaps.length === 0)) {
    throw new Error(`${label} closure counts are inconsistent`);
  }
  return parsed;
}

function parseClusterAggregateClosure(
  value: unknown,
  candidateCount: number
): ZhHansSenseInfoClusterAggregateClosure {
  const label = 'rich candidate aggregateClusterClosure';
  const row = object(value, label);
  exactKeys(row, [
    'cluster', 'currentUnresolvedSourceCount', 'expectedOriginalUnresolvedSourceCount',
    'priorArtifacts', 'finalArtifact', 'uniqueCoveredSourceCount', 'coveredSourceSha256',
    'remainingUncoveredSourceCount', 'complete', 'missingSources', 'unexpectedSources',
    'crossArtifactOverlaps'
  ], label);
  if (!Array.isArray(row.priorArtifacts)) {
    throw new Error(`${label} priorArtifacts must be an array`);
  }
  const priorArtifacts = row.priorArtifacts.map((item, index) =>
    artifactReference(item, `${label} prior artifact ${index}`));
  const finalArtifact = artifactReference(row.finalArtifact, `${label} final artifact`);
  assertDistinctArtifacts([...priorArtifacts, finalArtifact], label);
  const parsed: ZhHansSenseInfoClusterAggregateClosure = {
    field: 'aggregateClusterClosure',
    cluster: text(row.cluster, `${label} cluster`),
    currentUnresolvedSourceCount: integer(
      row.currentUnresolvedSourceCount,
      `${label} currentUnresolvedSourceCount`
    ),
    expectedOriginalUnresolvedSourceCount: integer(
      row.expectedOriginalUnresolvedSourceCount,
      `${label} expectedOriginalUnresolvedSourceCount`
    ),
    priorArtifacts,
    finalArtifact,
    uniqueCoveredSourceCount: integer(
      row.uniqueCoveredSourceCount,
      `${label} uniqueCoveredSourceCount`
    ),
    coveredSourceSha256: digest(row.coveredSourceSha256, `${label} coveredSourceSha256`),
    remainingUncoveredSourceCount: integer(
      row.remainingUncoveredSourceCount,
      `${label} remainingUncoveredSourceCount`
    ),
    complete: boolean(row.complete, `${label} complete`),
    missingSources: stringArray(row.missingSources, `${label} missingSources`),
    unexpectedSources: stringArray(row.unexpectedSources, `${label} unexpectedSources`),
    crossArtifactOverlaps: stringArray(
      row.crossArtifactOverlaps,
      `${label} crossArtifactOverlaps`
    )
  };
  const priorCount = priorArtifacts.reduce((sum, item) => sum + item.sourceCount, 0);
  if (parsed.currentUnresolvedSourceCount !== parsed.expectedOriginalUnresolvedSourceCount
    || parsed.finalArtifact.sourceCount !== candidateCount
    || priorCount + parsed.finalArtifact.sourceCount !== parsed.uniqueCoveredSourceCount
    || parsed.uniqueCoveredSourceCount + parsed.remainingUncoveredSourceCount
      !== parsed.currentUnresolvedSourceCount
    || parsed.complete !== (parsed.remainingUncoveredSourceCount === 0
      && parsed.missingSources.length === 0 && parsed.unexpectedSources.length === 0
      && parsed.crossArtifactOverlaps.length === 0)) {
    throw new Error(`${label} closure counts are inconsistent`);
  }
  return parsed;
}

function parseAggregateClosure(
  field: string | null,
  value: unknown,
  candidateCount: number
): ZhHansSenseInfoAggregateClosure | null {
  if (field === null) return null;
  if (field === 'aggregateFreeformClosure') {
    return parseFreeformAggregateClosure(value, candidateCount);
  }
  if (field === 'aggregateClusterClosure') {
    return parseClusterAggregateClosure(value, candidateCount);
  }
  const row = object(value, `rich candidate ${field}`);
  exactKeys(row, [
    'artifacts', 'currentUnresolvedSourceCount', 'currentUnresolvedOccurrenceCount',
    'currentUnresolvedClusterCounts', 'priorBatchSourceCount', 'finalBatchSourceCount',
    'uniqueCoveredSourceCount', 'coveredSourceSha256', 'missingSources', 'unexpectedSources',
    'remainingUncoveredSourceCount', 'complete'
  ], `rich candidate ${field}`);
  if (!Array.isArray(row.artifacts) || row.artifacts.length === 0) {
    throw new Error(`Rich candidate ${field} artifacts must be a non-empty array`);
  }
  const batchWord = /^aggregate([A-Z][a-z]+)BatchClosure$/.exec(field)?.[1];
  if (!batchWord || row.artifacts.length !== AGGREGATE_CLOSURE_BATCH_COUNTS[batchWord]) {
    throw new Error(`Rich candidate ${field} artifact count does not match its field name`);
  }
  const seenFiles = new Set<string>();
  const artifacts = row.artifacts.map((item, index) => {
    const artifact = artifactReference(item, `rich candidate ${field} artifact ${index}`);
    const file = artifact.file;
    if (seenFiles.has(file)) throw new Error(`Rich candidate ${field} repeats artifact ${file}`);
    seenFiles.add(file);
    return {
      file,
      sourceCount: artifact.sourceCount
    };
  });
  const clusterRow = object(
    row.currentUnresolvedClusterCounts,
    `rich candidate ${field} currentUnresolvedClusterCounts`
  );
  const currentUnresolvedClusterCounts = Object.fromEntries(Object.entries(clusterRow).map(
    ([cluster, count]) => [cluster, integer(count, `rich candidate ${field} cluster ${cluster}`)]
  ));
  const parsed: ZhHansSenseInfoAggregateClosure = {
    field,
    artifacts,
    currentUnresolvedSourceCount: integer(
      row.currentUnresolvedSourceCount,
      `rich candidate ${field} currentUnresolvedSourceCount`
    ),
    currentUnresolvedOccurrenceCount: integer(
      row.currentUnresolvedOccurrenceCount,
      `rich candidate ${field} currentUnresolvedOccurrenceCount`
    ),
    currentUnresolvedClusterCounts,
    priorBatchSourceCount: integer(
      row.priorBatchSourceCount,
      `rich candidate ${field} priorBatchSourceCount`
    ),
    finalBatchSourceCount: integer(
      row.finalBatchSourceCount,
      `rich candidate ${field} finalBatchSourceCount`
    ),
    uniqueCoveredSourceCount: integer(
      row.uniqueCoveredSourceCount,
      `rich candidate ${field} uniqueCoveredSourceCount`
    ),
    coveredSourceSha256: digest(
      row.coveredSourceSha256,
      `rich candidate ${field} coveredSourceSha256`
    ),
    missingSources: stringArray(row.missingSources, `rich candidate ${field} missingSources`),
    unexpectedSources: stringArray(
      row.unexpectedSources,
      `rich candidate ${field} unexpectedSources`
    ),
    remainingUncoveredSourceCount: integer(
      row.remainingUncoveredSourceCount,
      `rich candidate ${field} remainingUncoveredSourceCount`
    ),
    complete: boolean(row.complete, `rich candidate ${field} complete`)
  };
  const artifactTotal = artifacts.reduce((sum, item) => sum + item.sourceCount, 0);
  const clusterTotal = Object.values(currentUnresolvedClusterCounts).reduce(
    (sum, count) => sum + count,
    0
  );
  if (parsed.finalBatchSourceCount !== candidateCount
    || artifacts.at(-1)?.sourceCount !== candidateCount
    || parsed.priorBatchSourceCount + parsed.finalBatchSourceCount
      !== parsed.uniqueCoveredSourceCount
    || artifactTotal !== parsed.uniqueCoveredSourceCount
    || clusterTotal !== parsed.currentUnresolvedSourceCount
    || parsed.uniqueCoveredSourceCount + parsed.remainingUncoveredSourceCount
      !== parsed.currentUnresolvedSourceCount
    || parsed.complete !== (parsed.remainingUncoveredSourceCount === 0
      && parsed.missingSources.length === 0 && parsed.unexpectedSources.length === 0)) {
    throw new Error(`Rich candidate ${field} closure counts are inconsistent`);
  }
  return parsed;
}

function parseRichGrammarAddCandidates(value: unknown): RichAddCandidateArtifact {
  const row = object(value, 'rich add candidate artifact');
  const baseKeys = [
    'formatVersion', 'kind', 'locale', 'sourceLocale', 'status', 'inputPolicy',
    'selection', 'generatedFrom', 'selfLqa', 'candidates'
  ];
  const closureField = aggregateClosureKey(row, baseKeys);
  exactKeys(row, [...baseKeys, ...(closureField ? [closureField] : [])], 'rich add candidate artifact');
  assertCommonEnvelope(
    row,
    RICH_ADD_CANDIDATE_KIND,
    'private-candidates-not-production',
    'rich add candidate artifact'
  );
  if (row.inputPolicy !== RICH_ADD_INPUT_POLICY) {
    throw new Error('Rich add candidate artifact lacks the Codex-only input policy');
  }
  object(row.selection, 'rich add candidate selection');
  object(row.selfLqa, 'rich add candidate selfLqa');
  if (!Array.isArray(row.candidates) || row.candidates.length === 0) {
    throw new Error('Rich add candidates must be a non-empty array');
  }
  let previous = '';
  const candidates = row.candidates.map((item, index) => {
    const label = `rich add candidate ${index}`;
    const candidate = object(item, label);
    exactKeys(
      candidate,
      ['source', 'target', 'cluster', 'contexts', 'rationale', 'uncertainty'],
      label
    );
    text(candidate.cluster, `${label} cluster`);
    text(candidate.rationale, `${label} rationale`);
    const parsed: RichAddCandidate = {
      source: text(candidate.source, `${label} source`),
      target: text(candidate.target, `${label} target`),
      rationale: candidate.rationale as string,
      uncertainty: uncertainty(candidate.uncertainty, `${label} uncertainty`),
      contexts: richContexts(candidate.contexts, `${label} contexts`)
    };
    if (parsed.source <= previous) throw new Error('Rich add candidates must be sorted and unique');
    previous = parsed.source;
    return parsed;
  });
  return {
    sourceKind: RICH_ADD_CANDIDATE_KIND,
    generatedFrom: generatedFrom(row.generatedFrom, 'rich add generatedFrom', true),
    candidates,
    aggregateClosure: parseAggregateClosure(
      closureField,
      closureField ? row[closureField] : undefined,
      candidates.length
    )
  };
}

function parseRichAlternateAddCandidates(value: unknown): RichAddCandidateArtifact {
  const row = object(value, 'rich alternate add candidate artifact');
  const baseKeys = [
    'formatVersion', 'locale', 'sourceLocale', 'status', 'translator', 'generatedFrom',
    'selection', 'selfReview', 'candidates'
  ];
  const closureField = aggregateClosureKey(row, baseKeys);
  exactKeys(
    row,
    [...baseKeys, ...(closureField ? [closureField] : [])],
    'rich alternate add candidate artifact'
  );
  if (row.formatVersion !== 1 || row.locale !== 'zh-Hans' || row.sourceLocale !== 'en'
    || row.status !== 'candidate-only'
    || row.translator !== 'Codex contextual pass; no external translation service') {
    throw new Error('Rich alternate candidate artifact has an unsupported envelope');
  }
  const selection = object(row.selection, 'rich alternate candidate selection');
  exactKeys(selection, [
    'register', 'namedEntity', 'freeform', 'candidateCount', 'occurrenceContextCount',
    'clusterCounts'
  ], 'rich alternate candidate selection');
  text(selection.register, 'rich alternate candidate selection register');
  text(selection.namedEntity, 'rich alternate candidate selection namedEntity');
  text(selection.freeform, 'rich alternate candidate selection freeform');
  const selfReview = object(row.selfReview, 'rich alternate candidate selfReview');
  exactKeys(selfReview, [
    'candidatesSortedBySource', 'duplicateSourceCount', 'identityTargetCount',
    'missingJapaneseTokenCount', 'unbalancedTargetDelimiterCount', 'latinTargetCount',
    'latinTargetsReviewed', 'mediumConfidenceCount', 'flaggedCandidateCount',
    'terminologyReview', 'semanticReview'
  ], 'rich alternate candidate selfReview');
  if (selfReview.candidatesSortedBySource !== true) {
    throw new Error('Rich alternate candidate selfReview must attest sorted candidates');
  }
  for (const key of [
    'duplicateSourceCount', 'identityTargetCount', 'missingJapaneseTokenCount',
    'unbalancedTargetDelimiterCount', 'latinTargetCount', 'mediumConfidenceCount',
    'flaggedCandidateCount'
  ]) integer(selfReview[key], `rich alternate candidate selfReview ${key}`);
  stringArray(selfReview.latinTargetsReviewed, 'rich alternate candidate latinTargetsReviewed');
  object(selfReview.terminologyReview, 'rich alternate candidate terminologyReview');
  text(selfReview.semanticReview, 'rich alternate candidate semanticReview');
  const provenance = object(row.generatedFrom, 'rich alternate candidate generatedFrom');
  exactKeys(provenance, ['lqaReport', 'sourceIdentities'], 'rich alternate candidate generatedFrom');
  const lqa = object(provenance.lqaReport, 'rich alternate candidate lqaReport');
  exactKeys(lqa, ['path', 'sha256'], 'rich alternate candidate lqaReport');
  text(lqa.path, 'rich alternate candidate lqaReport path');
  digest(lqa.sha256, 'rich alternate candidate lqaReport sha256');
  if (!Array.isArray(row.candidates) || row.candidates.length === 0) {
    throw new Error('Rich alternate candidates must be a non-empty array');
  }
  let previous = '';
  const candidates = row.candidates.map((item, index): RichAddCandidate => {
    const label = `rich alternate candidate ${index}`;
    const candidate = object(item, label);
    exactKeys(candidate, [
      'source', 'cluster', 'sourceRisk', 'target', 'translatorConfidence',
      'translatorRationale', 'uncertaintyFlags', 'occurrenceCount', 'occurrences'
    ], label);
    const source = text(candidate.source, `${label} source`);
    if (source <= previous) throw new Error('Rich alternate candidates must be sorted and unique');
    previous = source;
    text(candidate.cluster, `${label} cluster`);
    if (!['low', 'medium', 'high'].includes(String(candidate.sourceRisk))) {
      throw new Error(`${label} has an invalid sourceRisk`);
    }
    if (!['low', 'medium', 'high'].includes(String(candidate.translatorConfidence))) {
      throw new Error(`${label} has an invalid translatorConfidence`);
    }
    const flags = stringArray(candidate.uncertaintyFlags, `${label} uncertaintyFlags`);
    const contexts = richContexts(candidate.occurrences, `${label} occurrences`);
    if (integer(candidate.occurrenceCount, `${label} occurrenceCount`) !== contexts.length) {
      throw new Error(`${label} occurrenceCount does not match occurrences`);
    }
    const rationale = text(candidate.translatorRationale, `${label} translatorRationale`);
    return {
      source,
      target: text(candidate.target, `${label} target`),
      rationale,
      uncertainty: {
        level: confidenceUncertainty(candidate.translatorConfidence as RichRevisionReview['confidence']),
        rationale: flags.length === 0 ? rationale : `${rationale} Flags: ${flags.join(', ')}.`
      },
      contexts
    };
  });
  if (integer(selection.candidateCount, 'rich alternate candidate selection candidateCount')
      !== candidates.length
    || integer(
      selection.occurrenceContextCount,
      'rich alternate candidate selection occurrenceContextCount'
    ) !== candidates.reduce((sum, item) => sum + item.contexts.length, 0)) {
    throw new Error('Rich alternate candidate selection counts are stale');
  }
  const clusterCounts = object(selection.clusterCounts, 'rich alternate candidate clusterCounts');
  for (const [cluster, count] of Object.entries(clusterCounts)) {
    if (integer(count, `rich alternate candidate clusterCounts ${cluster}`)
      !== row.candidates.filter(item => object(item, 'rich alternate candidate').cluster === cluster).length) {
      throw new Error(`Rich alternate candidate cluster count is stale: ${cluster}`);
    }
  }
  return {
    sourceKind: RICH_ALTERNATE_CANDIDATE_KIND,
    generatedFrom: generatedFrom(
      provenance.sourceIdentities,
      'rich alternate candidate sourceIdentities',
      true
    ),
    candidates,
    aggregateClosure: parseAggregateClosure(
      closureField,
      closureField ? row[closureField] : undefined,
      candidates.length
    )
  };
}

function parseRichAddCandidates(value: unknown): RichAddCandidateArtifact {
  const row = object(value, 'rich add candidate artifact');
  return Object.hasOwn(row, 'translator')
    ? parseRichAlternateAddCandidates(value)
    : parseRichGrammarAddCandidates(value);
}

function decisionCounts(decisions: readonly { readonly decision: string }[]) {
  return {
    approve: decisions.filter(item => item.decision === 'approve').length,
    revise: decisions.filter(item => item.decision === 'revise').length,
    reject: decisions.filter(item => item.decision === 'reject').length
  };
}

function parseRichGrammarAddReview(value: unknown): RichAddReviewArtifact {
  const row = object(value, 'rich add review artifact');
  exactKeys(row, [
    'formatVersion', 'kind', 'locale', 'sourceLocale', 'status', 'reviewer',
    'input', 'criteria', 'summary', 'decisions'
  ], 'rich add review artifact');
  assertCommonEnvelope(
    row,
    RICH_ADD_REVIEW_KIND,
    'independent-review',
    'rich add review artifact'
  );
  if (row.reviewer !== RICH_ADD_REVIEWER) {
    throw new Error('Rich add review artifact lacks the Codex-only reviewer policy');
  }
  stringArray(row.criteria, 'rich add review criteria');
  const input = object(row.input, 'rich add review input');
  exactKeys(input, ['path', 'sha256', 'candidateCount', 'generatedFrom'], 'rich add review input');
  text(input.path, 'rich add review input path');
  const summary = object(row.summary, 'rich add review summary');
  exactKeys(summary, ['approve', 'revise', 'reject', 'total'], 'rich add review summary');
  if (!Array.isArray(row.decisions) || row.decisions.length === 0) {
    throw new Error('Rich add review decisions must be a non-empty array');
  }
  let previous = '';
  const decisions = row.decisions.map((item, index): RichAddReviewArtifact['decisions'][number] => {
    const label = `rich add review decision ${index}`;
    const decision = object(item, label);
    const action = decision.decision;
    if (action === 'revise') {
      exactKeys(
        decision,
        ['source', 'cluster', 'candidateTarget', 'decision', 'revisedTarget', 'reason', 'confidence'],
        label
      );
    } else if (action === 'approve' || action === 'reject') {
      exactKeys(
        decision,
        ['source', 'cluster', 'candidateTarget', 'decision', 'reason', 'confidence'],
        label
      );
    } else {
      throw new Error(`${label} has an invalid decision`);
    }
    const source = text(decision.source, `${label} source`);
    text(decision.cluster, `${label} cluster`);
    text(decision.candidateTarget, `${label} candidateTarget`);
    const rationale = text(decision.reason, `${label} reason`);
    if (!['high', 'medium', 'low'].includes(String(decision.confidence))) {
      throw new Error(`${label} has an invalid confidence`);
    }
    if (source <= previous) throw new Error('Rich add review decisions must be sorted and unique');
    previous = source;
    return action === 'revise'
      ? {
          source,
          candidateTarget: decision.candidateTarget as string,
          decision: 'revise',
          target: text(decision.revisedTarget, `${label} revisedTarget`),
          rationale
        }
      : { source, candidateTarget: decision.candidateTarget as string, decision: action, rationale };
  });
  const counts = decisionCounts(decisions);
  if (integer(input.candidateCount, 'rich add review candidateCount') !== decisions.length
    || integer(summary.total, 'rich add review summary total') !== decisions.length
    || integer(summary.approve, 'rich add review summary approve') !== counts.approve
    || integer(summary.revise, 'rich add review summary revise') !== counts.revise
    || integer(summary.reject, 'rich add review summary reject') !== counts.reject) {
    throw new Error('Rich add review counts do not match decisions');
  }
  return {
    sourceKind: RICH_ADD_REVIEW_KIND,
    candidateSha256: digest(input.sha256, 'rich add review candidate sha256'),
    generatedFrom: generatedFrom(input.generatedFrom, 'rich add review generatedFrom', true),
    decisions
  };
}

function parseRichAlternateAddReview(value: unknown): RichAddReviewArtifact {
  const row = object(value, 'rich alternate add review artifact');
  exactKeys(row, [
    'formatVersion', 'locale', 'sourceLocale', 'status', 'reviewer', 'reviewOf',
    'closure', 'summary', 'reviewPolicy', 'decisions'
  ], 'rich alternate add review artifact');
  if (row.formatVersion !== 1 || row.locale !== 'zh-Hans' || row.sourceLocale !== 'en'
    || row.status !== 'independent-review-complete'
    || ![
      'Codex independent contextual LQA; no Apple or external translation service',
      'Codex independent contextual LQA; no Apple or external machine translation service'
    ].includes(String(row.reviewer))) {
    throw new Error('Rich alternate review artifact has an unsupported envelope');
  }
  stringArray(row.reviewPolicy, 'rich alternate review policy');
  const reviewOf = object(row.reviewOf, 'rich alternate review reviewOf');
  exactKeys(reviewOf, ['path', 'sha256'], 'rich alternate review reviewOf');
  text(reviewOf.path, 'rich alternate review reviewOf path');
  const closure = object(row.closure, 'rich alternate review closure');
  exactKeys(closure, [
    'inputCandidateCount', 'inputUniqueSourceCount', 'expectedUniqueSourceCount',
    'decisionCount', 'decisionUniqueSourceCount', 'exactSourceClosure',
    'decisionsSortedBySource', 'sortComparator', 'reviewedOccurrenceContextCount'
  ], 'rich alternate review closure');
  const summary = object(row.summary, 'rich alternate review summary');
  exactKeys(summary, ['approve', 'revise', 'reject'], 'rich alternate review summary');
  if (!Array.isArray(row.decisions) || row.decisions.length === 0) {
    throw new Error('Rich alternate review decisions must be a non-empty array');
  }
  let previous = '';
  const decisions = row.decisions.map((item, index): RichAddReviewArtifact['decisions'][number] => {
    const label = `rich alternate review decision ${index}`;
    const decision = object(item, label);
    const countKeys = Object.hasOwn(decision, 'occurrenceCount')
      ? ['occurrenceCount', 'reviewedOccurrenceCount']
      : ['contextCount', 'reviewedContextCount'];
    const action = decision.decision;
    const expected = [
      'source', 'cluster', 'candidateTarget', 'decision', 'reason', 'confidence', ...countKeys,
      ...(action === 'revise' ? ['revisedTarget'] : [])
    ];
    exactKeys(decision, expected, label);
    if (action !== 'approve' && action !== 'revise' && action !== 'reject') {
      throw new Error(`${label} has an invalid decision`);
    }
    const source = text(decision.source, `${label} source`);
    if (source <= previous) throw new Error('Rich alternate review decisions must be sorted and unique');
    previous = source;
    text(decision.cluster, `${label} cluster`);
    const candidateTarget = text(decision.candidateTarget, `${label} candidateTarget`);
    const rationale = text(decision.reason, `${label} reason`);
    if (!['high', 'medium', 'low'].includes(String(decision.confidence))) {
      throw new Error(`${label} has an invalid confidence`);
    }
    const suppliedCount = integer(decision[countKeys[0]!], `${label} supplied context count`);
    const reviewedCount = integer(decision[countKeys[1]!], `${label} reviewed context count`);
    if (suppliedCount !== reviewedCount) throw new Error(`${label} did not review every context`);
    return action === 'revise'
      ? {
          source,
          candidateTarget,
          decision: 'revise',
          target: text(decision.revisedTarget, `${label} revisedTarget`),
          rationale,
          reviewedContextCount: reviewedCount
        }
      : { source, candidateTarget, decision: action, rationale, reviewedContextCount: reviewedCount };
  });
  const counts = decisionCounts(decisions);
  if (integer(closure.inputCandidateCount, 'rich alternate closure inputCandidateCount')
      !== decisions.length
    || integer(closure.inputUniqueSourceCount, 'rich alternate closure inputUniqueSourceCount')
      !== decisions.length
    || integer(closure.expectedUniqueSourceCount, 'rich alternate closure expectedUniqueSourceCount')
      !== decisions.length
    || integer(closure.decisionCount, 'rich alternate closure decisionCount') !== decisions.length
    || integer(
      closure.decisionUniqueSourceCount,
      'rich alternate closure decisionUniqueSourceCount'
    ) !== decisions.length
    || integer(
      closure.reviewedOccurrenceContextCount,
      'rich alternate closure reviewedOccurrenceContextCount'
    ) !== decisions.reduce((sum, item) => sum + (item.reviewedContextCount ?? 0), 0)
    || closure.exactSourceClosure !== true || closure.decisionsSortedBySource !== true
    || integer(summary.approve, 'rich alternate summary approve') !== counts.approve
    || integer(summary.revise, 'rich alternate summary revise') !== counts.revise
    || integer(summary.reject, 'rich alternate summary reject') !== counts.reject) {
    throw new Error('Rich alternate review closure or counts are invalid');
  }
  text(closure.sortComparator, 'rich alternate closure sortComparator');
  return {
    sourceKind: RICH_ALTERNATE_REVIEW_KIND,
    candidateSha256: digest(reviewOf.sha256, 'rich alternate review candidate sha256'),
    generatedFrom: null,
    decisions
  };
}

function parseRichAddReview(value: unknown): RichAddReviewArtifact {
  const row = object(value, 'rich add review artifact');
  return Object.hasOwn(row, 'reviewOf')
    ? parseRichAlternateAddReview(value)
    : parseRichGrammarAddReview(value);
}

function parseRichRevisionReview(value: unknown): RichRevisionReviewArtifact {
  const row = object(value, 'rich catalog review artifact');
  exactKeys(row, [
    'formatVersion', 'kind', 'locale', 'sourceLocale', 'status', 'reviewPolicy',
    'generatedFrom', 'summary', 'selfLqa', 'reviews'
  ], 'rich catalog review artifact');
  assertCommonEnvelope(
    row,
    RICH_REVISION_REVIEW_KIND,
    'private-review-not-production',
    'rich catalog review artifact'
  );
  if (row.reviewPolicy !== RICH_REVISION_POLICY) {
    throw new Error('Rich catalog review artifact lacks the Codex-only review policy');
  }
  object(row.summary, 'rich catalog review summary');
  object(row.selfLqa, 'rich catalog review selfLqa');
  if (!Array.isArray(row.reviews) || row.reviews.length === 0) {
    throw new Error('Rich catalog reviews must be a non-empty array');
  }
  let previous = '';
  const reviews = row.reviews.map((item, index): RichRevisionReview => {
    const label = `rich catalog review ${index}`;
    const review = object(item, label);
    exactKeys(review, [
      'source', 'currentTarget', 'decision', 'proposedTarget', 'reviewerConfidence',
      'reasons', 'contexts', 'checks'
    ], label);
    const source = text(review.source, `${label} source`);
    if (source <= previous) throw new Error('Rich catalog reviews must be sorted and unique');
    previous = source;
    const action = review.decision;
    if (action !== 'approve' && action !== 'revise') {
      throw new Error(`${label} has an invalid decision`);
    }
    const proposedTarget = review.proposedTarget === null
      ? null
      : text(review.proposedTarget, `${label} proposedTarget`);
    if ((action === 'approve') !== (proposedTarget === null)) {
      throw new Error(`${label} proposedTarget must be present only for a revision`);
    }
    if (!['high', 'medium', 'low'].includes(String(review.reviewerConfidence))) {
      throw new Error(`${label} has an invalid reviewerConfidence`);
    }
    const reasons = stringArray(review.reasons, `${label} reasons`);
    if (reasons.length === 0) throw new Error(`${label} reasons must not be empty`);
    const checks = object(review.checks, `${label} checks`);
    exactKeys(
      checks,
      ['contextCount', 'japaneseFragmentsPreserved', 'missingJapaneseFragments', 'standardizedPunctuation'],
      `${label} checks`
    );
    integer(checks.contextCount, `${label} checks contextCount`);
    if (typeof checks.japaneseFragmentsPreserved !== 'boolean'
      || typeof checks.standardizedPunctuation !== 'boolean') {
      throw new Error(`${label} checks flags must be boolean`);
    }
    stringArray(checks.missingJapaneseFragments, `${label} checks missingJapaneseFragments`);
    return {
      source,
      currentTarget: text(review.currentTarget, `${label} currentTarget`),
      decision: action,
      proposedTarget,
      rationale: reasons.join(' '),
      confidence: review.reviewerConfidence as RichRevisionReview['confidence'],
      contexts: richContexts(review.contexts, `${label} contexts`)
    };
  });
  return {
    sourceKind: RICH_REVISION_REVIEW_KIND,
    generatedFrom: generatedFrom(row.generatedFrom, 'rich catalog review generatedFrom', false),
    reviews
  };
}

function canonicalRichContexts(entries: readonly CanonicalEntry[]): ReadonlyMap<string, readonly RichContext[]> {
  const result = new Map<string, RichContext[]>();
  for (const entry of entries) {
    const headwords = [...new Set([
      ...entry.kanji.map(form => form.text),
      ...entry.kana.map(form => form.text)
    ])];
    for (const sense of entry.senses) {
      for (const property of sense.properties) {
        if (property.tag !== 's_inf') continue;
        const values = result.get(property.text) ?? [];
        values.push({
          seq: entry.seq,
          sense: sense.ordinal,
          info: property.ordinal,
          headwords,
          englishGlosses: sense.glosses
        });
        result.set(property.text, values);
      }
    }
  }
  return result;
}

function assertContextsCurrent(
  source: string,
  supplied: readonly RichContext[],
  current: ReadonlyMap<string, readonly RichContext[]>
): void {
  const expected = current.get(source);
  if (!expected || JSON.stringify(supplied) !== JSON.stringify(expected)) {
    throw new Error(`Rich artifact context is stale or incomplete: ${source}`);
  }
}

function assertMetadata(metadata: ZhHansSenseInfoAdapterMetadata): void {
  if (metadata.translator.runId === metadata.reviewer.runId) {
    throw new Error('Codex translator and reviewer must use distinct run IDs');
  }
}

function assertSourceIdentities(
  generated: RichAddCandidateArtifact['generatedFrom'],
  jmdict: ZhHansSenseInfoSourceIdentity,
  catalog: ZhHansSenseInfoSourceIdentity
): void {
  if (generated.jmdict.id !== jmdict.id || generated.jmdict.sha256 !== jmdict.sha256) {
    throw new Error('Rich artifact has a stale JMdict identity');
  }
  if (generated.catalog.id !== catalog.id || generated.catalog.sha256 !== catalog.sha256) {
    throw new Error('Rich artifact has a stale catalog identity');
  }
}

function generatedFromForStrict(value: RichAddCandidateArtifact['generatedFrom']) {
  return {
    jmdict: value.jmdict,
    catalog: value.catalog,
    patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY
  } as const;
}

export function serializeZhHansSenseInfoAuthoringArtifact(value: unknown): Uint8Array {
  return new TextEncoder().encode(`${JSON.stringify(value, null, 2)}\n`);
}

function hash(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function finishAdaptation(
  mode: 'add' | 'revisions',
  candidateArtifact: ZhHansSenseInfoCandidateArtifact,
  reviewArtifactWithoutDigest: Omit<ZhHansSenseInfoReviewArtifact, 'candidateSha256'>,
  inputCandidates: ZhHansSenseInfoAdaptationReceipt['inputs']['candidates'],
  inputReview: ZhHansSenseInfoAdaptationReceipt['inputs']['review'],
  sourceDecisionCounts: ZhHansSenseInfoAdaptationReceipt['sourceDecisionCounts'],
  aggregateClosures: readonly ZhHansSenseInfoAggregateClosure[],
  nonMutatingDecisions: readonly ZhHansSenseInfoNonMutatingReview[],
  excludedDecisions: readonly ZhHansSenseInfoExcludedReview[]
): AdaptedZhHansSenseInfoReview {
  const parsedCandidate = parseZhHansSenseInfoCandidateArtifact(candidateArtifact);
  const candidateBytes = serializeZhHansSenseInfoAuthoringArtifact(parsedCandidate);
  const reviewArtifact = parseZhHansSenseInfoReviewArtifact({
    ...reviewArtifactWithoutDigest,
    candidateSha256: hash(candidateBytes)
  });
  const reviewBytes = serializeZhHansSenseInfoAuthoringArtifact(reviewArtifact);
  return {
    candidateArtifact: parsedCandidate,
    candidateBytes,
    reviewArtifact,
    reviewBytes,
    receipt: {
      formatVersion: 1,
      kind: 'zh-hans-sense-info-adaptation-receipt',
      mode,
      inputs: { candidates: inputCandidates, review: inputReview },
      outputs: { candidateSha256: hash(candidateBytes), reviewSha256: hash(reviewBytes) },
      sourceDecisionCounts,
      emittedCandidateCount: parsedCandidate.candidates.length,
      aggregateClosures,
      nonMutatingDecisions,
      excludedDecisions
    }
  };
}

export function adaptRichZhHansSenseInfoAddBatch(options: {
  readonly entries: readonly CanonicalEntry[];
  readonly catalog: ZhHansSenseInfoCatalog;
  readonly jmdictIdentity: ZhHansSenseInfoSourceIdentity;
  readonly catalogIdentity: ZhHansSenseInfoSourceIdentity;
  readonly richCandidates: unknown;
  readonly richCandidateSha256: string;
  readonly richReview: unknown;
  readonly richReviewSha256: string;
  readonly metadata: ZhHansSenseInfoAdapterMetadata;
}): AdaptedZhHansSenseInfoReview {
  assertMetadata(options.metadata);
  const candidateInputDigest = digest(options.richCandidateSha256, 'rich candidate input digest');
  const reviewInputDigest = digest(options.richReviewSha256, 'rich review input digest');
  const candidates = parseRichAddCandidates(options.richCandidates);
  const review = parseRichAddReview(options.richReview);
  if (review.candidateSha256 !== candidateInputDigest) {
    throw new Error('Rich add review is bound to a different candidate artifact');
  }
  if (review.generatedFrom !== null
    && JSON.stringify(review.generatedFrom) !== JSON.stringify(candidates.generatedFrom)) {
    throw new Error('Rich add candidate and review source identities disagree');
  }
  assertSourceIdentities(candidates.generatedFrom, options.jmdictIdentity, options.catalogIdentity);
  const catalogSources = new Set(options.catalog.translations.map(item => item.source));
  const currentRichContexts = canonicalRichContexts(options.entries);
  const strictContexts = buildZhHansSenseInfoContextIndex(options.entries);
  if (candidates.candidates.length !== review.decisions.length) {
    throw new Error('Rich add review must cover every candidate');
  }
  for (let index = 0; index < candidates.candidates.length; index++) {
    const candidate = candidates.candidates[index]!;
    const decision = review.decisions[index]!;
    if (candidate.source !== decision.source) {
      throw new Error('Rich add review must cover every candidate in source order');
    }
    if (decision.candidateTarget !== candidate.target) {
      throw new Error('Rich add review candidate target does not match its candidate');
    }
    if (decision.reviewedContextCount !== undefined
      && decision.reviewedContextCount !== candidate.contexts.length) {
      throw new Error('Rich add review context count does not match its candidate');
    }
    if (catalogSources.has(candidate.source)) {
      throw new Error(`Rich add candidate conflicts with the current catalog: ${candidate.source}`);
    }
    assertContextsCurrent(candidate.source, candidate.contexts, currentRichContexts);
  }
  const candidateArtifact = {
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: generatedFromForStrict(candidates.generatedFrom),
    origin: {
      kind: 'adapted',
      sourceKind: candidates.sourceKind,
      sha256: candidateInputDigest
    },
    translator: options.metadata.translator,
    candidates: candidates.candidates.map(item => ({
      source: item.source,
      target: item.target,
      catalogAction: 'add' as const,
      uncertainty: item.uncertainty,
      contexts: strictContexts.get(item.source)!
    }))
  } as const;
  return finishAdaptation(
    'add',
    candidateArtifact,
    {
      formatVersion: 1,
      kind: 'zh-hans-sense-info-review-decisions',
      locale: 'zh-Hans',
      sourceLocale: 'en',
      origin: { kind: 'adapted', sourceKind: review.sourceKind, sha256: reviewInputDigest },
      reviewer: options.metadata.reviewer,
      decisions: review.decisions.map(({
        candidateTarget: _candidateTarget,
        reviewedContextCount: _reviewedContextCount,
        ...decision
      }) => decision)
    },
    { sourceKind: candidates.sourceKind, sha256: candidateInputDigest },
    { sourceKind: review.sourceKind, sha256: reviewInputDigest },
    decisionCounts(review.decisions),
    candidates.aggregateClosure ? [candidates.aggregateClosure] : [],
    [],
    []
  );
}

function confidenceUncertainty(confidence: RichRevisionReview['confidence']): ZhHansSenseInfoUncertainty['level'] {
  return confidence === 'high' ? 'low' : confidence === 'medium' ? 'medium' : 'high';
}

export function adaptRichZhHansSenseInfoCatalogRevisions(options: {
  readonly entries: readonly CanonicalEntry[];
  readonly catalog: ZhHansSenseInfoCatalog;
  readonly jmdictIdentity: ZhHansSenseInfoSourceIdentity;
  readonly catalogIdentity: ZhHansSenseInfoSourceIdentity;
  readonly richReview: unknown;
  readonly richReviewSha256: string;
  readonly metadata: ZhHansSenseInfoAdapterMetadata;
}): AdaptedZhHansSenseInfoReview {
  assertMetadata(options.metadata);
  const inputDigest = digest(options.richReviewSha256, 'rich catalog review input digest');
  const review = parseRichRevisionReview(options.richReview);
  assertSourceIdentities(review.generatedFrom, options.jmdictIdentity, options.catalogIdentity);
  const catalogTargets = new Map(options.catalog.translations.map(item => [item.source, item.target]));
  const currentRichContexts = canonicalRichContexts(options.entries);
  const strictContexts = buildZhHansSenseInfoContextIndex(options.entries);
  for (const item of review.reviews) {
    if (catalogTargets.get(item.source) !== item.currentTarget) {
      throw new Error(`Rich catalog review has a stale current target: ${item.source}`);
    }
    assertContextsCurrent(item.source, item.contexts, currentRichContexts);
  }
  const requestedRevisions = review.reviews.filter(
    (item): item is RichRevisionReview & { readonly decision: 'revise'; readonly proposedTarget: string } =>
      item.decision === 'revise' && item.proposedTarget !== null
  );
  const revisions = requestedRevisions.filter(item =>
    translateZhHansSenseInfoPattern(item.source) === null);
  const excludedDecisions: ZhHansSenseInfoExcludedReview[] = requestedRevisions.filter(item =>
    translateZhHansSenseInfoPattern(item.source) !== null).map(item => ({
      source: item.source,
      decision: 'revise',
      rationale: item.rationale,
      reason: 'direct-rule-resolved'
    }));
  if (revisions.length === 0) {
    throw new Error('Rich catalog review contains no eligible explicit revisions');
  }
  const candidateArtifact = {
    formatVersion: 1,
    kind: 'zh-hans-sense-info-codex-candidates',
    locale: 'zh-Hans',
    sourceLocale: 'en',
    generatedFrom: generatedFromForStrict(review.generatedFrom),
    origin: { kind: 'adapted', sourceKind: review.sourceKind, sha256: inputDigest },
    translator: options.metadata.translator,
    candidates: revisions.map(item => ({
      source: item.source,
      target: item.currentTarget,
      catalogAction: 'revise' as const,
      priorTarget: item.currentTarget,
      uncertainty: {
        level: confidenceUncertainty(item.confidence),
        rationale: 'Synthesized from the existing-catalog review starting target.'
      },
      contexts: strictContexts.get(item.source)!
    }))
  } as const;
  const decisions: ZhHansSenseInfoReviewerDecision[] = revisions.map(item => ({
    source: item.source,
    decision: 'revise',
    target: item.proposedTarget,
    rationale: item.rationale
  }));
  const nonMutatingDecisions = review.reviews.filter(item => item.decision === 'approve').map(item => ({
    source: item.source,
    decision: 'approve' as const,
    rationale: item.rationale
  }));
  return finishAdaptation(
    'revisions',
    candidateArtifact,
    {
      formatVersion: 1,
      kind: 'zh-hans-sense-info-review-decisions',
      locale: 'zh-Hans',
      sourceLocale: 'en',
      origin: { kind: 'adapted', sourceKind: review.sourceKind, sha256: inputDigest },
      reviewer: options.metadata.reviewer,
      decisions
    },
    null,
    { sourceKind: review.sourceKind, sha256: inputDigest },
    decisionCounts(review.reviews),
    [],
    nonMutatingDecisions,
    excludedDecisions
  );
}
