import { createHash } from 'node:crypto';

import type { BrowserAlphaArtifactCounts } from '../browser-pack/release-orchestration.js';
import type { SourceCompilerBinarySections } from './release-input.js';

export interface SurfaceCompilerStats {
  readonly input: number;
  readonly accepted: number;
  readonly direct: number;
  readonly morphology: number;
  readonly overlap: number;
  readonly omitted: number;
  readonly states: number;
  readonly edges: number;
  readonly bytes: number;
}

export interface QualifiedArtifactBytes {
  readonly surfaceIndex: Uint8Array;
  readonly rootPayload: Uint8Array;
  readonly morphology: Uint8Array;
  readonly analyzerSupport: Uint8Array;
  readonly analyzerAnnotations: Uint8Array;
  readonly details: Uint8Array;
}

export interface RootPayloadOrderReview {
  readonly sourceSha256: string;
  readonly qualifiedSha256: string;
  readonly attestation: Uint8Array;
  readonly attestationSha256: string;
  readonly fullEvidence: {
    readonly rows: number;
    readonly bytes: number;
    readonly sha256: string;
  };
  readonly provenance: string;
  readonly policy: string;
  readonly preservedBehavior: string;
}

export interface GeneratedOrderArtifactReview {
  readonly attestationSha256: string;
  readonly attestation: GeneratedOrderAttestation;
}

export interface RootPayloadOrderAttestation {
  readonly formatVersion: 1;
  readonly scope: string;
  readonly provenance: {
    readonly qualifiedArtifactTag: string;
    readonly upstreamIchiranCommit: string;
    readonly generatedBy: string;
    readonly qualifiedOrder: string;
  };
  readonly fullEvidence: RootPayloadOrderReview['fullEvidence'] & { readonly path: string };
  readonly sourcePayload: { readonly bytes: number; readonly sha256: string };
  readonly qualifiedPayload: { readonly bytes: number; readonly sha256: string };
  readonly deterministicPolicy: string;
  readonly preservedBehavior: {
    readonly verdict: string;
    readonly directSurfaceClasses: number;
    readonly deltaClasses: number;
    readonly formsInDeltaClasses: number;
    readonly changedFirstCandidates: number;
  };
}

export interface EvidenceFileIdentity {
  readonly path: string;
  readonly bytes: number;
  readonly sha256: string;
}

export interface GeneratedOrderProjectionIdentity {
  readonly semanticPaths: number;
  readonly matchedPaths: number;
  readonly records: number;
  readonly countExceptions: number;
  readonly physicalGroups: number;
  readonly physicalMembers: number;
  readonly propertyOverrides: number;
  readonly normalizedRecordSha256: string;
}

export interface GeneratedOrderReleaseSection {
  readonly bytes: number;
  readonly sha256: string;
  readonly counts: Readonly<Record<string, number>>;
}

export interface GeneratedOrderReleaseGate {
  readonly source: {
    readonly analyzerSupport: GeneratedOrderReleaseSection;
    readonly analyzerAnnotations: GeneratedOrderReleaseSection;
  };
  readonly qualified: {
    readonly analyzerSupport: GeneratedOrderReleaseSection;
    readonly analyzerAnnotations: GeneratedOrderReleaseSection;
  };
}

export interface GeneratedOrderAttestation {
  readonly formatVersion: 1;
  readonly scope: string;
  readonly provenance: {
    readonly qualifiedArtifactTag: string;
    readonly upstreamIchiranCommit: string;
    readonly generatedBy: string;
    readonly qualifiedHotPack: { readonly bytes: number; readonly sha256: string };
  };
  readonly sourceProjection: GeneratedOrderProjectionIdentity & {
    readonly projectionSha256: string;
    readonly paths: EvidenceFileIdentity;
    readonly occurrences: EvidenceFileIdentity & { readonly rows: number };
  };
  readonly qualifiedProjection: GeneratedOrderProjectionIdentity;
  readonly producerEvidence: {
    readonly report: EvidenceFileIdentity;
    readonly rawDifferences: EvidenceFileIdentity & { readonly rows: number };
    readonly normalizedDifferences: EvidenceFileIdentity & { readonly rows: number };
    readonly time: Omit<EvidenceFileIdentity, 'bytes'> & {
      readonly elapsedSeconds: number;
      readonly peakRssKiB: number;
    };
  };
  readonly normalizedRecordDelta: {
    readonly qualifiedOnly: number;
    readonly sourceOnly: number;
    readonly changed: number;
    readonly sha256: string;
    readonly physicalGroupBijection: {
      readonly common: number;
      readonly sourceOnly: number;
      readonly qualifiedOnly: number;
      readonly renumbered: number;
      readonly sha256: string;
    };
    readonly ambiguousMemberSignatures: number;
  };
  readonly lookupUniverse: {
    readonly comparedSurfaces: number;
    readonly exactSurfaces: number;
    readonly changedSurfaces: number;
    readonly sourceAmbiguousSurfaces: number;
    readonly qualifiedReachableAmbiguousSurfaces: number;
    readonly sourcePhysicalClasses: number;
    readonly qualifiedReachablePhysicalClasses: number;
    readonly sourceLocators: number;
    readonly qualifiedLocators: number;
    readonly groupingChanges: number;
    readonly groupingKana: number;
    readonly groupingKanji: number;
    readonly orderingOnlyChanges: number;
    readonly winnerChanges: number;
    readonly sourceOnlyLocators: 0;
    readonly qualifiedOnlyLocators: 0;
    readonly collisionStatusChanges: 0;
    readonly qualifiedMissingRanks: 0;
    readonly qualifiedRankConflicts: 0;
    readonly qualifiedRankPartitionChanges: 0;
    readonly reverseSourceOnlyLocators: 0;
    readonly reversePackedOnlyLocators: 0;
    readonly normalizedSha256: string;
    readonly fullEvidence: EvidenceFileIdentity & { readonly rows: number };
    readonly reportEvidence: EvidenceFileIdentity;
    readonly time: Omit<EvidenceFileIdentity, 'bytes'> & {
      readonly elapsedSeconds: number;
      readonly peakRssKiB: number;
    };
  };
  readonly qualifiedAccounting: {
    readonly declaredAmbiguousSurfaces: number;
    readonly reachableAmbiguousSurfaces: number;
    readonly unreachableSurface: string;
    readonly reason: string;
  };
  readonly releaseGate: GeneratedOrderReleaseGate | null;
  readonly decision: {
    readonly candidateUniverse: string;
    readonly ordering: string;
    readonly representation: string;
  };
}

export interface ArtifactComparison {
  readonly name: keyof QualifiedArtifactBytes;
  readonly source: { readonly bytes: number; readonly sha256: string };
  readonly qualified: { readonly bytes: number; readonly sha256: string };
  readonly byteEqual: boolean;
  readonly decision: 'exact' | 'reviewed-root-order-delta' | 'reviewed-generated-order-delta';
  readonly review?: Omit<RootPayloadOrderReview, 'attestation'>;
  readonly generatedReview?: {
    readonly attestationSha256: string;
    readonly scope: string;
    readonly candidateUniverse: string;
    readonly ordering: string;
    readonly representation: string;
  };
}

export type ArtifactIdentities = Readonly<Record<
  keyof QualifiedArtifactBytes,
  { readonly bytes: number; readonly sha256: string }
>>;

export interface CountDifference {
  readonly path: string;
  readonly source: number | null;
  readonly qualified: number | null;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
}

function text(value: unknown, label: string): string {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${label} must be text`);
  return value;
}

function integer(value: unknown, label: string): number {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${label} must be a non-negative integer`);
  }
  return Number(value);
}

function number(value: unknown, label: string): number {
  if (typeof value !== 'number' || !Number.isFinite(value) || value < 0) {
    throw new Error(`${label} must be a non-negative number`);
  }
  return value;
}

function digest(value: unknown, label: string): string {
  const result = text(value, label);
  if (!/^[0-9a-f]{64}$/.test(result)) throw new Error(`${label} must be a SHA-256`);
  return result;
}

export function parseRootPayloadOrderAttestation(value: unknown): RootPayloadOrderAttestation {
  const root = record(value, 'Root-order attestation');
  if (root.formatVersion !== 1) throw new Error('Unsupported root-order attestation format');
  const provenance = record(root.provenance, 'Root-order provenance');
  const fullEvidence = record(root.fullEvidence, 'Root-order full evidence');
  const sourcePayload = record(root.sourcePayload, 'Source root payload');
  const qualifiedPayload = record(root.qualifiedPayload, 'Qualified root payload');
  const preserved = record(root.preservedBehavior, 'Root-order preserved behavior');
  return {
    formatVersion: 1,
    scope: text(root.scope, 'Root-order scope'),
    provenance: {
      qualifiedArtifactTag: text(provenance.qualifiedArtifactTag, 'Qualified artifact tag'),
      upstreamIchiranCommit: text(provenance.upstreamIchiranCommit, 'Upstream Ichiran commit'),
      generatedBy: text(provenance.generatedBy, 'Root-order generator'),
      qualifiedOrder: text(provenance.qualifiedOrder, 'Qualified root order')
    },
    fullEvidence: {
      path: text(fullEvidence.path, 'Full evidence path'),
      rows: integer(fullEvidence.rows, 'Full evidence rows'),
      bytes: integer(fullEvidence.bytes, 'Full evidence bytes'),
      sha256: digest(fullEvidence.sha256, 'Full evidence digest')
    },
    sourcePayload: {
      bytes: integer(sourcePayload.bytes, 'Source root payload bytes'),
      sha256: digest(sourcePayload.sha256, 'Source root payload digest')
    },
    qualifiedPayload: {
      bytes: integer(qualifiedPayload.bytes, 'Qualified root payload bytes'),
      sha256: digest(qualifiedPayload.sha256, 'Qualified root payload digest')
    },
    deterministicPolicy: text(root.deterministicPolicy, 'Deterministic root-order policy'),
    preservedBehavior: {
      verdict: text(preserved.verdict, 'Root-order verdict'),
      directSurfaceClasses: integer(preserved.directSurfaceClasses, 'Direct surface classes'),
      deltaClasses: integer(preserved.deltaClasses, 'Root-order delta classes'),
      formsInDeltaClasses: integer(preserved.formsInDeltaClasses, 'Root-order delta forms'),
      changedFirstCandidates: integer(
        preserved.changedFirstCandidates,
        'Changed first candidates'
      )
    }
  };
}

function evidenceFile(value: unknown, label: string): EvidenceFileIdentity {
  const input = record(value, label);
  return {
    path: text(input.path, `${label} path`),
    bytes: integer(input.bytes, `${label} bytes`),
    sha256: digest(input.sha256, `${label} digest`)
  };
}

function projection(value: unknown, label: string): GeneratedOrderProjectionIdentity {
  const input = record(value, label);
  return {
    semanticPaths: integer(input.semanticPaths, `${label} semantic paths`),
    matchedPaths: integer(input.matchedPaths, `${label} matched paths`),
    records: integer(input.records, `${label} records`),
    countExceptions: integer(input.countExceptions, `${label} count exceptions`),
    physicalGroups: integer(input.physicalGroups, `${label} physical groups`),
    physicalMembers: integer(input.physicalMembers, `${label} physical members`),
    propertyOverrides: integer(input.propertyOverrides, `${label} property overrides`),
    normalizedRecordSha256: digest(
      input.normalizedRecordSha256,
      `${label} normalized record digest`
    )
  };
}

function releaseCounts(value: unknown, label: string): Readonly<Record<string, number>> {
  const input = record(value, label);
  const output: Record<string, number> = {};
  for (const [name, count] of Object.entries(input)) {
    output[name] = integer(count, `${label} ${name}`);
  }
  if (Object.keys(output).length === 0) throw new Error(`${label} must not be empty`);
  return output;
}

function releaseSection(value: unknown, label: string): GeneratedOrderReleaseSection {
  const input = record(value, label);
  return {
    bytes: integer(input.bytes, `${label} bytes`),
    sha256: digest(input.sha256, `${label} digest`),
    counts: releaseCounts(input.counts, `${label} counts`)
  };
}

function releaseArtifact(
  value: unknown,
  label: string
): GeneratedOrderReleaseGate['source'] {
  const input = record(value, label);
  return {
    analyzerSupport: releaseSection(input.analyzerSupport, `${label} analyzer support`),
    analyzerAnnotations: releaseSection(
      input.analyzerAnnotations,
      `${label} analyzer annotations`
    )
  };
}

function releaseGate(value: unknown): GeneratedOrderReleaseGate | null {
  if (value === null) return null;
  const input = record(value, 'Generated-order release gate');
  return {
    source: releaseArtifact(input.source, 'Source release gate'),
    qualified: releaseArtifact(input.qualified, 'Qualified release gate')
  };
}

/** Parse and enforce the exhaustive source/qualified generated-order proof gate. */
export function parseGeneratedOrderAttestation(value: unknown): GeneratedOrderAttestation {
  const root = record(value, 'Generated-order attestation');
  if (root.formatVersion !== 1) throw new Error('Unsupported generated-order attestation format');
  if (!Object.hasOwn(root, 'releaseGate')) {
    throw new Error('Generated-order attestation omits its atomic release gate');
  }
  const provenance = record(root.provenance, 'Generated-order provenance');
  const qualifiedHotPack = record(provenance.qualifiedHotPack, 'Qualified hot pack');
  const sourceInput = record(root.sourceProjection, 'Source generated projection');
  const occurrencesInput = record(sourceInput.occurrences, 'Generated occurrences');
  const sourceProjection = {
    ...projection(sourceInput, 'Source generated projection'),
    projectionSha256: digest(sourceInput.projectionSha256, 'Source projection digest'),
    paths: evidenceFile(sourceInput.paths, 'Generated paths'),
    occurrences: {
      ...evidenceFile(occurrencesInput, 'Generated occurrences'),
      rows: integer(occurrencesInput.rows, 'Generated occurrence rows')
    }
  };
  const qualifiedProjection = projection(root.qualifiedProjection, 'Qualified generated projection');
  const producer = record(root.producerEvidence, 'Generated producer evidence');
  const rawDifferences = record(producer.rawDifferences, 'Raw generated differences');
  const normalizedDifferences = record(
    producer.normalizedDifferences,
    'Normalized generated differences'
  );
  const producerTime = record(producer.time, 'Generated producer time');
  const deltaInput = record(root.normalizedRecordDelta, 'Normalized generated delta');
  const bijectionInput = record(deltaInput.physicalGroupBijection, 'Physical group bijection');
  const universeInput = record(root.lookupUniverse, 'Generated lookup universe');
  const fullEvidenceInput = record(universeInput.fullEvidence, 'Generated lookup full evidence');
  const universeTime = record(universeInput.time, 'Generated lookup proof time');
  const zeroFields = [
    'sourceOnlyLocators',
    'qualifiedOnlyLocators',
    'collisionStatusChanges',
    'qualifiedMissingRanks',
    'qualifiedRankConflicts',
    'qualifiedRankPartitionChanges',
    'reverseSourceOnlyLocators',
    'reversePackedOnlyLocators'
  ] as const;
  for (const field of zeroFields) {
    if (integer(universeInput[field], `Generated lookup ${field}`) !== 0) {
      throw new Error(`Generated lookup proof has unresolved ${field}`);
    }
  }
  const fullEvidence = {
    ...evidenceFile(fullEvidenceInput, 'Generated lookup full evidence'),
    rows: integer(fullEvidenceInput.rows, 'Generated lookup full evidence rows')
  };
  const lookupUniverse = {
    comparedSurfaces: integer(universeInput.comparedSurfaces, 'Compared generated surfaces'),
    exactSurfaces: integer(universeInput.exactSurfaces, 'Exact generated surfaces'),
    changedSurfaces: integer(universeInput.changedSurfaces, 'Changed generated surfaces'),
    sourceAmbiguousSurfaces: integer(
      universeInput.sourceAmbiguousSurfaces,
      'Source ambiguous surfaces'
    ),
    qualifiedReachableAmbiguousSurfaces: integer(
      universeInput.qualifiedReachableAmbiguousSurfaces,
      'Qualified reachable ambiguous surfaces'
    ),
    sourcePhysicalClasses: integer(universeInput.sourcePhysicalClasses, 'Source physical classes'),
    qualifiedReachablePhysicalClasses: integer(
      universeInput.qualifiedReachablePhysicalClasses,
      'Qualified reachable physical classes'
    ),
    sourceLocators: integer(universeInput.sourceLocators, 'Source generated locators'),
    qualifiedLocators: integer(universeInput.qualifiedLocators, 'Qualified generated locators'),
    groupingChanges: integer(universeInput.groupingChanges, 'Generated grouping changes'),
    groupingKana: integer(universeInput.groupingKana, 'Generated kana grouping changes'),
    groupingKanji: integer(universeInput.groupingKanji, 'Generated kanji grouping changes'),
    orderingOnlyChanges: integer(
      universeInput.orderingOnlyChanges,
      'Generated ordering-only changes'
    ),
    winnerChanges: integer(universeInput.winnerChanges, 'Generated winner changes'),
    sourceOnlyLocators: 0 as const,
    qualifiedOnlyLocators: 0 as const,
    collisionStatusChanges: 0 as const,
    qualifiedMissingRanks: 0 as const,
    qualifiedRankConflicts: 0 as const,
    qualifiedRankPartitionChanges: 0 as const,
    reverseSourceOnlyLocators: 0 as const,
    reversePackedOnlyLocators: 0 as const,
    normalizedSha256: digest(universeInput.normalizedSha256, 'Generated lookup digest'),
    fullEvidence,
    reportEvidence: evidenceFile(universeInput.reportEvidence, 'Generated lookup report'),
    time: {
      path: text(universeTime.path, 'Generated lookup proof time path'),
      elapsedSeconds: number(universeTime.elapsedSeconds, 'Generated lookup elapsed seconds'),
      peakRssKiB: integer(universeTime.peakRssKiB, 'Generated lookup peak RSS'),
      sha256: digest(universeTime.sha256, 'Generated lookup time digest')
    }
  };
  if (sourceProjection.semanticPaths !== qualifiedProjection.semanticPaths
    || sourceProjection.matchedPaths !== qualifiedProjection.matchedPaths) {
    throw new Error('Generated source and qualified path coverage differs');
  }
  if (lookupUniverse.sourceLocators !== lookupUniverse.qualifiedLocators) {
    throw new Error('Generated source and qualified locator universes differ');
  }
  if (lookupUniverse.comparedSurfaces
      !== lookupUniverse.exactSurfaces + lookupUniverse.changedSurfaces
    || lookupUniverse.changedSurfaces
      !== lookupUniverse.groupingChanges + lookupUniverse.orderingOnlyChanges
    || lookupUniverse.groupingChanges
      !== lookupUniverse.groupingKana + lookupUniverse.groupingKanji
    || lookupUniverse.changedSurfaces !== fullEvidence.rows) {
    throw new Error('Generated lookup proof counts do not close');
  }
  const accounting = record(root.qualifiedAccounting, 'Qualified generated accounting');
  const qualifiedAccounting = {
    declaredAmbiguousSurfaces: integer(
      accounting.declaredAmbiguousSurfaces,
      'Declared qualified ambiguous surfaces'
    ),
    reachableAmbiguousSurfaces: integer(
      accounting.reachableAmbiguousSurfaces,
      'Reachable qualified ambiguous surfaces'
    ),
    unreachableSurface: text(accounting.unreachableSurface, 'Unreachable qualified surface'),
    reason: text(accounting.reason, 'Qualified generated accounting reason')
  };
  if (qualifiedAccounting.declaredAmbiguousSurfaces
      !== qualifiedAccounting.reachableAmbiguousSurfaces + 1) {
    throw new Error('Qualified generated tombstone accounting does not close');
  }
  const decision = record(root.decision, 'Generated-order decision');
  return {
    formatVersion: 1,
    scope: text(root.scope, 'Generated-order scope'),
    provenance: {
      qualifiedArtifactTag: text(provenance.qualifiedArtifactTag, 'Qualified artifact tag'),
      upstreamIchiranCommit: text(provenance.upstreamIchiranCommit, 'Upstream Ichiran commit'),
      generatedBy: text(provenance.generatedBy, 'Generated-order proof generator'),
      qualifiedHotPack: {
        bytes: integer(qualifiedHotPack.bytes, 'Qualified hot pack bytes'),
        sha256: digest(qualifiedHotPack.sha256, 'Qualified hot pack digest')
      }
    },
    sourceProjection,
    qualifiedProjection,
    producerEvidence: {
      report: evidenceFile(producer.report, 'Generated producer report'),
      rawDifferences: {
        ...evidenceFile(rawDifferences, 'Raw generated differences'),
        rows: integer(rawDifferences.rows, 'Raw generated difference rows')
      },
      normalizedDifferences: {
        ...evidenceFile(normalizedDifferences, 'Normalized generated differences'),
        rows: integer(normalizedDifferences.rows, 'Normalized generated difference rows')
      },
      time: {
        path: text(producerTime.path, 'Generated producer time path'),
        elapsedSeconds: number(producerTime.elapsedSeconds, 'Generated producer elapsed seconds'),
        peakRssKiB: integer(producerTime.peakRssKiB, 'Generated producer peak RSS'),
        sha256: digest(producerTime.sha256, 'Generated producer time digest')
      }
    },
    normalizedRecordDelta: {
      qualifiedOnly: integer(deltaInput.qualifiedOnly, 'Qualified-only generated records'),
      sourceOnly: integer(deltaInput.sourceOnly, 'Source-only generated records'),
      changed: integer(deltaInput.changed, 'Changed generated records'),
      sha256: digest(deltaInput.sha256, 'Normalized generated delta digest'),
      physicalGroupBijection: {
        common: integer(bijectionInput.common, 'Common generated groups'),
        sourceOnly: integer(bijectionInput.sourceOnly, 'Source-only generated groups'),
        qualifiedOnly: integer(bijectionInput.qualifiedOnly, 'Qualified-only generated groups'),
        renumbered: integer(bijectionInput.renumbered, 'Renumbered generated groups'),
        sha256: digest(bijectionInput.sha256, 'Physical group bijection digest')
      },
      ambiguousMemberSignatures: integer(
        deltaInput.ambiguousMemberSignatures,
        'Ambiguous generated member signatures'
      )
    },
    lookupUniverse,
    qualifiedAccounting,
    releaseGate: releaseGate(root.releaseGate),
    decision: {
      candidateUniverse: text(decision.candidateUniverse, 'Generated candidate decision'),
      ordering: text(decision.ordering, 'Generated ordering decision'),
      representation: text(decision.representation, 'Generated representation decision')
    }
  };
}

export function artifactIdentities(bytes: QualifiedArtifactBytes): ArtifactIdentities {
  return {
    surfaceIndex: { bytes: bytes.surfaceIndex.byteLength, sha256: sha256(bytes.surfaceIndex) },
    rootPayload: { bytes: bytes.rootPayload.byteLength, sha256: sha256(bytes.rootPayload) },
    morphology: { bytes: bytes.morphology.byteLength, sha256: sha256(bytes.morphology) },
    analyzerSupport: {
      bytes: bytes.analyzerSupport.byteLength,
      sha256: sha256(bytes.analyzerSupport)
    },
    analyzerAnnotations: {
      bytes: bytes.analyzerAnnotations.byteLength,
      sha256: sha256(bytes.analyzerAnnotations)
    },
    details: { bytes: bytes.details.byteLength, sha256: sha256(bytes.details) }
  };
}

export function generatedOrderReleaseGateCandidate(
  sourceBytes: QualifiedArtifactBytes,
  qualifiedBytes: QualifiedArtifactBytes,
  sourceCounts: BrowserAlphaArtifactCounts,
  qualifiedCounts: BrowserAlphaArtifactCounts
): GeneratedOrderReleaseGate {
  const source = artifactIdentities(sourceBytes);
  const qualified = artifactIdentities(qualifiedBytes);
  return {
    source: {
      analyzerSupport: {
        ...source.analyzerSupport,
        counts: { ...sourceCounts.analyzerSupport }
      },
      analyzerAnnotations: {
        ...source.analyzerAnnotations,
        counts: { ...sourceCounts.annotations }
      }
    },
    qualified: {
      analyzerSupport: {
        ...qualified.analyzerSupport,
        counts: { ...qualifiedCounts.analyzerSupport }
      },
      analyzerAnnotations: {
        ...qualified.analyzerAnnotations,
        counts: { ...qualifiedCounts.annotations }
      }
    }
  };
}

function take(values: ReadonlyMap<string, number>, name: string): number {
  const value = values.get(name);
  if (value === undefined || !Number.isSafeInteger(value)) {
    throw new Error(`Surface compiler omitted ${name}`);
  }
  return value;
}

export function parseSurfaceCompilerStats(stderr: string): SurfaceCompilerStats {
  const line = stderr.trim().split('\n').find(value => value.startsWith('surfaces='));
  if (!line) throw new Error('Surface compiler did not emit its deterministic stats line');
  const values = new Map<string, number>();
  for (const field of line.split(' ')) {
    const match = /^([a-z_]+)=([0-9]+)$/.exec(field);
    if (match && match[1] !== 'elapsed_ms') values.set(match[1], Number(match[2]));
  }
  return {
    input: take(values, 'surfaces'),
    accepted: take(values, 'accepted'),
    direct: take(values, 'direct'),
    morphology: take(values, 'morphology'),
    overlap: take(values, 'overlap'),
    omitted: take(values, 'omitted'),
    states: take(values, 'states'),
    edges: take(values, 'edges'),
    bytes: take(values, 'bytes')
  };
}

export function sourceReleaseArtifactCounts(
  sections: SourceCompilerBinarySections,
  surface: SurfaceCompilerStats
): BrowserAlphaArtifactCounts {
  return {
    surfaceIndex: {
      input: surface.input,
      accepted: surface.accepted,
      direct: surface.direct,
      morphology: surface.morphology,
      overlap: surface.overlap,
      omitted: surface.omitted,
      states: surface.states,
      edges: surface.edges
    },
    rootPayload: {
      surfaces: sections.root.stats.counts.surfaces,
      forms: sections.root.stats.counts.forms,
      entries: sections.root.stats.counts.entries,
      restrictions: sections.root.stats.counts.restrictions
    },
    morphology: {
      positions: sections.morphology.stats.positions,
      rules: sections.morphology.stats.rules,
      templates: sections.morphology.stats.templates,
      suffixes: sections.morphology.stats.suffixes,
      rootKeys: sections.morphology.stats.rootKeys,
      rootGroups: sections.morphology.stats.rootGroups,
      patches: sections.morphology.stats.patches,
      tombstones: sections.morphology.stats.tombstones
    },
    analyzerSupport: {
      suffixKeys: sections.support.stats.counts.suffixKeys,
      suffixValues: sections.support.stats.counts.suffixValues,
      suffixClasses: sections.support.stats.counts.suffixClasses,
      counterKeys: sections.support.stats.counts.counterKeys,
      counterVariants: sections.support.stats.counts.counterVariants,
      collisions: sections.support.stats.counts.collisions,
      generatedRules: sections.support.stats.counts.generatedRules,
      generatedAliases: sections.support.stats.counts.generatedAliases
    },
    annotations: {
      blocks: sections.annotations.stats.blocks,
      splits: sections.annotations.stats.splits,
      hints: sections.annotations.stats.hints,
      generatedBlocks: sections.annotations.stats.generatedBlocks,
      generatedRoots: sections.annotations.stats.generatedRoots,
      generatedRecords: sections.annotations.stats.generatedRecords,
      lookupOrderRecords: sections.annotations.stats.lookupOrderRecords,
      lookupOrderRoots: sections.annotations.stats.lookupOrderRoots,
      lookupOrderBytes: sections.annotations.stats.lookupOrderBytes,
      lookupOrderExceptionSurfaces: sections.annotations.stats.lookupOrderExceptionSurfaces,
      lookupOrderExceptionClasses: sections.annotations.stats.lookupOrderExceptionClasses,
      lookupOrderExceptionLocators: sections.annotations.stats.lookupOrderExceptionLocators,
      lookupOrderExceptionBytes: sections.annotations.stats.lookupOrderExceptionBytes,
      generatedPhysicalGroups: sections.annotations.stats.generatedPhysicalGroups,
      generatedFactPairs: sections.annotations.stats.generatedFactPairs,
      indexBytes: sections.annotations.stats.indexBytes,
      uncompressedBytes: sections.annotations.stats.uncompressedBytes,
      compressedBytes: sections.annotations.stats.compressedBytes,
      annotationUncompressedBytes: sections.annotations.stats.annotationUncompressedBytes,
      annotationCompressedBytes: sections.annotations.stats.annotationCompressedBytes,
      generatedUncompressedBytes: sections.annotations.stats.generatedUncompressedBytes,
      generatedCompressedBytes: sections.annotations.stats.generatedCompressedBytes,
      totalBytes: sections.annotations.stats.totalBytes,
      largestUncompressedBlock: sections.annotations.stats.largestUncompressedBlock,
      largestGeneratedBlock: sections.annotations.stats.largestGeneratedBlock,
      largestGeneratedCompressedBlock: sections.annotations.stats.largestGeneratedCompressedBlock
    },
    details: {
      entries: sections.details.stats.entryCount,
      forms: sections.details.stats.formCount,
      senses: sections.details.stats.senseCount,
      glosses: sections.details.stats.glossCount,
      properties: sections.details.stats.propertyCount
    }
  };
}

function compareCountGroup<Group extends object>(
  source: Group,
  qualified: Group,
  group: string,
  output: CountDifference[]
): void {
  const keys = new Set([...Object.keys(source), ...Object.keys(qualified)]);
  for (const key of [...keys].sort() as Array<keyof Group>) {
    const sourceValue = source[key];
    const qualifiedValue = qualified[key];
    if (typeof sourceValue !== 'number' || typeof qualifiedValue !== 'number') {
      throw new Error(`Release count ${group}.${String(key)} is not numeric`);
    }
    if (sourceValue !== qualifiedValue) {
      output.push({
        path: `${group}.${String(key)}`,
        source: sourceValue,
        qualified: qualifiedValue
      });
    }
  }
}

export function compareArtifactCounts(
  source: BrowserAlphaArtifactCounts,
  qualified: BrowserAlphaArtifactCounts
): readonly CountDifference[] {
  const output: CountDifference[] = [];
  compareCountGroup(source.surfaceIndex, qualified.surfaceIndex, 'surfaceIndex', output);
  compareCountGroup(source.rootPayload, qualified.rootPayload, 'rootPayload', output);
  compareCountGroup(source.morphology, qualified.morphology, 'morphology', output);
  compareCountGroup(source.analyzerSupport, qualified.analyzerSupport, 'analyzerSupport', output);
  compareCountGroup(source.annotations, qualified.annotations, 'annotations', output);
  compareCountGroup(source.details, qualified.details, 'details', output);
  return output;
}

function assertPinnedCountGroup(
  actual: object,
  pinned: Readonly<Record<string, number>>,
  label: string
): void {
  const differences: CountDifference[] = [];
  compareCountGroup(actual, pinned, label, differences);
  if (differences.length !== 0) {
    throw new Error(`${label} counts differ from the generated-order release gate: `
      + JSON.stringify(differences));
  }
}

export function compareQualifiedArtifactCounts(
  source: BrowserAlphaArtifactCounts,
  qualified: BrowserAlphaArtifactCounts,
  review: GeneratedOrderArtifactReview
): readonly CountDifference[] {
  const gate = review.attestation.releaseGate;
  if (gate === null) {
    throw new Error('Generated-order release gate has no pinned source section identities');
  }
  const unreviewed: CountDifference[] = [];
  compareCountGroup(source.surfaceIndex, qualified.surfaceIndex, 'surfaceIndex', unreviewed);
  compareCountGroup(source.rootPayload, qualified.rootPayload, 'rootPayload', unreviewed);
  compareCountGroup(source.morphology, qualified.morphology, 'morphology', unreviewed);
  compareCountGroup(source.details, qualified.details, 'details', unreviewed);
  if (unreviewed.length !== 0) {
    throw new Error(`Unreviewed qualified artifact count differences: ${JSON.stringify(unreviewed)}`);
  }
  assertPinnedCountGroup(
    source.analyzerSupport,
    gate.source.analyzerSupport.counts,
    'Source analyzerSupport'
  );
  assertPinnedCountGroup(
    qualified.analyzerSupport,
    gate.qualified.analyzerSupport.counts,
    'Qualified analyzerSupport'
  );
  assertPinnedCountGroup(
    source.annotations,
    gate.source.analyzerAnnotations.counts,
    'Source annotations'
  );
  assertPinnedCountGroup(
    qualified.annotations,
    gate.qualified.analyzerAnnotations.counts,
    'Qualified annotations'
  );
  return compareArtifactCounts(source, qualified);
}

function artifactIdentity(bytes: Uint8Array): { readonly bytes: number; readonly sha256: string } {
  return { bytes: bytes.byteLength, sha256: sha256(bytes) };
}

function exactArtifact(
  name: keyof QualifiedArtifactBytes,
  source: Uint8Array,
  qualified: Uint8Array
): ArtifactComparison {
  const sourceIdentity = artifactIdentity(source);
  const qualifiedIdentity = artifactIdentity(qualified);
  if (!Buffer.from(source).equals(Buffer.from(qualified))) {
    throw new Error(
      `${name} differs from the qualified baseline: `
      + `${sourceIdentity.sha256} != ${qualifiedIdentity.sha256}`
    );
  }
  return {
    name,
    source: sourceIdentity,
    qualified: qualifiedIdentity,
    byteEqual: true,
    decision: 'exact'
  };
}

function reviewedGeneratedArtifact(
  name: 'analyzerSupport' | 'analyzerAnnotations',
  source: Uint8Array,
  qualified: Uint8Array,
  review: GeneratedOrderArtifactReview | undefined
): ArtifactComparison {
  const sourceIdentity = artifactIdentity(source);
  const qualifiedIdentity = artifactIdentity(qualified);
  const byteEqual = Buffer.from(source).equals(Buffer.from(qualified));
  const gate = review?.attestation.releaseGate;
  if (!gate) {
    if (byteEqual) return { name, source: sourceIdentity, qualified: qualifiedIdentity,
      byteEqual: true, decision: 'exact' };
    throw new Error(`${name} differs from the qualified baseline without a complete generated-order gate`);
  }
  const section = name === 'analyzerSupport'
    ? { source: gate.source.analyzerSupport, qualified: gate.qualified.analyzerSupport }
    : { source: gate.source.analyzerAnnotations, qualified: gate.qualified.analyzerAnnotations };
  for (const [label, actual, pinned] of [
    ['Source', sourceIdentity, section.source],
    ['Qualified', qualifiedIdentity, section.qualified]
  ] as const) {
    if (actual.bytes !== pinned.bytes || actual.sha256 !== pinned.sha256) {
      throw new Error(
        `${label} ${name} identity differs from the generated-order release gate: `
        + `${actual.bytes}/${actual.sha256} != ${pinned.bytes}/${pinned.sha256}`
      );
    }
  }
  if (byteEqual) return { name, source: sourceIdentity, qualified: qualifiedIdentity,
    byteEqual: true, decision: 'exact' };
  return {
    name,
    source: sourceIdentity,
    qualified: qualifiedIdentity,
    byteEqual: false,
    decision: 'reviewed-generated-order-delta',
    generatedReview: {
      attestationSha256: review.attestationSha256,
      scope: review.attestation.scope,
      candidateUniverse: review.attestation.decision.candidateUniverse,
      ordering: review.attestation.decision.ordering,
      representation: review.attestation.decision.representation
    }
  };
}

export function compareQualifiedArtifactBytes(
  source: QualifiedArtifactBytes,
  qualified: QualifiedArtifactBytes,
  rootReview: RootPayloadOrderReview,
  generatedReview?: GeneratedOrderArtifactReview
): readonly ArtifactComparison[] {
  if (sha256(rootReview.attestation) !== rootReview.attestationSha256) {
    throw new Error('Direct-root ordering attestation digest does not match its review');
  }
  if (!Number.isSafeInteger(rootReview.fullEvidence.rows) || rootReview.fullEvidence.rows < 0
    || !Number.isSafeInteger(rootReview.fullEvidence.bytes) || rootReview.fullEvidence.bytes < 0
    || !/^[0-9a-f]{64}$/.test(rootReview.fullEvidence.sha256)) {
    throw new Error('Direct-root full evidence identity is invalid');
  }

  const sourceRoot = artifactIdentity(source.rootPayload);
  const qualifiedRoot = artifactIdentity(qualified.rootPayload);
  if (sourceRoot.sha256 !== rootReview.sourceSha256) {
    throw new Error(
      `Source root payload digest ${sourceRoot.sha256} is not the reviewed ${rootReview.sourceSha256}`
    );
  }
  if (qualifiedRoot.sha256 !== rootReview.qualifiedSha256) {
    throw new Error(
      `Qualified root payload digest ${qualifiedRoot.sha256} is not the reviewed `
      + rootReview.qualifiedSha256
    );
  }
  const rootByteEqual = Buffer.from(source.rootPayload).equals(Buffer.from(qualified.rootPayload));
  const root: ArtifactComparison = {
    name: 'rootPayload',
    source: sourceRoot,
    qualified: qualifiedRoot,
    byteEqual: rootByteEqual,
    decision: rootByteEqual ? 'exact' : 'reviewed-root-order-delta',
    ...(rootByteEqual ? {} : {
      review: {
        sourceSha256: rootReview.sourceSha256,
        qualifiedSha256: rootReview.qualifiedSha256,
        attestationSha256: rootReview.attestationSha256,
        fullEvidence: rootReview.fullEvidence,
        provenance: rootReview.provenance,
        policy: rootReview.policy,
        preservedBehavior: rootReview.preservedBehavior
      }
    })
  };
  return [
    exactArtifact('surfaceIndex', source.surfaceIndex, qualified.surfaceIndex),
    root,
    exactArtifact('morphology', source.morphology, qualified.morphology),
    reviewedGeneratedArtifact(
      'analyzerSupport', source.analyzerSupport, qualified.analyzerSupport, generatedReview
    ),
    reviewedGeneratedArtifact(
      'analyzerAnnotations',
      source.analyzerAnnotations,
      qualified.analyzerAnnotations,
      generatedReview
    ),
    exactArtifact('details', source.details, qualified.details)
  ];
}
