import { createHash } from 'node:crypto';

import type { BrowserAlphaArtifactCounts } from '../browser-pack/release-orchestration.js';
import type { GeneratedOrderAttestation } from './release-evidence.js';
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
    throw new Error(`${name} differs from the qualified baseline without a generated-order gate`);
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
