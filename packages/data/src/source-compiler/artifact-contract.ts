import { createHash } from 'node:crypto';

export interface BrowserAlphaArtifactCounts {
  readonly surfaceIndex: {
    readonly input: number;
    readonly accepted: number;
    readonly direct: number;
    readonly morphology: number;
    readonly overlap: number;
    readonly omitted: number;
    readonly states: number;
    readonly edges: number;
  };
  readonly rootPayload: {
    readonly surfaces: number;
    readonly forms: number;
    readonly entries: number;
    readonly restrictions: number;
  };
  readonly morphology: {
    readonly positions: number;
    readonly rules: number;
    readonly templates: number;
    readonly suffixes: number;
    readonly rootKeys: number;
    readonly rootGroups: number;
    readonly patches: number;
    readonly tombstones: number;
  };
  readonly analyzerSupport: {
    readonly suffixKeys: number;
    readonly suffixValues: number;
    readonly suffixClasses: number;
    readonly counterKeys: number;
    readonly counterVariants: number;
    readonly collisions: number;
    readonly generatedRules: number;
    readonly generatedAliases: number;
  };
  readonly annotations: {
    readonly blocks: number;
    readonly splits: number;
    readonly hints: number;
    readonly generatedBlocks: number;
    readonly generatedRoots: number;
    readonly generatedRecords: number;
    readonly lookupOrderRecords: number;
    readonly lookupOrderRoots: number;
    readonly lookupOrderBytes: number;
    readonly lookupOrderExceptionSurfaces: number;
    readonly lookupOrderExceptionClasses: number;
    readonly lookupOrderExceptionLocators: number;
    readonly lookupOrderExceptionBytes: number;
    readonly generatedPhysicalGroups: number;
    readonly generatedFactPairs: number;
    readonly indexBytes: number;
    readonly uncompressedBytes: number;
    readonly compressedBytes: number;
    readonly annotationUncompressedBytes: number;
    readonly annotationCompressedBytes: number;
    readonly generatedUncompressedBytes: number;
    readonly generatedCompressedBytes: number;
    readonly totalBytes: number;
    readonly largestUncompressedBlock: number;
    readonly largestGeneratedBlock: number;
    readonly largestGeneratedCompressedBlock: number;
  };
  readonly details: {
    readonly entries: number;
    readonly forms: number;
    readonly senses: number;
    readonly glosses: number;
    readonly properties: number;
  };
}

export function sha256Bytes(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

export function deterministicJson(value: unknown): Uint8Array {
  return new TextEncoder().encode(`${JSON.stringify(value, null, 2)}\n`);
}

export function assertBytesEqual(left: Uint8Array, right: Uint8Array, label: string): void {
  if (left.byteLength !== right.byteLength) {
    throw new Error(`${label} rebuild changed length (${left.byteLength} != ${right.byteLength})`);
  }
  for (let index = 0; index < left.byteLength; index++) {
    if (left[index] !== right[index]) throw new Error(`${label} rebuild differs at byte ${index}`);
  }
}
