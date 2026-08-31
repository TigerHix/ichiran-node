import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { gunzipSync } from 'node:zlib';

import { assertExactReleaseInventory } from '../packages/data/src/browser-pack/release-publication.js';
import {
  assertAnalyzerReleaseSize,
  parseAnalyzerReleaseManifest,
  type AnalyzerReleaseBuild
} from '../packages/data/src/browser-pack/release-manifest.js';
import {
  assertExactCount,
  sha256Bytes,
  type BrowserAlphaArtifactCounts
} from '../packages/data/src/browser-pack/release-orchestration.js';
import {
  ANALYZER_ANNOTATIONS_SECTION_ID,
  ANALYZER_LOOKUP_ORDER_RECORD_BYTES,
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../packages/core/src/analyzer-annotations.js';
import { ANALYZER_SUPPORT_SECTION_ID, openAnalyzerSupport } from '../packages/core/src/analyzer-support.js';
import { memoryDetailSource, openDetailStore } from '../packages/core/src/details.js';
import { MORPHOLOGY_SECTION_ID, openMorphology } from '../packages/core/src/morphology.js';
import { openPack } from '../packages/core/src/pack.js';
import { ROOT_PAYLOAD_SECTION_ID, openRootPayload } from '../packages/core/src/root-payload.js';
import { SURFACE_INDEX_SECTION_ID, openSurfaceIndex } from '../packages/core/src/surface-index.js';

export interface VerifiedAnalyzerRelease extends AnalyzerReleaseBuild {
  readonly inspection: {
    readonly artifacts: BrowserAlphaArtifactCounts;
    readonly sections: readonly {
      readonly id: number;
      readonly bytes: number;
      readonly sha256: string;
    }[];
    readonly details: { readonly bytes: number; readonly sha256: string };
  };
}

async function gzipDecode(bytes: Uint8Array): Promise<Uint8Array> {
  return new Uint8Array(gunzipSync(bytes));
}

export async function verifyRelease(
  out: string,
  shellBytes: number
): Promise<VerifiedAnalyzerRelease> {
  const manifestBytes = new Uint8Array(await readFile(join(out, 'manifest.json')));
  const manifest = parseAnalyzerReleaseManifest(
    JSON.parse(new TextDecoder().decode(manifestBytes)),
    text => sha256Bytes(new TextEncoder().encode(text))
  );
  await assertExactReleaseInventory(out, [
    manifest.hot.file,
    manifest.details.file,
    'manifest.json',
    'stats.json'
  ]);
  const hotDownload = new Uint8Array(await readFile(join(out, manifest.hot.file)));
  const detailsDownload = new Uint8Array(await readFile(join(out, manifest.details.file)));
  for (const [label, asset, download] of [
    ['hot', manifest.hot, hotDownload],
    ['details', manifest.details, detailsDownload]
  ] as const) {
    assertExactCount(download.byteLength, asset.downloadBytes, `${label} download bytes`);
    if (sha256Bytes(download) !== asset.downloadSha256) throw new Error(`${label} download digest mismatch`);
  }
  const hot = manifest.hot.encoding === 'gzip' ? await gzipDecode(hotDownload) : hotDownload.slice();
  const details = manifest.details.encoding === 'gzip'
    ? await gzipDecode(detailsDownload)
    : detailsDownload.slice();
  for (const [label, asset, installed] of [
    ['hot', manifest.hot, hot], ['details', manifest.details, details]
  ] as const) {
    assertExactCount(installed.byteLength, asset.installedBytes, `${label} installed bytes`);
    if (sha256Bytes(installed) !== asset.installedSha256) throw new Error(`${label} installed digest mismatch`);
  }

  const pack = openPack(hot);
  pack.verifyAll();
  const ids = pack.manifest.sections.map(section => section.id);
  if (ids.join(',') !== '1,2,3,4,5') throw new Error(`Hot pack sections must be 1,2,3,4,5; found ${ids}`);
  const surface = openSurfaceIndex(pack.getSection(SURFACE_INDEX_SECTION_ID));
  const root = openRootPayload(pack.getSection(ROOT_PAYLOAD_SECTION_ID));
  const morphology = openMorphology(pack.getSection(MORPHOLOGY_SECTION_ID));
  const support = openAnalyzerSupport(pack.getSection(ANALYZER_SUPPORT_SECTION_ID));
  const annotations = await AnalyzerAnnotationsReader.open(
    analyzerAnnotationsMemorySource(pack.getSection(ANALYZER_ANNOTATIONS_SECTION_ID)),
    gzipDecode
  );
  const detailReader = await openDetailStore(memoryDetailSource(details), gzipDecode);
  let detailForms = 0;
  let detailSenses = 0;
  let detailGlosses = 0;
  let detailProperties = 0;
  for (let index = 0; index < detailReader.manifest.entryCount; index++) {
    const entry = await detailReader.entry(index);
    detailForms += entry.forms.length;
    detailSenses += entry.senses.length;
    for (const sense of entry.senses) {
      detailGlosses += sense.glosses.length;
      detailProperties += sense.properties.length;
    }
  }
  const release: VerifiedAnalyzerRelease = {
    manifest,
    manifestBytes,
    hotDownload,
    detailsDownload,
    inspection: {
      artifacts: {
        surfaceIndex: {
          input: surface.manifest.inputCount,
          accepted: surface.manifest.acceptedCount,
          direct: surface.manifest.directCount,
          morphology: surface.manifest.morphologyCount,
          overlap: surface.manifest.overlapCount,
          omitted: surface.manifest.inputCount - surface.manifest.acceptedCount,
          states: surface.manifest.stateCount,
          edges: surface.manifest.edgeCount
        },
        rootPayload: {
          surfaces: root.surfaceCount,
          forms: root.formCount,
          entries: root.entryCount,
          restrictions: root.restrictionCount
        },
        morphology: { ...morphology.stats },
        analyzerSupport: { ...support.stats },
        annotations: {
          blocks: annotations.manifest.blocks,
          splits: annotations.manifest.splits,
          hints: annotations.manifest.hints,
          generatedBlocks: annotations.manifest.generatedBlocks,
          generatedRoots: annotations.manifest.generatedRoots,
          generatedRecords: annotations.manifest.generatedRecords,
          lookupOrderRecords: annotations.manifest.lookupOrderRecords,
          lookupOrderRoots: annotations.manifest.lookupOrderRoots,
          lookupOrderBytes: annotations.manifest.lookupOrderRecords * ANALYZER_LOOKUP_ORDER_RECORD_BYTES,
          lookupOrderExceptionSurfaces: annotations.manifest.lookupOrderExceptionSurfaces,
          lookupOrderExceptionClasses: annotations.manifest.lookupOrderExceptionClasses,
          lookupOrderExceptionLocators: annotations.manifest.lookupOrderExceptionLocators,
          lookupOrderExceptionBytes: annotations.manifest.lookupOrderExceptionBytes,
          generatedPhysicalGroups: annotations.manifest.generatedPhysicalGroups,
          generatedFactPairs: annotations.manifest.generatedFactPairs,
          indexBytes: annotations.manifest.residentIndexBytes,
          uncompressedBytes: annotations.manifest.uncompressedBytes,
          compressedBytes: annotations.manifest.compressedBytes,
          annotationUncompressedBytes: annotations.manifest.uncompressedBytes
            - annotations.manifest.generatedUncompressedBytes,
          annotationCompressedBytes: annotations.manifest.compressedBytes
            - annotations.manifest.generatedCompressedBytes,
          generatedUncompressedBytes: annotations.manifest.generatedUncompressedBytes,
          generatedCompressedBytes: annotations.manifest.generatedCompressedBytes,
          totalBytes: annotations.manifest.byteLength,
          largestUncompressedBlock: annotations.manifest.largestUncompressedBlock,
          largestGeneratedBlock: annotations.manifest.largestGeneratedBlock,
          largestGeneratedCompressedBlock: annotations.manifest.largestGeneratedCompressedBlock
        },
        details: {
          entries: detailReader.manifest.entryCount,
          forms: detailForms,
          senses: detailSenses,
          glosses: detailGlosses,
          properties: detailProperties
        }
      },
      sections: pack.manifest.sections.map(section => {
        const bytes = pack.getSection(section.id);
        return { id: section.id, bytes: bytes.byteLength, sha256: sha256Bytes(bytes) };
      }),
      details: { bytes: details.byteLength, sha256: sha256Bytes(details) }
    }
  };
  assertAnalyzerReleaseSize(release, shellBytes);
  return release;
}
