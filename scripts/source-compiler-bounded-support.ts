#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { mkdir, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { buildAnalyzerAnnotations } from '../packages/data/src/browser-pack/analyzer-annotations.js';
import { buildMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import {
  compileBoundedSourceNativeAnalyzerSupport
} from '../packages/data/src/source-compiler/analyzer-support-stream.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  assertSourceCompatibilityConsumed,
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility,
  kanjidicCompatibility,
  physicalTargetOrderCompatibility
} from '../packages/data/src/source-compiler/compatibility.js';
import {
  chronologicalMorphologySource,
  foldChronologicalConjugationErrata
} from '../packages/data/src/source-compiler/conjugation-errata.js';
import {
  conjugationPositionsByRoot
} from '../packages/data/src/source-compiler/conjugation-emission-order.js';
import {
  writeScheduledGeneratedProjection
} from '../packages/data/src/source-compiler/generated-projection-stream.js';
import { loadKanjidicHintReadings } from '../packages/data/src/source-compiler/kanjidic-hints.js';
import { writeBoundedSurfaceIndexTsv } from '../packages/data/src/source-compiler/surface-index-spool.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

function argumentsOf(argv: readonly string[]): { readonly out: string; readonly work: string } {
  if (argv.length !== 4 || argv[0] !== '--out' || argv[2] !== '--work') {
    throw new Error('Usage: bun scripts/source-compiler-bounded-support.ts --out report.json --work empty-directory');
  }
  return { out: resolve(argv[1]!), work: resolve(argv[3]!) };
}

const options = argumentsOf(process.argv.slice(2));
const repository = resolve(import.meta.dir, '..');
const sourceLock = await verifySourceCompilerLock(repository);
const conjugationRules = {
  kwpos: sourceLock.inputs.kwpos.absolutePath,
  conjo: sourceLock.inputs.conjo.absolutePath
};
await mkdir(options.work);
const roots = await compileCanonicalRoots({
  jmdict: sourceLock.inputs.jmdict.absolutePath,
  jmdictSourceId: sourceLock.inputs.jmdict.id,
  extra: sourceLock.inputs.extra.absolutePath,
  municipality: sourceLock.inputs.municipality.absolutePath,
  ward: sourceLock.inputs.ward.absolutePath,
  errata: sourceLock.inputs.chronologicalErrata.absolutePath,
  compatibility: sourceLock.inputs.compatibility.absolutePath
});
const fold = foldChronologicalConjugationErrata(
  roots.entries, roots.errata.conjugationRows, { conjugationRules }
);
const morphologySource = chronologicalMorphologySource(
  roots.entries,
  roots.errata.conjugationRows,
  {
    conjugationRules,
    extraPositions: conjugationPositionCompatibility(roots.compatibility)
  }
);
const morphology = buildMorphology(morphologySource, { conjugationRules }).artifact;
const da = roots.errata.conjugationRows.find(value => value.operation === 'conjugateDa');
if (!da) throw new Error('Chronological errata has no conjugateDa declaration');
const customRootSeqs = new Set(roots.custom.createdRoots.map(entry => entry.seq));
const projection = writeScheduledGeneratedProjection({
  entries: roots.entries,
  positionsByRoot: conjugationPositionsByRoot(morphologySource),
  customRootSeqs,
  firstErrataEvent: roots.custom.nextEvent,
  chronologicalPositions: [{
    rootSeq: 2_089_020,
    pos: 'cop',
    event: roots.custom.nextEvent + da.event
  }],
  suppressions: fold.suppressions,
  regeneratedLineages: fold.regeneratedLineages,
  physicalTargetOrderCompatibility: physicalTargetOrderCompatibility(roots.compatibility),
  lineageCompatibility: conjugationReadingLineageCompatibility(roots.compatibility),
  morphology,
  firstGeneratedSeq: Math.max(...roots.entries.map(entry => entry.seq)) + 1,
  pathsPath: join(options.work, 'generated-paths.bin'),
  occurrencesPath: join(options.work, 'generated-occurrences.bin')
});
const compiled = compileBoundedSourceNativeAnalyzerSupport({
  projection,
  entries: roots.entries,
  morphology,
  temporaryDirectory: options.work,
  kanjidicReadings: await loadKanjidicHintReadings(
    sourceLock.inputs.kanjidic.absolutePath,
    kanjidicCompatibility(roots.compatibility)
  ),
  customRootSeqs,
  firstErrataEvent: roots.custom.nextEvent
});
const surface = await writeBoundedSurfaceIndexTsv({
  entries: roots.entries,
  physicalTargets: projection.targets,
  occurrencesPath: projection.occurrencesPath,
  temporaryDirectory: options.work,
  destination: join(options.work, 'surface.tsv')
});
const compatibilityUsage = assertSourceCompatibilityConsumed(roots.compatibility);
const generated = compiled.support.generated!;
const annotations = buildAnalyzerAnnotations(
  compiled.support.splits,
  compiled.support.hints,
  generated
);
const report = {
  formatVersion: 1,
  postgresUnavailable: true,
  canonicalEntries: roots.entries.length,
  compatibilityUsage,
  morphology: {
    positions: morphology.positions.length,
    rules: morphology.rules.length,
    patches: morphology.patches.length,
    tombstones: morphology.tombstones.length
  },
  spool: projection.spool,
  targets: {
    total: projection.targets.length,
    generated: projection.targets.reduce(
      (count, value) => count + Number(value.origin === 'generated'), 0
    )
  },
  phases: projection.phases,
  patches: projection.patches,
  support: compiled.summary,
  annotations: {
    bytes: annotations.bytes.byteLength,
    sha256: createHash('sha256').update(annotations.bytes).digest('hex'),
    counts: annotations.stats
  },
  surface,
  generated: {
    semanticPaths: generated.semanticPaths,
    matchedPaths: generated.matchedPaths,
    records: generated.records.length,
    countExceptions: generated.countExceptions,
    physicalGroups: generated.physicalGroups,
    physicalMembers: generated.physicalMembers,
    propertyOverrides: generated.propertyOverrides,
    maxMemberOrd: generated.maxMemberOrd,
    maxViaMemberOrd: generated.maxViaMemberOrd,
    maxPropOrd: generated.maxPropOrd,
    projectionSha256: generated.projectionSha256,
    lookupOrderRecords: generated.lookupOrders.length,
    lookupOrderSourceRows: generated.lookupOrderSourceRows,
    lookupOrderSourceSha256: generated.lookupOrderSourceSha256,
    lookupOrderSurfaces: generated.lookupOrderSurfaces,
    lookupOrderClasses: generated.lookupOrderClasses,
    lookupOrderExceptions: generated.lookupOrderExceptions.length,
    lookupOrderExceptionClasses: generated.lookupOrderExceptionClasses,
    lookupOrderExceptionLocators: generated.lookupOrderExceptionLocators,
    lookupOrderProjectionSha256: generated.lookupOrderProjectionSha256
  }
};
await writeFile(options.out, `${JSON.stringify(report, null, 2)}\n`, { flag: 'wx' });
process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
