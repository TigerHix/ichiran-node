#!/usr/bin/env bun

import { mkdir, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { buildMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility,
  physicalTargetOrderCompatibility
} from '../packages/data/src/source-compiler/compatibility.js';
import {
  chronologicalMorphologySource,
  foldChronologicalConjugationErrata
} from '../packages/data/src/source-compiler/conjugation-errata.js';
import { conjugationPositionsByRoot } from '../packages/data/src/source-compiler/conjugation-emission-order.js';
import { writeScheduledGeneratedProjection } from '../packages/data/src/source-compiler/generated-projection-stream.js';
import { writeBoundedSurfaceIndexTsv } from '../packages/data/src/source-compiler/surface-index-spool.js';

function argumentsOf(argv: readonly string[]): { readonly out: string; readonly work: string } {
  if (argv.length !== 4 || argv[0] !== '--out' || argv[2] !== '--work') {
    throw new Error('Usage: bun scripts/source-compiler-surface-probe.ts --out report.json --work empty-directory');
  }
  return { out: resolve(argv[1]!), work: resolve(argv[3]!) };
}

const options = argumentsOf(process.argv.slice(2));
const repository = resolve(import.meta.dir, '..');
const data = join(repository, 'data');
await mkdir(options.work);
const roots = await compileCanonicalRoots({
  jmdict: join(repository, 'packages/data/JMdict_e.gz'),
  extra: join(data, 'sources/extra.xml'),
  municipality: join(data, 'sources/jichitai.csv'),
  ward: join(data, 'sources/gyoseiku.csv'),
  errata: join(data, 'source-compiler-errata.json'),
  compatibility: join(data, 'source-compiler-compatibility.json')
});
const fold = foldChronologicalConjugationErrata(
  roots.entries, roots.errata.conjugationRows, { dataPath: data }
);
const morphologySource = chronologicalMorphologySource(
  roots.entries,
  roots.errata.conjugationRows,
  {
    dataPath: data,
    extraPositions: conjugationPositionCompatibility(roots.compatibility)
      .map(value => ({ seq: value.seq, pos: value.pos }))
  }
);
const morphology = buildMorphology(morphologySource, { dataPath: data }).artifact;
const da = roots.errata.conjugationRows.find(value => value.operation === 'conjugateDa');
if (!da) throw new Error('Chronological errata has no conjugateDa declaration');
const projection = writeScheduledGeneratedProjection({
  entries: roots.entries,
  positionsByRoot: conjugationPositionsByRoot(morphologySource),
  customRootSeqs: new Set(roots.custom.createdRoots.map(entry => entry.seq)),
  firstErrataEvent: roots.custom.nextEvent,
  chronologicalPositions: [{
    rootSeq: 2_089_020, pos: 'cop', event: roots.custom.nextEvent + da.event
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
const surface = await writeBoundedSurfaceIndexTsv({
  entries: roots.entries,
  physicalTargets: projection.targets,
  occurrencesPath: projection.occurrencesPath,
  temporaryDirectory: options.work,
  destination: join(options.work, 'surface.tsv')
});
const report = {
  formatVersion: 1,
  postgresUnavailable: true,
  spool: projection.spool,
  targets: projection.targets.length,
  generatedTargets: projection.targets.filter(value => value.origin === 'generated').length,
  phases: projection.phases,
  patches: projection.patches,
  surface
};
await writeFile(options.out, `${JSON.stringify(report, null, 2)}\n`, { flag: 'wx' });
process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
