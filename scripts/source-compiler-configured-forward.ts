#!/usr/bin/env bun

import { resolve } from 'node:path';
import { buildMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility
} from '../packages/data/src/source-compiler/compatibility.js';
import {
  foldChronologicalConjugationErrata,
  chronologicalMorphologySource
} from '../packages/data/src/source-compiler/conjugation-errata.js';
import { conjugationPositionsByRoot } from '../packages/data/src/source-compiler/conjugation-emission-order.js';
import { writeConfiguredConjugationRelation } from '../packages/data/src/source-compiler/configured-conjugation-relation.js';

function destination(argv: readonly string[]): string {
  if (argv.length !== 2 || argv[0] !== '--out') {
    throw new Error('Usage: bun scripts/source-compiler-configured-forward.ts --out relation.ndjson');
  }
  return resolve(argv[1]!);
}

const repository = resolve(import.meta.dir, '..');
const data = resolve(repository, 'data');
const roots = await compileCanonicalRoots({
  jmdict: resolve(repository, 'packages/data/JMdict_e.gz'),
  extra: resolve(data, 'sources/extra.xml'),
  municipality: resolve(data, 'sources/jichitai.csv'),
  ward: resolve(data, 'sources/gyoseiku.csv'),
  errata: resolve(data, 'source-compiler-errata.json'),
  compatibility: resolve(data, 'source-compiler-compatibility.json')
});
const fold = foldChronologicalConjugationErrata(
  roots.entries,
  roots.errata.conjugationRows,
  { dataPath: data }
);
const morphologySource = chronologicalMorphologySource(
  roots.entries,
  roots.errata.conjugationRows,
  {
    dataPath: data,
    extraPositions: conjugationPositionCompatibility(roots.compatibility)
      .map(row => ({ seq: row.seq, pos: row.pos }))
  }
);
const morphology = buildMorphology(morphologySource, { dataPath: data }).artifact;
const result = await writeConfiguredConjugationRelation({
  entries: roots.entries,
  positionsByRoot: conjugationPositionsByRoot(morphologySource),
  suppressions: fold.suppressions,
  lineageCompatibility: conjugationReadingLineageCompatibility(roots.compatibility),
  morphology
}, destination(process.argv.slice(2)));
console.log(JSON.stringify(result, null, 2));
