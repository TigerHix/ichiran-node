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
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

function destination(argv: readonly string[]): string {
  if (argv.length !== 2 || argv[0] !== '--out') {
    throw new Error('Usage: bun scripts/source-compiler-configured-forward.ts --out relation.ndjson');
  }
  return resolve(argv[1]!);
}

const repository = resolve(import.meta.dir, '..');
const sourceLock = await verifySourceCompilerLock(repository);
const conjugationRules = {
  kwpos: sourceLock.inputs.kwpos.absolutePath,
  conjo: sourceLock.inputs.conjo.absolutePath
};
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
  roots.entries,
  roots.errata.conjugationRows,
  { conjugationRules }
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
const result = await writeConfiguredConjugationRelation({
  entries: roots.entries,
  positionsByRoot: conjugationPositionsByRoot(morphologySource),
  suppressions: fold.suppressions,
  lineageCompatibility: conjugationReadingLineageCompatibility(roots.compatibility),
  morphology
}, destination(process.argv.slice(2)));
console.log(JSON.stringify(result, null, 2));
