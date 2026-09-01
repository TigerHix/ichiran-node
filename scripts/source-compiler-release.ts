#!/usr/bin/env bun

import { execFile as execFileCallback } from 'node:child_process';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, relative, resolve, sep } from 'node:path';
import { promisify } from 'node:util';

import { buildMorphology } from '../packages/data/src/browser-pack/morphology-compiler.js';
import { compileBoundedSourceNativeAnalyzerSupport } from '../packages/data/src/source-compiler/analyzer-support-stream.js';
import { compileCanonicalRoots, QUALIFIED_JMDICT_SOURCE_ID } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility,
  kanjidicCompatibility,
  physicalTargetOrderCompatibility
} from '../packages/data/src/source-compiler/compatibility.js';
import {
  chronologicalMorphologySource,
  foldChronologicalConjugationErrata
} from '../packages/data/src/source-compiler/conjugation-errata.js';
import { conjugationPositionsByRoot } from '../packages/data/src/source-compiler/conjugation-emission-order.js';
import { writeScheduledGeneratedProjection } from '../packages/data/src/source-compiler/generated-projection-stream.js';
import { loadKanjidicHintReadings } from '../packages/data/src/source-compiler/kanjidic-hints.js';
import { writeSourceCompilerRelease } from '../packages/data/src/source-compiler/release-output.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

const execFile = promisify(execFileCallback);
const RELEASE_TEMP_ROOT = process.platform === 'win32' ? tmpdir() : '/tmp';
const BASELINE_SOURCE_LOCK = 'data/source-compiler-sources.lock.json';
const BASELINE_JMDICT = 'packages/data/JMdict_e.gz';

interface Options {
  readonly mode: 'baseline' | 'update';
  readonly output: string;
  readonly packVersion: string;
  readonly sourceLock: string;
  readonly jmdict?: string;
  readonly jmdictSourceId?: string;
  readonly surfaceChunkRows?: number;
  readonly allowDirty: boolean;
}

function usage(message?: string): never {
  const prefix = message ? `error: ${message}\n\n` : '';
  throw new Error(`${prefix}usage:
  bun scripts/source-compiler-release.ts baseline --out <directory> --pack-version <version> [--allow-dirty]
  bun scripts/source-compiler-release.ts update --out <directory> --pack-version <version> \\
    --jmdict <repo-relative-file> --jmdict-source-id <identity> \\
    --source-lock <repo-relative-file> [--allow-dirty]`);
}

function parseArguments(argv: readonly string[]): Options {
  const mode = argv[0];
  if (mode !== 'baseline' && mode !== 'update') usage('first argument must be baseline or update');
  let output: string | undefined;
  let packVersion: string | undefined;
  let sourceLock: string | undefined;
  let jmdict: string | undefined;
  let jmdictSourceId: string | undefined;
  let surfaceChunkRows: number | undefined;
  let allowDirty = false;
  for (let index = 1; index < argv.length; index++) {
    const argument = argv[index]!;
    const next = (): string => {
      const value = argv[++index];
      if (!value) usage(`${argument} requires a value`);
      return value;
    };
    if (argument === '--out') output = next();
    else if (argument === '--pack-version') packVersion = next();
    else if (argument === '--source-lock') sourceLock = next();
    else if (argument === '--jmdict') jmdict = next();
    else if (argument === '--jmdict-source-id') jmdictSourceId = next();
    else if (argument === '--surface-chunk-rows') {
      surfaceChunkRows = Number(next());
      if (!Number.isSafeInteger(surfaceChunkRows) || surfaceChunkRows <= 0) {
        usage('--surface-chunk-rows must be a positive integer');
      }
    } else if (argument === '--allow-dirty') allowDirty = true;
    else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (!output) usage('--out is required');
  if (!packVersion) usage('--pack-version is required');
  if (mode === 'baseline' && (jmdict !== undefined || jmdictSourceId !== undefined)) {
    usage('--jmdict and --jmdict-source-id apply only to update');
  }
  if (mode === 'update' && (!jmdict || !jmdictSourceId)) {
    usage('update requires --jmdict and --jmdict-source-id');
  }
  if (mode === 'update' && !sourceLock) usage('update requires --source-lock');
  if (mode === 'update' && (jmdict === BASELINE_JMDICT
    || jmdictSourceId === QUALIFIED_JMDICT_SOURCE_ID)) {
    usage('update requires a non-baseline JMdict path and source identity');
  }
  return {
    mode,
    output,
    packVersion,
    sourceLock: sourceLock ?? BASELINE_SOURCE_LOCK,
    ...(jmdict === undefined ? {} : { jmdict }),
    ...(jmdictSourceId === undefined ? {} : { jmdictSourceId }),
    ...(surfaceChunkRows === undefined ? {} : { surfaceChunkRows }),
    allowDirty
  };
}

function repositoryPath(repository: string, value: string, label: string): string {
  if (value.includes('\\')) throw new Error(`${label} must use portable forward slashes`);
  const path = resolve(repository, value);
  const within = relative(repository, path);
  if (within === '' || within === '..' || within.startsWith(`..${sep}`)) {
    throw new Error(`${label} must be below the repository root`);
  }
  return path;
}

async function gitOutput(repository: string, args: readonly string[]): Promise<string> {
  const result = await execFile('git', ['-C', repository, ...args], { encoding: 'utf8' });
  return result.stdout.trim();
}

async function assertClean(repository: string, allowDirty: boolean): Promise<void> {
  if (allowDirty) return;
  const status = await gitOutput(repository, ['status', '--porcelain=v1', '--untracked-files=all']);
  if (status.length !== 0) {
    throw new Error('Source checkout is dirty; commit it or use --allow-dirty for development');
  }
}

const options = parseArguments(process.argv.slice(2));
const repository = await gitOutput(import.meta.dir, ['rev-parse', '--show-toplevel']);
await assertClean(repository, options.allowDirty);
const sourceCommit = await gitOutput(repository, ['rev-parse', 'HEAD']);
if (!/^[0-9a-f]{40}$/.test(sourceCommit)) throw new Error('Git returned an invalid source commit');

const lock = await verifySourceCompilerLock(repository, options.sourceLock);
const jmdictRelative = options.jmdict ?? BASELINE_JMDICT;
const jmdictSourceId = options.jmdictSourceId ?? QUALIFIED_JMDICT_SOURCE_ID;
const jmdict = repositoryPath(repository, jmdictRelative, 'JMdict path');
if (!lock.files.some(file => file.id === jmdictSourceId && resolve(repository, file.path) === jmdict)) {
  throw new Error(`Verified source lock does not pin ${jmdictSourceId} at ${jmdictRelative}`);
}

const data = join(repository, 'data');
const roots = await compileCanonicalRoots({
  jmdict,
  jmdictSourceId,
  extra: join(data, 'sources/extra.xml'),
  municipality: join(data, 'sources/jichitai.csv'),
  ward: join(data, 'sources/gyoseiku.csv'),
  errata: join(data, 'source-compiler-errata.json'),
  compatibility: join(data, 'source-compiler-compatibility.json')
});
const fold = foldChronologicalConjugationErrata(
  roots.entries,
  roots.errata.conjugationRows,
  { dataPath: data }
);
const extraPositions = conjugationPositionCompatibility(roots.compatibility)
  .map(value => ({ seq: value.seq, pos: value.pos }));
const morphologySource = chronologicalMorphologySource(
  roots.entries,
  roots.errata.conjugationRows,
  { dataPath: data, extraPositions }
);
const morphology = buildMorphology(morphologySource, { dataPath: data }).artifact;
const da = roots.errata.conjugationRows.find(value => value.operation === 'conjugateDa');
if (!da) throw new Error('Chronological errata has no conjugateDa declaration');
const firstGeneratedSeq = roots.entries.reduce(
  (maximum, entry) => Math.max(maximum, entry.seq),
  0
) + 1;
const customRootSeqs = new Set(roots.custom.createdRoots.map(entry => entry.seq));
const kanjidicReadings = await loadKanjidicHintReadings(
  join(repository, 'packages/data/kanjidic2.xml.gz'),
  kanjidicCompatibility(roots.compatibility)
);
const temporaryDirectory = await mkdtemp(join(RELEASE_TEMP_ROOT, 'ichiran-source-release-'));
try {
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
    firstGeneratedSeq,
    pathsPath: join(temporaryDirectory, 'generated-paths.bin'),
    occurrencesPath: join(temporaryDirectory, 'generated-occurrences.bin')
  });
  const bounded = await compileBoundedSourceNativeAnalyzerSupport({
    projection,
    entries: roots.entries,
    morphology,
    temporaryDirectory,
    kanjidicReadings,
    customRootSeqs,
    firstErrataEvent: roots.custom.nextEvent
  });
  const release = await writeSourceCompilerRelease({
    repository,
    output: repositoryPath(repository, options.output, 'Release output'),
    temporaryDirectory,
    sourceCommit,
    requireCleanSource: !options.allowDirty,
    packVersion: options.packVersion,
    sourceLock: lock,
    ...(options.mode === 'baseline' ? {
      baseline: {
        directory: join(repository, 'work/m2-baseline'),
        directOrderAttestationPath: join(data, 'source-compiler-direct-order-attestation.json'),
        generatedOrderAttestationPath: join(
          data,
          'source-compiler-generated-order-attestation.json'
        )
      }
    } : {}),
    entries: roots.entries,
    morphology: morphologySource,
    support: bounded.support,
    occurrencesPath: projection.occurrencesPath,
    physicalTargets: projection.targets,
    dataPath: data,
    ...(options.surfaceChunkRows === undefined ? {} : {
      surfaceChunkRows: options.surfaceChunkRows
    }),
    sourceSummary: {
      mode: options.mode,
      jmdict: { id: jmdictSourceId, path: jmdictRelative },
      canonicalEntries: roots.entries.length,
      jmdictEntries: roots.jmdictEntries,
      customCreatedRoots: roots.custom.createdRoots.length,
      errataRows: roots.errata.conjugationRows.length,
      compatibilityRows: roots.compatibility.rows.length
    },
    projectionSummary: {
      spool: projection.spool,
      targets: projection.targets.length,
      generatedTargets: projection.targets.filter(value => value.origin === 'generated').length,
      ruleAliases: projection.ruleAliases.length,
      aliasProperties: projection.aliasProperties.length,
      phases: projection.phases,
      patches: projection.patches,
      analyzerSupport: bounded.summary
    }
  });
  process.stdout.write(`${JSON.stringify({
    generation: release.generation,
    statsBytes: release.report.byteLength,
    artifacts: release.counts
  }, null, 2)}\n`);
} finally {
  await rm(temporaryDirectory, { recursive: true, force: true });
}
