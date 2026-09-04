#!/usr/bin/env bun

import { execFile as execFileCallback } from 'node:child_process';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { promisify } from 'node:util';

import { buildMorphology } from '../browser-pack/morphology-compiler.js';
import { compileBoundedSourceNativeAnalyzerSupport } from './analyzer-support-stream.js';
import { compileCanonicalRoots } from './canonical-roots.js';
import {
  assertSourceCompatibilityConsumed,
  conjugationPositionCompatibility,
  conjugationReadingLineageCompatibility,
  kanjidicCompatibility,
  physicalTargetOrderCompatibility
} from './compatibility.js';
import {
  chronologicalMorphologySource,
  foldChronologicalConjugationErrata
} from './conjugation-errata.js';
import { conjugationPositionsByRoot } from './conjugation-emission-order.js';
import { writeScheduledGeneratedProjection } from './generated-projection-stream.js';
import { loadKanjidicHintReadings } from './kanjidic-hints.js';
import { writeSourceCompilerRelease } from './release-output.js';
import { resolveSourceReleaseDestination } from './release-path.js';
import { writeBoundedSurfaceIndexTsv } from './surface-index-spool.js';
import {
  assertSourceCompilerReleaseMode,
  verifySourceCompilerLock
} from './source-lock.js';
import { loadTomoshiZhHans } from './tomoshi-zh-hans.js';
import { loadZhHansSenseInfo } from './zh-hans-sense-info.js';

const execFile = promisify(execFileCallback);
const RELEASE_TEMP_ROOT = process.platform === 'win32' ? tmpdir() : '/tmp';
const BASELINE_SOURCE_LOCK = 'data/source-compiler-sources.lock.json';

export {
  assertSourceReleaseDestination,
  resolveSourceReleaseDestination,
  resolveSourceReleaseOutput
} from './release-path.js';

interface Options {
  readonly mode: 'baseline' | 'update';
  readonly output: string;
  readonly packVersion: string;
  readonly sourceLock: string;
  readonly surfaceChunkRows?: number;
}

const USAGE = `usage:
  ichiran-data baseline --out <directory> --pack-version <version>
  ichiran-data update --out <directory> --pack-version <version> \\
    --source-lock <repo-relative-file>`;

function usage(message?: string): never {
  const prefix = message ? `error: ${message}\n\n` : '';
  throw new Error(`${prefix}${USAGE}`);
}

function parseArguments(argv: readonly string[]): Options {
  const mode = argv[0];
  if (mode !== 'baseline' && mode !== 'update') usage('first argument must be baseline or update');
  let output: string | undefined;
  let packVersion: string | undefined;
  let sourceLock: string | undefined;
  let surfaceChunkRows: number | undefined;
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
    else if (argument === '--surface-chunk-rows') {
      surfaceChunkRows = Number(next());
      if (!Number.isSafeInteger(surfaceChunkRows) || surfaceChunkRows <= 0) {
        usage('--surface-chunk-rows must be a positive integer');
      }
    } else if (argument === '--help' || argument === '-h') usage();
    else usage(`unknown argument ${argument}`);
  }
  if (!output) usage('--out is required');
  if (!packVersion) usage('--pack-version is required');
  if (mode === 'baseline' && sourceLock !== undefined) {
    usage(`baseline uses ${BASELINE_SOURCE_LOCK} and does not accept --source-lock`);
  }
  if (mode === 'update' && !sourceLock) usage('update requires --source-lock');
  return {
    mode,
    output,
    packVersion,
    sourceLock: sourceLock ?? BASELINE_SOURCE_LOCK,
    ...(surfaceChunkRows === undefined ? {} : { surfaceChunkRows })
  };
}

async function gitOutput(repository: string, args: readonly string[]): Promise<string> {
  const result = await execFile('git', ['-C', repository, ...args], { encoding: 'utf8' });
  return result.stdout.trim();
}

async function assertClean(repository: string): Promise<void> {
  const status = await gitOutput(repository, ['status', '--porcelain=v1', '--untracked-files=all']);
  if (status.length !== 0) {
    throw new Error('Source checkout is dirty; commit it before building a release');
  }
}

export async function runSourceCompilerRelease(argv: readonly string[]): Promise<void> {
  if (argv.length === 1 && (argv[0] === '--help' || argv[0] === '-h')) {
    process.stdout.write(`${USAGE}\n`);
    return;
  }
  const options = parseArguments(argv);
  const repository = await gitOutput(import.meta.dir, ['rev-parse', '--show-toplevel']);
  const sourceCommit = await gitOutput(repository, ['rev-parse', 'HEAD']);
  if (!/^[0-9a-f]{40}$/.test(sourceCommit)) throw new Error('Git returned an invalid source commit');
  const expectedCommit = process.env.ICHIRAN_SOURCE_COMPILER_COMMIT;
  if (!expectedCommit || !/^[0-9a-f]{40}$/.test(expectedCommit)) {
    throw new Error('Source compiler must be launched with its clean build commit');
  }
  if (sourceCommit !== expectedCommit) {
    throw new Error(`Source compiler launch commit ${expectedCommit} does not match HEAD ${sourceCommit}`);
  }
  const output = await resolveSourceReleaseDestination(repository, options.output);
  await assertClean(repository);

  const temporaryDirectory = await mkdtemp(join(RELEASE_TEMP_ROOT, 'ichiran-source-release-'));
  try {
    const lock = await verifySourceCompilerLock(
      repository,
      options.sourceLock,
      join(temporaryDirectory, 'locked-inputs')
    );
    assertSourceCompilerReleaseMode(options.mode, lock.inputs.jmdict);
    const jmdictRelative = lock.inputs.jmdict.path;
    const jmdictSourceId = lock.inputs.jmdict.id;
    const conjugationRules = {
      kwpos: lock.inputs.kwpos.absolutePath,
      conjo: lock.inputs.conjo.absolutePath
    };

    const data = join(repository, 'data');
    const roots = await compileCanonicalRoots({
      jmdict: lock.inputs.jmdict.absolutePath,
      jmdictSourceId,
      extra: lock.inputs.extra.absolutePath,
      municipality: lock.inputs.municipality.absolutePath,
      ward: lock.inputs.ward.absolutePath,
      errata: lock.inputs.chronologicalErrata.absolutePath,
      compatibility: lock.inputs.compatibility.absolutePath
    });
    const tomoshiSource = lock.lock.sources.find(item => item.kind === 'tomoshi-dict');
    if (!tomoshiSource) throw new Error('Verified source lock has no Tomoshi dictionary source');
    const tomoshiZhHans = loadTomoshiZhHans(
      lock.inputs.tomoshiZhHans.absolutePath,
      roots.entries,
      {
        exportVersion: tomoshiSource.exportVersion,
        sourceSchemaVersion: tomoshiSource.sourceSchemaVersion,
        exportedAt: tomoshiSource.exportedAt
      }
    );
    const zhHans = await loadZhHansSenseInfo(
      lock.inputs.zhHansSenseInfo.absolutePath,
      roots.entries,
      tomoshiZhHans.entries
    );
    const fold = foldChronologicalConjugationErrata(
      roots.entries,
      roots.errata.conjugationRows,
      { conjugationRules }
    );
    const extraPositions = conjugationPositionCompatibility(roots.compatibility);
    const morphologySource = chronologicalMorphologySource(
      roots.entries,
      roots.errata.conjugationRows,
      { conjugationRules, extraPositions }
    );
    const morphology = buildMorphology(morphologySource, { conjugationRules }).artifact;
    const da = roots.errata.conjugationRows.find(value => value.operation === 'conjugateDa');
    if (!da) throw new Error('Chronological errata has no conjugateDa declaration');
    const firstGeneratedSeq = roots.entries.reduce(
      (maximum, entry) => Math.max(maximum, entry.seq),
      0
    ) + 1;
    const customRootSeqs = new Set(roots.custom.createdRoots.map(entry => entry.seq));
    const kanjidicReadings = await loadKanjidicHintReadings(
      lock.inputs.kanjidic.absolutePath,
      kanjidicCompatibility(roots.compatibility)
    );
    const targetPhase = await (async function compileTargetPhase() {
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
      Bun.gc(true);
      const bounded = await compileBoundedSourceNativeAnalyzerSupport({
        projection,
        entries: roots.entries,
        morphology,
        temporaryDirectory,
        kanjidicReadings,
        customRootSeqs,
        firstErrataEvent: roots.custom.nextEvent
      });
      const surfaceTsv = join(temporaryDirectory, 'surface.tsv');
      const surfaceSpool = await writeBoundedSurfaceIndexTsv({
        entries: roots.entries,
        physicalTargets: projection.targets,
        occurrencesPath: projection.occurrencesPath,
        temporaryDirectory,
        destination: surfaceTsv,
        ...(options.surfaceChunkRows === undefined ? {} : {
          maxChunkRows: options.surfaceChunkRows
        })
      });
      return {
        support: bounded.support,
        surfaceTsv,
        surfaceSpool,
        projectionSummary: {
          spool: projection.spool,
          targets: projection.targets.length,
          generatedTargets: projection.targets.reduce(
            (count, value) => count + Number(value.origin === 'generated'), 0
          ),
          ruleAliases: projection.ruleAliases.length,
          aliasProperties: projection.aliasProperties.length,
          phases: projection.phases,
          patches: projection.patches,
          analyzerSupport: bounded.summary
        }
      };
    })();
    const compatibilityUsage = assertSourceCompatibilityConsumed(roots.compatibility);
    // The target graph is owned only by compileTargetPhase and is now unreachable.
    Bun.gc(true);
    const release = await writeSourceCompilerRelease({
      repository,
      output: output.lexical,
      outputPhysical: output.physical,
      temporaryDirectory,
      sourceCommit,
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
      zhHans: zhHans.entries,
      morphology: morphologySource,
      support: targetPhase.support,
      surfaceTsv: targetPhase.surfaceTsv,
      surfaceSpool: targetPhase.surfaceSpool,
      conjugationRules,
      sourceSummary: {
        mode: options.mode,
        jmdict: { id: jmdictSourceId, path: jmdictRelative },
        tomoshi: {
          id: tomoshiSource.id,
          path: lock.inputs.tomoshiZhHans.path,
          locale: 'zh-Hans',
          projection: tomoshiZhHans.stats
        },
        zhHansSenseInfo: {
          id: lock.inputs.zhHansSenseInfo.id,
          path: lock.inputs.zhHansSenseInfo.path,
          locale: 'zh-Hans',
          projection: zhHans.stats
        },
        canonicalEntries: roots.entries.length,
        jmdictEntries: roots.jmdictEntries,
        customCreatedRoots: roots.custom.createdRoots.length,
        chronologicalErrataRows: roots.errata.counts.declared,
        conjugationErrataRows: roots.errata.conjugationRows.length,
        errataNoopRowIds: roots.errata.noopRowIds,
        compatibilityRows: roots.compatibility.rows.length,
        compatibilityUsage
      },
      projectionSummary: targetPhase.projectionSummary
    });
    process.stdout.write(`${JSON.stringify({
      generation: release.generation,
      statsBytes: release.report.byteLength,
      artifacts: release.counts
    }, null, 2)}\n`);
  } finally {
    await rm(temporaryDirectory, { recursive: true, force: true });
  }
}
