#!/usr/bin/env bun

import { createHash, randomUUID } from 'node:crypto';
import { mkdir, readFile, rename, unlink, writeFile } from 'node:fs/promises';
import { dirname, relative, resolve } from 'node:path';

import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';
import {
  assertCodexCandidateArtifactPath,
  emptyZhHansSenseInfoReviewProvenance,
  mergeZhHansSenseInfoReviews,
  parseZhHansSenseInfoCandidateArtifact,
  parseZhHansSenseInfoReviewArtifact,
  parseZhHansSenseInfoReviewProvenance
} from '../packages/data/src/source-compiler/zh-hans-sense-info-review.js';
import { parseZhHansSenseInfoCatalog } from '../packages/data/src/source-compiler/zh-hans-sense-info.js';

interface Options {
  readonly sourceLock: string;
  readonly candidates: string;
  readonly decisions: string;
  readonly provenance?: string;
  readonly outCatalog: string;
  readonly outProvenance: string;
}

const USAGE = 'usage: bun scripts/zh-hans-sense-info-review-merge.ts '
  + '--candidates <codex-candidates.json> --decisions <review-decisions.json> '
  + '[--provenance <existing-provenance.json>] '
  + '--out-catalog <catalog.json> --out-provenance <provenance.json> '
  + '[--source-lock <repository-relative-lock.json>]';

function parseOptions(args: readonly string[]): Options {
  const values = new Map<string, string>();
  for (let index = 0; index < args.length; index += 2) {
    const name = args[index];
    const value = args[index + 1];
    if (!name?.startsWith('--') || !value || value.startsWith('--')) throw new Error(USAGE);
    if (![
      '--source-lock', '--candidates', '--decisions', '--provenance',
      '--out-catalog', '--out-provenance'
    ].includes(name)) throw new Error(`Unknown option ${name}\n${USAGE}`);
    if (values.has(name)) throw new Error(`Duplicate option ${name}`);
    values.set(name, value);
  }
  const candidates = values.get('--candidates');
  const decisions = values.get('--decisions');
  const outCatalog = values.get('--out-catalog');
  const outProvenance = values.get('--out-provenance');
  if (!candidates || !decisions || !outCatalog || !outProvenance) throw new Error(USAGE);
  return {
    sourceLock: values.get('--source-lock') ?? 'data/source-compiler-update-2026-09-02.lock.json',
    candidates,
    decisions,
    ...(values.has('--provenance') ? { provenance: values.get('--provenance')! } : {}),
    outCatalog,
    outProvenance
  };
}

function parseJson(bytes: Uint8Array, label: string): unknown {
  try {
    return JSON.parse(new TextDecoder().decode(bytes));
  } catch {
    throw new Error(`${label} is not valid JSON`);
  }
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function readOptionalProvenance(path: string | undefined) {
  if (path === undefined) return emptyZhHansSenseInfoReviewProvenance();
  try {
    const bytes = new Uint8Array(await readFile(path));
    return parseZhHansSenseInfoReviewProvenance(parseJson(bytes, `Provenance ${path}`));
  } catch (error) {
    if (error && typeof error === 'object' && 'code' in error && error.code === 'ENOENT') {
      return emptyZhHansSenseInfoReviewProvenance();
    }
    throw error;
  }
}

async function stageJson(path: string, value: unknown): Promise<string> {
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.tmp-${randomUUID()}`;
  await writeFile(temporary, `${JSON.stringify(value, null, 2)}\n`, { flag: 'wx' });
  return temporary;
}

const repository = resolve(import.meta.dir, '..');
const options = parseOptions(process.argv.slice(2));
const candidatePath = resolve(process.cwd(), options.candidates);
const decisionPath = resolve(process.cwd(), options.decisions);
const provenancePath = options.provenance
  ? resolve(process.cwd(), options.provenance)
  : undefined;
const outCatalog = resolve(process.cwd(), options.outCatalog);
const outProvenance = resolve(process.cwd(), options.outProvenance);
assertCodexCandidateArtifactPath(candidatePath);
if (outCatalog === outProvenance) throw new Error('Catalog and provenance outputs must differ');
if (outCatalog === candidatePath || outCatalog === decisionPath
  || outProvenance === candidatePath || outProvenance === decisionPath) {
  throw new Error('Outputs must not overwrite candidate or decision artifacts');
}

const lock = await verifySourceCompilerLock(repository, options.sourceLock);
const roots = await compileCanonicalRoots({
  jmdict: lock.inputs.jmdict.absolutePath,
  jmdictSourceId: lock.inputs.jmdict.id,
  extra: lock.inputs.extra.absolutePath,
  municipality: lock.inputs.municipality.absolutePath,
  ward: lock.inputs.ward.absolutePath,
  errata: lock.inputs.chronologicalErrata.absolutePath,
  compatibility: lock.inputs.compatibility.absolutePath
});
const [catalogBytes, candidateBytes, reviewBytes, provenance] = await Promise.all([
  readFile(lock.inputs.zhHansSenseInfo.absolutePath),
  readFile(candidatePath),
  readFile(decisionPath),
  readOptionalProvenance(provenancePath)
]);
const catalog = parseZhHansSenseInfoCatalog(parseJson(catalogBytes, 'Locked production catalog'));
const candidateArtifact = parseZhHansSenseInfoCandidateArtifact(
  parseJson(candidateBytes, `Candidate artifact ${candidatePath}`)
);
const reviewArtifact = parseZhHansSenseInfoReviewArtifact(
  parseJson(reviewBytes, `Review artifact ${decisionPath}`)
);
const result = mergeZhHansSenseInfoReviews({
  entries: roots.entries,
  catalog,
  catalogIdentity: {
    id: lock.inputs.zhHansSenseInfo.id,
    sha256: sha256(catalogBytes)
  },
  jmdictIdentity: {
    id: lock.inputs.jmdict.id,
    sha256: lock.inputs.jmdict.sha256
  },
  candidateArtifact,
  candidateSha256: sha256(candidateBytes),
  reviewArtifact,
  provenance
});

let stagedCatalog: string | undefined;
let stagedProvenance: string | undefined;
try {
  stagedCatalog = await stageJson(outCatalog, result.catalog);
  stagedProvenance = await stageJson(outProvenance, result.provenance);
  await rename(stagedProvenance, outProvenance);
  stagedProvenance = undefined;
  await rename(stagedCatalog, outCatalog);
  stagedCatalog = undefined;
} finally {
  if (stagedCatalog) await unlink(stagedCatalog).catch(() => undefined);
  if (stagedProvenance) await unlink(stagedProvenance).catch(() => undefined);
}

const writtenCatalog = new Uint8Array(await readFile(outCatalog));
process.stdout.write(`${JSON.stringify({
  catalog: {
    path: relative(repository, outCatalog),
    bytes: writtenCatalog.byteLength,
    sha256: sha256(writtenCatalog),
    sourceLockRepinRequired: sha256(writtenCatalog) !== lock.inputs.zhHansSenseInfo.sha256
  },
  provenance: { path: relative(repository, outProvenance) },
  stats: result.stats
}, null, 2)}\n`);
