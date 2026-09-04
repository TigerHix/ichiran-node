#!/usr/bin/env bun

import { createHash, randomUUID } from 'node:crypto';
import { mkdir, readFile, rename, unlink, writeFile } from 'node:fs/promises';
import { dirname, relative, resolve } from 'node:path';

import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';
import {
  adaptRichZhHansSenseInfoAddBatch,
  adaptRichZhHansSenseInfoCatalogRevisions,
  serializeZhHansSenseInfoAuthoringArtifact
} from '../packages/data/src/source-compiler/zh-hans-sense-info-review-adapter.js';
import {
  ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY,
  assertCodexCandidateArtifactPath
} from '../packages/data/src/source-compiler/zh-hans-sense-info-review.js';
import { parseZhHansSenseInfoCatalog } from '../packages/data/src/source-compiler/zh-hans-sense-info.js';

interface Options {
  readonly mode: 'add' | 'revisions';
  readonly sourceLock: string;
  readonly richCandidates?: string;
  readonly richReview: string;
  readonly translatorModel: string;
  readonly translatorRunId: string;
  readonly translatedAt: string;
  readonly reviewerModel: string;
  readonly reviewerRunId: string;
  readonly reviewedAt: string;
  readonly outCandidates: string;
  readonly outDecisions: string;
  readonly outReceipt: string;
}

const USAGE = 'usage: bun scripts/zh-hans-sense-info-review-adapt.ts '
  + '--mode <add|revisions> [--rich-candidates <rich-candidates.json>] '
  + '--rich-review <rich-review.json> '
  + '--translator-model <model> --translator-run-id <run-id> --translated-at <ISO timestamp> '
  + '--reviewer-model <model> --reviewer-run-id <run-id> --reviewed-at <ISO timestamp> '
  + '--out-candidates <strict-candidates.json> --out-decisions <strict-decisions.json> '
  + '--out-receipt <adaptation-receipt.json> [--source-lock <repository-relative-lock.json>]';

function parseOptions(args: readonly string[]): Options {
  const allowed = new Set([
    '--mode', '--source-lock', '--rich-candidates', '--rich-review', '--translator-model',
    '--translator-run-id', '--translated-at', '--reviewer-model', '--reviewer-run-id',
    '--reviewed-at', '--out-candidates', '--out-decisions', '--out-receipt'
  ]);
  const values = new Map<string, string>();
  for (let index = 0; index < args.length; index += 2) {
    const name = args[index];
    const value = args[index + 1];
    if (!name?.startsWith('--') || !value || value.startsWith('--')) throw new Error(USAGE);
    if (!allowed.has(name)) throw new Error(`Unknown option ${name}\n${USAGE}`);
    if (values.has(name)) throw new Error(`Duplicate option ${name}`);
    values.set(name, value);
  }
  const mode = values.get('--mode');
  if (mode !== 'add' && mode !== 'revisions') throw new Error(USAGE);
  const required = [
    '--rich-review', '--translator-model', '--translator-run-id', '--translated-at',
    '--reviewer-model', '--reviewer-run-id', '--reviewed-at', '--out-candidates',
    '--out-decisions', '--out-receipt'
  ];
  if (required.some(name => !values.has(name))) throw new Error(USAGE);
  const richCandidates = values.get('--rich-candidates');
  if ((mode === 'add') !== (richCandidates !== undefined)) {
    throw new Error('--rich-candidates is required only for add mode');
  }
  return {
    mode,
    sourceLock: values.get('--source-lock') ?? 'data/source-compiler-update-2026-09-02.lock.json',
    ...(richCandidates ? { richCandidates } : {}),
    richReview: values.get('--rich-review')!,
    translatorModel: values.get('--translator-model')!,
    translatorRunId: values.get('--translator-run-id')!,
    translatedAt: values.get('--translated-at')!,
    reviewerModel: values.get('--reviewer-model')!,
    reviewerRunId: values.get('--reviewer-run-id')!,
    reviewedAt: values.get('--reviewed-at')!,
    outCandidates: values.get('--out-candidates')!,
    outDecisions: values.get('--out-decisions')!,
    outReceipt: values.get('--out-receipt')!
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

async function stage(path: string, bytes: Uint8Array): Promise<string> {
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.tmp-${randomUUID()}`;
  await writeFile(temporary, bytes, { flag: 'wx' });
  return temporary;
}

const repository = resolve(import.meta.dir, '..');
const options = parseOptions(process.argv.slice(2));
const richReviewPath = resolve(process.cwd(), options.richReview);
const richCandidatePath = options.richCandidates
  ? resolve(process.cwd(), options.richCandidates)
  : undefined;
assertCodexCandidateArtifactPath(richReviewPath);
if (richCandidatePath) assertCodexCandidateArtifactPath(richCandidatePath);
const outputPaths = [
  resolve(process.cwd(), options.outCandidates),
  resolve(process.cwd(), options.outDecisions),
  resolve(process.cwd(), options.outReceipt)
];
if (new Set(outputPaths).size !== outputPaths.length) throw new Error('All outputs must differ');
for (const output of outputPaths) {
  if (output === richReviewPath || output === richCandidatePath) {
    throw new Error('Outputs must not overwrite rich input artifacts');
  }
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
const [catalogBytes, richReviewBytes, richCandidateBytes] = await Promise.all([
  readFile(lock.inputs.zhHansSenseInfo.absolutePath),
  readFile(richReviewPath),
  richCandidatePath ? readFile(richCandidatePath) : Promise.resolve(undefined)
]);
const catalog = parseZhHansSenseInfoCatalog(parseJson(catalogBytes, 'Locked production catalog'));
const metadata = {
  translator: {
    kind: 'codex' as const,
    provider: 'openai' as const,
    model: options.translatorModel,
    runId: options.translatorRunId,
    generatedAt: options.translatedAt,
    sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
  },
  reviewer: {
    kind: 'codex' as const,
    provider: 'openai' as const,
    model: options.reviewerModel,
    runId: options.reviewerRunId,
    reviewedAt: options.reviewedAt,
    sourcePolicy: ZH_HANS_SENSE_INFO_CODEX_SOURCE_POLICY
  }
};
const common = {
  entries: roots.entries,
  catalog,
  jmdictIdentity: { id: lock.inputs.jmdict.id, sha256: lock.inputs.jmdict.sha256 },
  catalogIdentity: {
    id: lock.inputs.zhHansSenseInfo.id,
    sha256: sha256(catalogBytes)
  },
  richReview: parseJson(richReviewBytes, `Rich review ${richReviewPath}`),
  richReviewSha256: sha256(richReviewBytes),
  metadata
};
const adapted = options.mode === 'add'
  ? adaptRichZhHansSenseInfoAddBatch({
      ...common,
      richCandidates: parseJson(richCandidateBytes!, `Rich candidates ${richCandidatePath}`),
      richCandidateSha256: sha256(richCandidateBytes!)
    })
  : adaptRichZhHansSenseInfoCatalogRevisions(common);
const outputBytes = [
  adapted.candidateBytes,
  adapted.reviewBytes,
  serializeZhHansSenseInfoAuthoringArtifact(adapted.receipt)
];
const staged: (string | undefined)[] = [];
try {
  for (let index = 0; index < outputPaths.length; index++) {
    staged[index] = await stage(outputPaths[index]!, outputBytes[index]!);
  }
  for (let index = 0; index < outputPaths.length; index++) {
    await rename(staged[index]!, outputPaths[index]!);
    staged[index] = undefined;
  }
} finally {
  await Promise.all(staged.filter((path): path is string => path !== undefined).map(
    path => unlink(path).catch(() => undefined)
  ));
}
process.stdout.write(`${JSON.stringify({
  mode: options.mode,
  outputs: outputPaths.map((path, index) => ({
    path: relative(repository, path),
    bytes: outputBytes[index]!.byteLength,
    sha256: sha256(outputBytes[index]!)
  })),
  decisions: adapted.receipt.sourceDecisionCounts,
  emittedCandidates: adapted.receipt.emittedCandidateCount,
  nonMutatingApprovals: adapted.receipt.nonMutatingDecisions.length,
  excludedDecisions: adapted.receipt.excludedDecisions.length
}, null, 2)}\n`);
