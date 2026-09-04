#!/usr/bin/env bun

import { createHash, randomUUID } from 'node:crypto';
import { mkdir, readFile, rename, unlink, writeFile } from 'node:fs/promises';
import { dirname, relative, resolve } from 'node:path';

import { combineZhHansSenseInfoReviewPairs } from '../packages/data/src/source-compiler/zh-hans-sense-info-review-combine.js';
import {
  assertCodexCandidateArtifactPath
} from '../packages/data/src/source-compiler/zh-hans-sense-info-review.js';

const USAGE = 'usage: bun scripts/zh-hans-sense-info-review-combine.ts '
  + '--pair <strict-candidates.json> <strict-decisions.json> [--pair ...] '
  + '--out-candidates <combined-candidates.json> --out-decisions <combined-decisions.json>';

function parseOptions(args: readonly string[]) {
  const pairs: { candidate: string; review: string }[] = [];
  let outCandidates: string | undefined;
  let outDecisions: string | undefined;
  for (let index = 0; index < args.length;) {
    const name = args[index++];
    if (name === '--pair') {
      const candidate = args[index++];
      const review = args[index++];
      if (!candidate || !review || candidate.startsWith('--') || review.startsWith('--')) {
        throw new Error(USAGE);
      }
      pairs.push({ candidate, review });
    } else if (name === '--out-candidates' || name === '--out-decisions') {
      const value = args[index++];
      if (!value || value.startsWith('--')) throw new Error(USAGE);
      if (name === '--out-candidates') {
        if (outCandidates) throw new Error('Duplicate --out-candidates option');
        outCandidates = value;
      } else {
        if (outDecisions) throw new Error('Duplicate --out-decisions option');
        outDecisions = value;
      }
    } else {
      throw new Error(`${name ? `Unknown option ${name}\n` : ''}${USAGE}`);
    }
  }
  if (pairs.length < 2 || !outCandidates || !outDecisions) throw new Error(USAGE);
  return { pairs, outCandidates, outDecisions };
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
const pairPaths = options.pairs.map(pair => ({
  candidate: resolve(process.cwd(), pair.candidate),
  review: resolve(process.cwd(), pair.review)
}));
for (const pair of pairPaths) {
  assertCodexCandidateArtifactPath(pair.candidate);
  assertCodexCandidateArtifactPath(pair.review);
}
const outputPaths = [
  resolve(process.cwd(), options.outCandidates),
  resolve(process.cwd(), options.outDecisions)
];
if (outputPaths[0] === outputPaths[1]) throw new Error('Combined outputs must differ');
const inputs = new Set(pairPaths.flatMap(pair => [pair.candidate, pair.review]));
if (outputPaths.some(path => inputs.has(path))) {
  throw new Error('Combined outputs must not overwrite input artifacts');
}
const rawPairs = await Promise.all(pairPaths.map(async pair => {
  const [candidateBytes, reviewBytes] = await Promise.all([
    readFile(pair.candidate),
    readFile(pair.review)
  ]);
  return { candidateBytes, reviewBytes };
}));
const combined = combineZhHansSenseInfoReviewPairs(rawPairs);
const outputBytes = [combined.candidateBytes, combined.reviewBytes];
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
  inputs: rawPairs.length,
  candidates: combined.candidateArtifact.candidates.length,
  outputs: outputPaths.map((path, index) => ({
    path: relative(repository, path),
    bytes: outputBytes[index]!.byteLength,
    sha256: sha256(outputBytes[index]!)
  }))
}, null, 2)}\n`);
