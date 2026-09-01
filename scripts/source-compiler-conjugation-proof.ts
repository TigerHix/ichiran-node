#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { readFile, writeFile, mkdir, mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gunzipSync } from 'node:zlib';
import { loadAllConjugationRules } from '../packages/data/src/data/conj-rules.js';
import { readCanonicalEntryNdjson } from '../packages/data/src/source-compiler/canonical-entry-ndjson.js';
import {
  compareSortedRelationFiles,
  type ReviewedRelationDelta
} from '../packages/data/src/source-compiler/conjugation-relation-proof.js';
import { writeForwardRelation } from '../packages/data/src/source-compiler/conjugation-relation-proof.js';
import { writePackedMorphologyRelation } from '../packages/data/src/source-compiler/packed-morphology-relation.js';

interface Options {
  readonly roots: string | null;
  readonly forward: string | null;
  readonly pack: string;
  readonly out: string;
  readonly reviewed: string | null;
  readonly work: string | null;
  readonly keepWork: boolean;
  readonly rootLimit: number | undefined;
  readonly surfaceLimit: number | undefined;
}

function usage(): never {
  throw new Error(
    'Usage: bun scripts/source-compiler-conjugation-proof.ts '
    + '(--forward complete-relation.ndjson | --roots canonical-roots.ndjson) '
    + '--pack hot.bin[.gz] --out report.json '
    + '[--reviewed exact-deltas.json] [--work directory] [--keep-work] '
    + '[--root-limit n] [--surface-limit n]'
  );
}

function positiveInteger(value: string | undefined, flag: string): number {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 1) throw new Error(`${flag} requires a positive integer`);
  return parsed;
}

function parseArgs(argv: readonly string[]): Options {
  const values = new Map<string, string>();
  let keepWork = false;
  for (let index = 0; index < argv.length; index++) {
    const flag = argv[index]!;
    if (flag === '--keep-work') {
      keepWork = true;
      continue;
    }
    if (!flag.startsWith('--') || index + 1 >= argv.length) usage();
    values.set(flag, argv[++index]!);
  }
  const roots = values.get('--roots') ?? null;
  const forward = values.get('--forward') ?? null;
  const pack = values.get('--pack');
  const out = values.get('--out');
  if ((roots === null) === (forward === null) || !pack || !out) usage();
  if (forward !== null && values.has('--root-limit')) {
    throw new Error('--root-limit applies only to --roots mode');
  }
  return {
    roots: roots === null ? null : resolve(roots),
    forward: forward === null ? null : resolve(forward),
    pack: resolve(pack),
    out: resolve(out),
    reviewed: values.has('--reviewed') ? resolve(values.get('--reviewed')!) : null,
    work: values.has('--work') ? resolve(values.get('--work')!) : null,
    keepWork,
    rootLimit: values.has('--root-limit')
      ? positiveInteger(values.get('--root-limit'), '--root-limit') : undefined,
    surfaceLimit: values.has('--surface-limit')
      ? positiveInteger(values.get('--surface-limit'), '--surface-limit') : undefined
  };
}

function reviewedRows(value: unknown): ReviewedRelationDelta[] {
  if (!Array.isArray(value)) throw new Error('Reviewed delta file must contain a JSON array');
  return value.map((item, index) => {
    if (typeof item !== 'object' || item === null || Array.isArray(item)) {
      throw new Error(`Reviewed delta ${index} must be an object`);
    }
    const row = item as Record<string, unknown>;
    if ((row.side !== 'omission' && row.side !== 'packed-only')
      || typeof row.key !== 'string' || typeof row.category !== 'string'
      || typeof row.provenance !== 'string' || typeof row.preservedBehavior !== 'string') {
      throw new Error(`Reviewed delta ${index} has an invalid field`);
    }
    return {
      side: row.side,
      key: row.key,
      category: row.category,
      provenance: row.provenance,
      preservedBehavior: row.preservedBehavior
    };
  });
}

async function sortRelation(input: string, output: string, temporary: string): Promise<void> {
  await new Promise<void>((resolvePromise, reject) => {
    const child = spawn('sort', ['-T', temporary, '-o', output, input], {
      stdio: ['ignore', 'ignore', 'pipe'],
      env: { ...process.env, LC_ALL: 'C' }
    });
    const errors: Buffer[] = [];
    child.stderr.on('data', (chunk: Buffer) => errors.push(chunk));
    child.once('error', reject);
    child.once('close', code => {
      if (code === 0) resolvePromise();
      else reject(new Error(`sort failed (${String(code)}): ${Buffer.concat(errors).toString('utf8')}`));
    });
  });
}

function installedPack(bytes: Uint8Array): Uint8Array {
  return bytes[0] === 0x1f && bytes[1] === 0x8b ? new Uint8Array(gunzipSync(bytes)) : bytes;
}

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function main(): Promise<void> {
  const options = parseArgs(process.argv.slice(2));
  const base = options.work ?? tmpdir();
  if (options.work) await mkdir(base, { recursive: true });
  const work = await mkdtemp(join(base, 'ichiran-conjugation-proof-'));
  const generatedForwardRaw = join(work, 'forward.raw.ndjson');
  const packedRaw = join(work, 'packed.raw.ndjson');
  const forwardSorted = join(work, 'forward.sorted.ndjson');
  const packedSorted = join(work, 'packed.sorted.ndjson');
  const repository = fileURLToPath(new URL('..', import.meta.url));
  loadAllConjugationRules(join(repository, 'data'));

  try {
    const downloadedPack = new Uint8Array(await readFile(options.pack));
    const hotPack = installedPack(downloadedPack);
    const forward = options.roots === null ? null : await writeForwardRelation(
      readCanonicalEntryNdjson(options.roots), generatedForwardRaw, {
        rootLimit: options.rootLimit,
        onProgress: (roots, rows) => console.error(`forward roots=${roots} rows=${rows}`)
      });
    const forwardRaw = options.forward ?? generatedForwardRaw;
    const packed = await writePackedMorphologyRelation(hotPack, packedRaw, {
      surfaceLimit: options.surfaceLimit,
      onProgress: (surfaces, candidates) =>
        console.error(`packed surfaces=${surfaces} candidates=${candidates}`)
    });
    await Promise.all([
      sortRelation(forwardRaw, forwardSorted, work),
      sortRelation(packedRaw, packedSorted, work)
    ]);
    const reviewed = options.reviewed === null
      ? []
      : reviewedRows(JSON.parse(await readFile(options.reviewed, 'utf8')));
    const relation = await compareSortedRelationFiles(forwardSorted, packedSorted, reviewed);
    if (relation.packed.rows !== packed.candidates) {
      throw new Error(
        `Packed relation file contains ${relation.packed.rows} rows; traversal emitted ${packed.candidates}`
      );
    }
    if (forward !== null && relation.forward.rows !== forward.rows) {
      throw new Error(
        `Forward relation file contains ${relation.forward.rows} rows; enumeration emitted ${forward.rows}`
      );
    }
    // `--roots` is a rule-only integration aid. The complete gate consumes the
    // final compiler relation so chronological emission edits cannot be skipped.
    const complete = options.forward !== null && options.surfaceLimit === undefined && packed.complete;
    const report = {
      formatVersion: 1,
      complete,
      passed: complete && relation.passed,
      inputs: {
        roots: options.roots,
        forward: options.forward,
        pack: options.pack,
        downloadedPackSha256: sha256(downloadedPack),
        installedPackSha256: sha256(hotPack),
        reviewed: options.reviewed
      },
      enumeration: {
        forward: forward ?? { roots: null, rows: relation.forward.rows },
        packed
      },
      relation
    };
    await mkdir(dirname(options.out), { recursive: true });
    await writeFile(options.out, `${JSON.stringify(report, null, 2)}\n`, { flag: 'wx' });
    console.log(JSON.stringify(report, null, 2));
    if (!report.passed) process.exitCode = 1;
  } finally {
    if (options.keepWork) console.error(`kept work=${work}`);
    else await rm(work, { recursive: true, force: true });
  }
}

await main();
