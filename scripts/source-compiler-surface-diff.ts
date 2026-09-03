#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { readFile, writeFile } from 'node:fs/promises';
import { createInterface } from 'node:readline';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import {
  openPack,
  openSurfaceIndex,
  SURFACE_INDEX_SECTION_ID
} from '@ichiran/core/compiler';
import { packedMorphologySurfaces } from '../packages/data/src/source-compiler/packed-morphology-relation.js';

interface CandidateSurface {
  readonly surface: string;
  readonly direct: boolean;
  readonly morphology: boolean;
}

function argumentsOf(argv: readonly string[]): {
  readonly candidate: string; readonly pack: string; readonly out: string;
} {
  if (argv.length !== 6 || argv[0] !== '--candidate' || argv[2] !== '--pack'
    || argv[4] !== '--out') {
    throw new Error('Usage: bun scripts/source-compiler-surface-diff.ts --candidate surface.tsv --pack hot.bin.gz --out report.json');
  }
  return { candidate: resolve(argv[1]!), pack: resolve(argv[3]!), out: resolve(argv[5]!) };
}

async function* candidateSurfaces(path: string): AsyncGenerator<CandidateSurface> {
  const input = createReadStream(path, { encoding: 'utf8' });
  const lines = createInterface({ input, crlfDelay: Infinity });
  try {
    for await (const line of lines) {
      const fields = line.split('\t');
      if (fields.length !== 5 || fields.slice(1).some(value => value !== '0' && value !== '1')) {
        throw new Error(`Invalid candidate surface row ${JSON.stringify(line)}`);
      }
      yield {
        surface: fields[0]!,
        direct: fields[1] === '1' || fields[3] === '1',
        morphology: fields[2] === '1' || fields[4] === '1'
      };
    }
  } finally {
    lines.close();
    input.destroy();
  }
}

async function* selected(path: string, kind: 'direct' | 'morphology'): AsyncGenerator<string> {
  for await (const value of candidateSurfaces(path)) if (value[kind]) yield value.surface;
}

function compare(left: string, right: string): number {
  return Buffer.compare(Buffer.from(left), Buffer.from(right));
}

async function difference(
  candidate: AsyncGenerator<string>,
  qualified: Iterator<string>
): Promise<object> {
  let left = await candidate.next();
  let right = qualified.next();
  let common = 0;
  let missing = 0;
  let extra = 0;
  const missingHash = createHash('sha256');
  const extraHash = createHash('sha256');
  const missingExamples: string[] = [];
  const extraExamples: string[] = [];
  while (!left.done || !right.done) {
    const order = left.done ? 1 : right.done ? -1 : compare(left.value, right.value);
    if (order === 0) {
      common++;
      left = await candidate.next();
      right = qualified.next();
    } else if (order < 0) {
      extra++;
      extraHash.update(JSON.stringify(left.value) + '\n');
      if (extraExamples.length < 20) extraExamples.push(left.value);
      left = await candidate.next();
    } else {
      missing++;
      missingHash.update(JSON.stringify(right.value) + '\n');
      if (missingExamples.length < 20) missingExamples.push(right.value);
      right = qualified.next();
    }
  }
  return {
    common,
    missing,
    extra,
    missingSha256: missingHash.digest('hex'),
    extraSha256: extraHash.digest('hex'),
    missingExamples,
    extraExamples
  };
}

const options = argumentsOf(process.argv.slice(2));
const compressed = new Uint8Array(await readFile(options.pack));
const hot = compressed[0] === 0x1f && compressed[1] === 0x8b
  ? new Uint8Array(gunzipSync(compressed)) : compressed;
const pack = openPack(hot);
const surfaceBytes = pack.getSection(SURFACE_INDEX_SECTION_ID);
const qualified = openSurfaceIndex(surfaceBytes);
const direct = await difference(
  selected(options.candidate, 'direct'),
  (function* () {
    for (let rank = 0; rank < qualified.manifest.directCount; rank++) {
      yield qualified.directSurface(rank);
    }
  })()
);
const morphology = await difference(
  selected(options.candidate, 'morphology'),
  packedMorphologySurfaces(surfaceBytes)
);
let flagDifferences = 0;
const flagHash = createHash('sha256');
const flagExamples: object[] = [];
for await (const value of candidateSurfaces(options.candidate)) {
  const expected = qualified.lookup(value.surface);
  if (expected && expected.direct === value.direct && expected.morphology === value.morphology) continue;
  flagDifferences++;
  const row = {
    surface: value.surface,
    candidate: { direct: value.direct, morphology: value.morphology },
    qualified: expected === null ? null : {
      direct: expected.direct, morphology: expected.morphology
    }
  };
  flagHash.update(JSON.stringify(row) + '\n');
  if (flagExamples.length < 20) flagExamples.push(row);
}
const report = {
  formatVersion: 1,
  qualified: qualified.manifest,
  direct,
  morphology,
  flagDifferences,
  flagDifferenceSha256: flagHash.digest('hex'),
  flagExamples
};
await writeFile(options.out, `${JSON.stringify(report, null, 2)}\n`, { flag: 'wx' });
process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
