#!/usr/bin/env bun

import { createHash, randomUUID } from 'node:crypto';
import { mkdir, readFile, rename, unlink, writeFile } from 'node:fs/promises';
import { dirname, relative, resolve } from 'node:path';

import { normalizeZhHansSenseInfoCatalog } from
  '../packages/data/src/source-compiler/zh-hans-sense-info-catalog-normalize.js';
import { parseZhHansSenseInfoCatalog } from
  '../packages/data/src/source-compiler/zh-hans-sense-info.js';

const USAGE = 'usage: bun scripts/zh-hans-sense-info-catalog-normalize.ts '
  + '--catalog <post-merge-catalog.json> --out-catalog <normalized-catalog.json>';

function parseOptions(args: readonly string[]): { readonly catalog: string; readonly outCatalog: string } {
  const values = new Map<string, string>();
  for (let index = 0; index < args.length; index += 2) {
    const name = args[index];
    const value = args[index + 1];
    if (!name?.startsWith('--') || !value || value.startsWith('--')) throw new Error(USAGE);
    if (name !== '--catalog' && name !== '--out-catalog') {
      throw new Error(`Unknown option ${name}\n${USAGE}`);
    }
    if (values.has(name)) throw new Error(`Duplicate option ${name}`);
    values.set(name, value);
  }
  const catalog = values.get('--catalog');
  const outCatalog = values.get('--out-catalog');
  if (!catalog || !outCatalog) throw new Error(USAGE);
  return { catalog, outCatalog };
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

const repository = resolve(import.meta.dir, '..');
const options = parseOptions(process.argv.slice(2));
const inputPath = resolve(process.cwd(), options.catalog);
const outputPath = resolve(process.cwd(), options.outCatalog);
if (inputPath === outputPath) {
  throw new Error('Catalog normalization requires a distinct output path');
}

const inputBytes = new Uint8Array(await readFile(inputPath));
const result = normalizeZhHansSenseInfoCatalog(
  parseZhHansSenseInfoCatalog(parseJson(inputBytes, `Catalog ${inputPath}`))
);
const outputBytes = new TextEncoder().encode(`${JSON.stringify(result.catalog, null, 2)}\n`);
await mkdir(dirname(outputPath), { recursive: true });
const temporary = `${outputPath}.tmp-${randomUUID()}`;
try {
  await writeFile(temporary, outputBytes, { flag: 'wx' });
  await rename(temporary, outputPath);
} catch (error) {
  await unlink(temporary).catch(() => undefined);
  throw error;
}

process.stdout.write(`${JSON.stringify({
  input: {
    path: relative(repository, inputPath),
    bytes: inputBytes.byteLength,
    sha256: sha256(inputBytes)
  },
  output: {
    path: relative(repository, outputPath),
    bytes: outputBytes.byteLength,
    sha256: sha256(outputBytes),
    sourceLockRepinRequired: sha256(outputBytes) !== sha256(inputBytes)
  },
  stats: result.stats,
  reviewedTargetUpdates: result.reviewedTargetUpdates,
  prunedTranslations: result.prunedTranslations
}, null, 2)}\n`);
