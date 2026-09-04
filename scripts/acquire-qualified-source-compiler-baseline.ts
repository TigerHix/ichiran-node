#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { access, mkdir, mkdtemp, readFile, rename, rm, writeFile } from 'node:fs/promises';
import { dirname, join, resolve } from 'node:path';

const TAG = 'portable-core-260118-baseline';
const BASE_URL = `https://github.com/TigerHix/ichiran-node/releases/download/${TAG}`;
const INDEX_NAME = 'artifact-sha256.txt';
const INDEX_SHA256 = 'a032bbd11c257259877b438a79c674544985a58acdaff86327ae57ae8cbeb3ac';
// Historical format-v1 acquisition only; current releases use lexicon + locale artifacts.
const ARTIFACTS = ['manifest.json', 'hot.bin.gz', 'details.bin.gz', 'stats.json'] as const;

function sha256(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function exists(path: string): Promise<boolean> {
  try {
    await access(path);
    return true;
  } catch {
    return false;
  }
}

async function download(name: string): Promise<Uint8Array> {
  const response = await fetch(`${BASE_URL}/${name}`);
  if (!response.ok) throw new Error(`Failed to download ${name}: HTTP ${response.status}`);
  return new Uint8Array(await response.arrayBuffer());
}

function expectedArtifacts(index: Uint8Array): ReadonlyMap<string, string> {
  if (sha256(index) !== INDEX_SHA256) {
    throw new Error('Qualified artifact checksum index is not the immutable reviewed index');
  }
  const result = new Map<string, string>();
  for (const line of new TextDecoder().decode(index).trim().split('\n')) {
    const match = /^([0-9a-f]{64})  ([^/]+)$/.exec(line);
    if (!match) throw new Error(`Invalid qualified artifact checksum row: ${line}`);
    result.set(match[2]!, match[1]!);
  }
  if (result.size !== ARTIFACTS.length
    || ARTIFACTS.some(name => !result.has(name))) {
    throw new Error('Qualified artifact checksum index does not name exactly four release artifacts');
  }
  return result;
}

async function verify(directory: string): Promise<void> {
  const index = new Uint8Array(await readFile(join(directory, INDEX_NAME)));
  const expected = expectedArtifacts(index);
  for (const name of ARTIFACTS) {
    const bytes = new Uint8Array(await readFile(join(directory, name)));
    if (sha256(bytes) !== expected.get(name)) {
      throw new Error(`Qualified artifact ${name} differs from the immutable checksum index`);
    }
  }
}

const output = resolve(process.argv[2] ?? 'work/m2-baseline');
if (await exists(output)) {
  await verify(output);
  process.stdout.write(`verified ${output}\n`);
  process.exit(0);
}

await mkdir(dirname(output), { recursive: true });
const staging = await mkdtemp(join(dirname(output), '.qualified-baseline-'));
try {
  const index = await download(INDEX_NAME);
  const expected = expectedArtifacts(index);
  await writeFile(join(staging, INDEX_NAME), index);
  for (const name of ARTIFACTS) {
    const bytes = await download(name);
    if (sha256(bytes) !== expected.get(name)) {
      throw new Error(`Downloaded qualified artifact ${name} differs from the immutable index`);
    }
    await writeFile(join(staging, name), bytes);
  }
  await verify(staging);
  await rename(staging, output);
  process.stdout.write(`acquired and verified ${output}\n`);
} catch (error) {
  await rm(staging, { recursive: true, force: true });
  throw error;
}
