#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { resolve } from 'node:path';

import { buildDetailStore } from '../packages/data/src/browser-pack/details.js';
import { buildRootPayload } from '../packages/data/src/browser-pack/root-payload.js';
import { canonicalEntryJson } from '../packages/data/src/source-compiler/digest.js';
import { loadJmdictEntries } from '../packages/data/src/source-compiler/jmdict.js';
import {
  canonicalDetailEntries,
  canonicalRootPayloadSource
} from '../packages/data/src/source-compiler/pack-input.js';
import {
  canonicalSurfaceIndexRows,
  encodeSurfaceIndexTsv
} from '../packages/data/src/source-compiler/surface-index-input.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';
import type { CanonicalEntry } from '../packages/data/src/source-compiler/model.js';

const LOCK_PATH = 'data/source-compiler-update-2026-01-02.lock.json';
const UPDATE_SOURCE_ID = 'edrdg-jmdict-e-2026-01-02';
const WITNESS_SEQ = 2_868_547;

function sha256(bytes: Uint8Array | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function findEntry(
  path: string,
  sourceId: string
): Promise<CanonicalEntry | null> {
  for await (const entry of loadJmdictEntries(path, sourceId)) {
    if (entry.seq === WITNESS_SEQ) return entry;
  }
  return null;
}

function assertWitness(entry: CanonicalEntry): void {
  const kana = entry.kana.map(form => form.text);
  const glosses = entry.senses.flatMap(sense => sense.glosses);
  if (JSON.stringify(kana) !== JSON.stringify(['パオーン'])
    || !glosses.includes('sound of elephants')) {
    throw new Error(`JMdict update witness ${WITNESS_SEQ} has unexpected semantics`);
  }
}

const repository = resolve(import.meta.dir, '..');
const [baselineLock, lock] = await Promise.all([
  verifySourceCompilerLock(repository),
  verifySourceCompilerLock(repository, LOCK_PATH)
]);
const baselineSource = baselineLock.inputs.jmdict;
const updateSource = lock.inputs.jmdict;
if (updateSource.id !== UPDATE_SOURCE_ID) {
  throw new Error(`Update lock pins ${updateSource.id}, not ${UPDATE_SOURCE_ID}`);
}

const [baseline, update] = await Promise.all([
  findEntry(baselineSource.absolutePath, baselineSource.id),
  findEntry(updateSource.absolutePath, updateSource.id)
]);
if (baseline !== null) throw new Error(`Baseline unexpectedly contains ${WITNESS_SEQ}`);
if (update === null) throw new Error(`January 2 update omits ${WITNESS_SEQ}`);
assertWitness(update);

const firstRoot = buildRootPayload(canonicalRootPayloadSource([update]));
const secondRoot = buildRootPayload(canonicalRootPayloadSource([update]));
const firstDetails = buildDetailStore(canonicalDetailEntries([update]));
const secondDetails = buildDetailStore(canonicalDetailEntries([update]));
const firstSurface = encodeSurfaceIndexTsv(canonicalSurfaceIndexRows([update], []));
const secondSurface = encodeSurfaceIndexTsv(canonicalSurfaceIndexRows([update], []));
if (!Buffer.from(firstRoot.bytes).equals(Buffer.from(secondRoot.bytes))
  || !Buffer.from(firstDetails.bytes).equals(Buffer.from(secondDetails.bytes))
  || !Buffer.from(firstSurface).equals(Buffer.from(secondSurface))) {
  throw new Error('Update witness encoders are nondeterministic');
}
const expectedSurface = new TextEncoder().encode('パオーン\t1\t0\t0\t0\n');
if (!Buffer.from(firstSurface).equals(Buffer.from(expectedSurface))) {
  throw new Error(
    `Update witness has unexpected surface semantics: ${new TextDecoder().decode(firstSurface)}`
  );
}

process.stdout.write(`${JSON.stringify({
  formatVersion: 1,
  source: {
    id: updateSource.id,
    path: updateSource.path,
    bytes: updateSource.bytes,
    sha256: updateSource.sha256,
    lockSha256: lock.sha256
  },
  witness: {
    seq: update.seq,
    baselinePresent: false,
    source: update.source,
    kana: update.kana.map(form => form.text),
    glosses: update.senses.flatMap(sense => sense.glosses),
    canonicalSha256: sha256(canonicalEntryJson(update))
  },
  output: {
    surfaceTsv: { bytes: firstSurface.byteLength, sha256: sha256(firstSurface) },
    rootPayload: { bytes: firstRoot.bytes.byteLength, sha256: sha256(firstRoot.bytes) },
    details: { bytes: firstDetails.bytes.byteLength, sha256: sha256(firstDetails.bytes) }
  }
}, null, 2)}\n`);
