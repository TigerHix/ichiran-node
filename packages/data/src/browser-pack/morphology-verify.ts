import { createWriteStream } from 'node:fs';
import { once } from 'node:events';
import {
  assertBrowserAlphaMorphologyAttestation,
  parseBrowserAlphaSourceLock,
  sha256Bytes
} from './release-orchestration.js';
import {
  morphologyRelationAttestation,
  verifyMorphologyRelation,
  type MorphologyLookup
} from './morphology-verifier.js';

function argument(name: string): string | undefined {
  const index = process.argv.indexOf(name);
  return index < 0 ? undefined : process.argv[index + 1];
}

const artifactPath = argument('--artifact');
if (!artifactPath) {
  throw new Error(
    'Usage: bun morphology-verify.ts --artifact <morphology.bin> '
    + '[--diff <diff.jsonl>] [--lock <sources.lock.json>]'
  );
}

const moduleUrl = new URL('../../../portable/src/morphology.ts', import.meta.url).href;
const portable = await import(moduleUrl) as {
  openMorphology(input: ArrayBuffer | Uint8Array): MorphologyLookup;
};
const lockPath = argument('--lock')
  ?? new URL('../../../../browser-alpha/sources.lock.json', import.meta.url);
const lock = parseBrowserAlphaSourceLock(await Bun.file(lockPath).text());
if (!lock.artifactDigests) throw new Error('Sources lock is missing exact artifact digests');
const artifact = new Uint8Array(await Bun.file(artifactPath).arrayBuffer());
const expectedArtifact = lock.artifactDigests.morphology;
if (artifact.byteLength !== expectedArtifact.bytes || sha256Bytes(artifact) !== expectedArtifact.sha256) {
  throw new Error('Morphology artifact does not match the sources lock');
}
const reader = portable.openMorphology(artifact);
const diffPath = argument('--diff');
const diffStream = diffPath ? createWriteStream(diffPath, { flags: 'w' }) : null;

const result = await verifyMorphologyRelation({
  lookup: reader,
  onDiff: diffStream
    ? (_diff, canonicalLine) => { diffStream.write(canonicalLine); }
    : undefined,
  onProgress: (groups, rows) => {
    console.error(`verified ${groups.toLocaleString()} surfaces / ${rows.toLocaleString()} relation rows`);
  }
});

if (diffStream) {
  diffStream.end();
  await once(diffStream, 'close');
}
const attestation = morphologyRelationAttestation(result);
assertBrowserAlphaMorphologyAttestation(
  attestation,
  lock.artifactDigests.morphologyRelation
);
console.log(JSON.stringify({
  verified: true,
  artifact: artifactPath,
  diff: diffPath ?? null,
  attestation,
  elapsedMs: result.elapsedMs,
  examples: result.examples
}, null, 2));
