import { createHash } from 'node:crypto';
import { writeFile } from 'node:fs/promises';
import { userInfo } from 'node:os';
import postgres from 'postgres';
import { buildDetailStore, loadDetailEntries } from '../packages/data/src/browser-pack/details.js';
import {
  buildRootPayload,
  loadRootPayloadSource,
  type RootPayloadFormSource
} from '../packages/data/src/browser-pack/root-payload.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  canonicalDetailEntries,
  canonicalRootPayloadSource
} from '../packages/data/src/source-compiler/pack-input.js';

const database = process.argv[2];
if (!database) {
  throw new Error('Usage: bun scripts/source-compiler-root-proof.ts DATABASE [ORDER_EVIDENCE_JSON]');
}

const paths = {
  jmdict: 'packages/data/JMdict_e.gz',
  extra: 'data/sources/extra.xml',
  municipality: 'data/sources/jichitai.csv',
  ward: 'data/sources/gyoseiku.csv',
  errata: 'data/source-compiler-errata.json',
  compatibility: 'data/source-compiler-compatibility.json'
};

function sha256(bytes: Uint8Array | string): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function formIdentity(form: RootPayloadFormSource): string {
  return JSON.stringify({
    seq: form.seq,
    ord: form.ord,
    common: form.common,
    commonTags: form.commonTags,
    conjugatable: form.conjugatable,
    nokanji: form.nokanji,
    best: form.best
  });
}

function groups(forms: readonly RootPayloadFormSource[]): Map<string, RootPayloadFormSource[]> {
  const result = new Map<string, RootPayloadFormSource[]>();
  for (const form of forms) {
    const key = `${form.route}\u0000${form.surface}`;
    const values = result.get(key) ?? [];
    values.push(form);
    result.set(key, values);
  }
  return result;
}

const compilation = await compileCanonicalRoots(paths);
const sourceDetails = canonicalDetailEntries(compilation.entries);
const sourceRoot = canonicalRootPayloadSource(compilation.entries);

const sql = postgres({
  database,
  host: process.env.PGHOST ?? '/var/run/postgresql',
  user: process.env.PGUSER ?? userInfo().username
});
const [oracleDetails, oracleRoot] = await Promise.all([
  loadDetailEntries(sql),
  loadRootPayloadSource(sql)
]);
await sql.end();

let exactDetails = 0;
const detailDeltas: number[] = [];
for (let index = 0; index < Math.max(sourceDetails.length, oracleDetails.length); index++) {
  if (JSON.stringify(sourceDetails[index]) === JSON.stringify(oracleDetails[index])) exactDetails++;
  else detailDeltas.push(sourceDetails[index]?.seq ?? oracleDetails[index]!.seq);
}

const sourceGroups = groups(sourceRoot.forms);
const oracleGroups = groups(oracleRoot.forms);
const orderDeltas: object[] = [];
let formsInDeltaGroups = 0;
let firstWinnerDeltas = 0;
let formSetsExact = sourceGroups.size === oracleGroups.size;
for (const [key, sourceForms] of sourceGroups) {
  const oracleForms = oracleGroups.get(key) ?? [];
  const sourceOrder = sourceForms.map(formIdentity);
  const oracleOrder = oracleForms.map(formIdentity);
  if (JSON.stringify([...sourceOrder].sort()) !== JSON.stringify([...oracleOrder].sort())) {
    formSetsExact = false;
  }
  if (JSON.stringify(sourceOrder) === JSON.stringify(oracleOrder)) continue;
  formsInDeltaGroups += sourceForms.length;
  const winnerChanged = sourceOrder[0] !== oracleOrder[0];
  if (winnerChanged) firstWinnerDeltas++;
  const [route, surface] = key.split('\u0000') as ['kanji' | 'kana', string];
  orderDeltas.push({ route, surface, winnerChanged, sourceOrder, qualifiedOrder: oracleOrder });
}

const sourceRootBuild = buildRootPayload(sourceRoot);
const oracleRootBuild = buildRootPayload(oracleRoot);
const sourceDetailBuild = buildDetailStore(sourceDetails);
const oracleDetailBuild = buildDetailStore(oracleDetails);
const orderEvidence = `${orderDeltas.map(row => JSON.stringify(row)).join('\n')}\n`;
if (process.argv[3]) await writeFile(process.argv[3], orderEvidence);

console.log(JSON.stringify({
  canonical: {
    entries: compilation.entries.length,
    compatibilityRows: compilation.compatibility.rows.length,
    customEdits: compilation.custom.edits.length,
    errata: compilation.errata.counts
  },
  details: {
    sourceEntries: sourceDetails.length,
    qualifiedEntries: oracleDetails.length,
    exactEntries: exactDetails,
    deltaEntries: detailDeltas,
    byteEqual: Buffer.compare(sourceDetailBuild.bytes, oracleDetailBuild.bytes) === 0,
    bytes: sourceDetailBuild.bytes.length,
    sha256: sha256(sourceDetailBuild.bytes)
  },
  roots: {
    entriesExact: JSON.stringify(sourceRoot.entries) === JSON.stringify(oracleRoot.entries),
    restrictionsExact: JSON.stringify(sourceRoot.restrictions) === JSON.stringify(oracleRoot.restrictions),
    formSetsExact,
    directOrder: {
      groups: sourceGroups.size,
      deltaGroups: orderDeltas.length,
      formsInDeltaGroups,
      firstWinnerDeltas,
      evidenceSha256: sha256(orderEvidence),
      sourceProjectionSha256: sourceRootBuild.stats.directOrderProjection.sha256,
      qualifiedProjectionSha256: oracleRootBuild.stats.directOrderProjection.sha256
    },
    source: {
      bytes: sourceRootBuild.bytes.length,
      sha256: sha256(sourceRootBuild.bytes)
    },
    qualified: {
      bytes: oracleRootBuild.bytes.length,
      sha256: sha256(oracleRootBuild.bytes)
    }
  }
}, null, 2));
