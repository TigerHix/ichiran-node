import { createHash } from 'node:crypto';
import { writeFile } from 'node:fs/promises';
import { userInfo } from 'node:os';
import { resolve } from 'node:path';
import postgres from 'postgres';
import {
  buildLexiconStore
} from '../packages/data/src/browser-pack/lexicon.js';
import {
  buildLocaleGlossStore
} from '../packages/data/src/browser-pack/locale-gloss.js';
import { loadDictionaryEntries } from '../packages/data/src/browser-pack/dictionary-oracle.js';
import {
  buildRootPayload,
  type RootPayloadFormSource
} from '../packages/data/src/browser-pack/root-payload.js';
import { loadRootPayloadSource } from '../packages/data/src/browser-pack/root-payload-oracle.js';
import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import {
  canonicalEnglishLocaleEntries,
  canonicalLexiconEntries,
  canonicalRootPayloadSource
} from '../packages/data/src/source-compiler/pack-input.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';

const database = process.argv[2];
if (!database) {
  throw new Error('Usage: bun scripts/source-compiler-root-proof.ts DATABASE [ORDER_EVIDENCE_JSON]');
}

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

const repository = resolve(import.meta.dir, '..');
const sourceLock = await verifySourceCompilerLock(repository);
const compilation = await compileCanonicalRoots({
  jmdict: sourceLock.inputs.jmdict.absolutePath,
  jmdictSourceId: sourceLock.inputs.jmdict.id,
  extra: sourceLock.inputs.extra.absolutePath,
  municipality: sourceLock.inputs.municipality.absolutePath,
  ward: sourceLock.inputs.ward.absolutePath,
  errata: sourceLock.inputs.chronologicalErrata.absolutePath,
  compatibility: sourceLock.inputs.compatibility.absolutePath
});
const sourceLexicon = canonicalLexiconEntries(compilation.entries);
const sourceEnglish = canonicalEnglishLocaleEntries(compilation.entries);
const sourceRoot = canonicalRootPayloadSource(compilation.entries);

const sql = postgres({
  database,
  host: process.env.PGHOST ?? '/var/run/postgresql',
  user: process.env.PGUSER ?? userInfo().username
});
const [oracleDictionary, oracleRoot] = await Promise.all([
  loadDictionaryEntries(sql),
  loadRootPayloadSource(sql)
]);
await sql.end();

let exactDetails = 0;
const detailDeltas: number[] = [];
const oracleLexicon = oracleDictionary.lexicon;
const oracleEnglish = oracleDictionary.english;
for (let index = 0; index < Math.max(sourceLexicon.length, oracleLexicon.length); index++) {
  const structureMatches = JSON.stringify(sourceLexicon[index]) === JSON.stringify(oracleLexicon[index]);
  const englishMatches = JSON.stringify(sourceEnglish[index]) === JSON.stringify(oracleEnglish[index]);
  if (structureMatches && englishMatches) exactDetails++;
  else detailDeltas.push(sourceLexicon[index]?.seq ?? oracleLexicon[index]!.seq);
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
const sourceLexiconBuild = buildLexiconStore(sourceLexicon);
const oracleLexiconBuild = buildLexiconStore(oracleLexicon);
const sourceLexiconSha256 = sha256(sourceLexiconBuild.bytes);
const oracleLexiconSha256 = sha256(oracleLexiconBuild.bytes);
const sourceEnglishBuild = buildLocaleGlossStore({
  locale: 'en', lexiconSha256: sourceLexiconSha256, entries: sourceEnglish
});
const oracleEnglishBuild = buildLocaleGlossStore({
  locale: 'en', lexiconSha256: oracleLexiconSha256, entries: oracleEnglish
});
const orderEvidence = `${orderDeltas.map(row => JSON.stringify(row)).join('\n')}\n`;
if (process.argv[3]) await writeFile(process.argv[3], orderEvidence);

console.log(JSON.stringify({
  canonical: {
    sourceLockSha256: sourceLock.sha256,
    entries: compilation.entries.length,
    compatibilityRows: compilation.compatibility.rows.length,
    customEdits: compilation.custom.edits.length,
    errata: compilation.errata.counts
  },
  dictionary: {
    sourceEntries: sourceLexicon.length,
    qualifiedEntries: oracleLexicon.length,
    exactEntries: exactDetails,
    deltaEntries: detailDeltas,
    lexicon: {
      byteEqual: Buffer.compare(sourceLexiconBuild.bytes, oracleLexiconBuild.bytes) === 0,
      bytes: sourceLexiconBuild.bytes.length,
      sha256: sourceLexiconSha256
    },
    english: {
      byteEqual: Buffer.compare(sourceEnglishBuild.bytes, oracleEnglishBuild.bytes) === 0,
      bytes: sourceEnglishBuild.bytes.length,
      sha256: sha256(sourceEnglishBuild.bytes)
    }
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
