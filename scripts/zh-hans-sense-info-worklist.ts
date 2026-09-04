#!/usr/bin/env bun

import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';

import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';
import { parseZhHansSenseInfoCatalog } from '../packages/data/src/source-compiler/zh-hans-sense-info.js';
import { buildZhHansSenseInfoWorklist } from '../packages/data/src/source-compiler/zh-hans-sense-info-worklist.js';

const repository = resolve(import.meta.dir, '..');
const outputArgument = process.argv[2];
if (!outputArgument || process.argv.length !== 3) {
  throw new Error('usage: bun scripts/zh-hans-sense-info-worklist.ts <output.json>');
}
const output = resolve(process.cwd(), outputArgument);
const lock = await verifySourceCompilerLock(
  repository,
  'data/source-compiler-update-2026-09-02.lock.json'
);
const roots = await compileCanonicalRoots({
  jmdict: lock.inputs.jmdict.absolutePath,
  jmdictSourceId: lock.inputs.jmdict.id,
  extra: lock.inputs.extra.absolutePath,
  municipality: lock.inputs.municipality.absolutePath,
  ward: lock.inputs.ward.absolutePath,
  errata: lock.inputs.chronologicalErrata.absolutePath,
  compatibility: lock.inputs.compatibility.absolutePath
});
const catalog = parseZhHansSenseInfoCatalog(JSON.parse(await readFile(
  lock.inputs.zhHansSenseInfo.absolutePath,
  'utf8'
)));
const worklist = buildZhHansSenseInfoWorklist(roots.entries, catalog);
await mkdir(dirname(output), { recursive: true });
await writeFile(output, `${JSON.stringify({
  formatVersion: 1,
  locale: 'zh-Hans',
  generatedFrom: {
    jmdict: { id: lock.inputs.jmdict.id, sha256: lock.inputs.jmdict.sha256 },
    catalog: { id: lock.inputs.zhHansSenseInfo.id, sha256: lock.inputs.zhHansSenseInfo.sha256 }
  },
  untranslatedUniqueCount: worklist.length,
  untranslatedOccurrenceCount: worklist.reduce((sum, item) => sum + item.occurrenceCount, 0),
  items: worklist
}, null, 2)}\n`);
process.stdout.write(`${output}\n`);
