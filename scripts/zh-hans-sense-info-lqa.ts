#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, relative, resolve } from 'node:path';

import { compileCanonicalRoots } from '../packages/data/src/source-compiler/canonical-roots.js';
import { verifySourceCompilerLock } from '../packages/data/src/source-compiler/source-lock.js';
import { parseZhHansSenseInfoCatalog } from '../packages/data/src/source-compiler/zh-hans-sense-info.js';
import {
  analyzeZhHansSenseInfoLqa,
  buildZhHansSenseInfoRuleOutput,
  parseZhHansSenseInfoRuleOutput,
  type ZhHansSenseInfoRuleMatch,
  type ZhHansSenseInfoRuleOutput
} from '../packages/data/src/source-compiler/zh-hans-sense-info-lqa.js';
import { ZH_HANS_SENSE_INFO_PATTERN_POLICY } from '../packages/data/src/source-compiler/zh-hans-sense-info-patterns.js';

const repository = resolve(import.meta.dir, '..');
const [outputArgument, rulesArgument, ...extraArguments] = process.argv.slice(2);
if (!outputArgument || extraArguments.length > 0) {
  throw new Error(
    'usage: bun scripts/zh-hans-sense-info-lqa.ts <output.json> [additional-rules.json]'
  );
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
const catalogBytes = await readFile(lock.inputs.zhHansSenseInfo.absolutePath);
const catalog = parseZhHansSenseInfoCatalog(JSON.parse(catalogBytes.toString('utf8')));

const builtInRuleOutput = buildZhHansSenseInfoRuleOutput(roots.entries);
let ruleOutput: ZhHansSenseInfoRuleOutput = builtInRuleOutput;
let additionalRuleIdentity: Readonly<Record<string, string>> | null = null;
if (rulesArgument !== undefined) {
  const rulePath = resolve(process.cwd(), rulesArgument);
  const ruleBytes = await readFile(rulePath);
  const additional = parseZhHansSenseInfoRuleOutput(JSON.parse(ruleBytes.toString('utf8')));
  const distinctMatches = new Map<string, ZhHansSenseInfoRuleMatch>();
  for (const match of [...builtInRuleOutput.matches, ...additional.matches]) {
    distinctMatches.set(`${match.source}\u0000${match.ruleId}\u0000${match.target}`, match);
  }
  ruleOutput = {
    ...builtInRuleOutput,
    matches: [...distinctMatches.values()]
  };
  additionalRuleIdentity = {
    path: relative(repository, rulePath),
    sha256: createHash('sha256').update(ruleBytes).digest('hex')
  };
}

const report = analyzeZhHansSenseInfoLqa(roots.entries, catalog, ruleOutput);
await mkdir(dirname(output), { recursive: true });
await writeFile(output, `${JSON.stringify({
  ...report,
  generatedFrom: {
    jmdict: { id: lock.inputs.jmdict.id, sha256: lock.inputs.jmdict.sha256 },
    catalog: {
      id: lock.inputs.zhHansSenseInfo.id,
      sha256: createHash('sha256').update(catalogBytes).digest('hex')
    },
    deterministicRules: {
      builtInPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
      additionalOutput: additionalRuleIdentity
    }
  }
}, null, 2)}\n`);
process.stdout.write(`${output}\n`);
