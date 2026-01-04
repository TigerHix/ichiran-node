import { readdirSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadBunproGrammarItemWithOptions } from '../src/data/bunpro/index.ts';
import { GinzaClient } from '../src/ginza/client.ts';
import type { BunproLevel } from '../src/types.ts';

type KeySummary = {
  count: number;
  types: Record<string, number>;
  samples: string[];
};

function parseArg(name: string): string | undefined {
  const idx = process.argv.indexOf(name);
  if (idx === -1) return undefined;
  return process.argv[idx + 1];
}

function parseIntArg(name: string, fallback: number): number {
  const v = parseArg(name);
  if (!v) return fallback;
  const n = Number.parseInt(v, 10);
  return Number.isFinite(n) ? n : fallback;
}

function parseCsvArg(name: string): string[] | undefined {
  const v = parseArg(name);
  if (!v) return undefined;
  return v
    .split(',')
    .map((s) => s.trim())
    .filter(Boolean);
}

function addKey(summary: Record<string, KeySummary>, key: string, value: unknown) {
  const s = (summary[key] ??= { count: 0, types: {}, samples: [] });
  s.count++;
  const t = value === null ? 'null' : Array.isArray(value) ? 'array' : typeof value;
  s.types[t] = (s.types[t] ?? 0) + 1;
  const repr =
    value === true ? 'true' : value === false ? 'false' : value === null ? 'null' : String(value ?? '');
  if (repr && s.samples.length < 25 && !s.samples.includes(repr)) s.samples.push(repr);
}

function collectFromRecord(summary: Record<string, KeySummary>, rec: Record<string, unknown> | undefined) {
  if (!rec) return;
  for (const [k, v] of Object.entries(rec)) addKey(summary, k, v);
}

function takeOtherKeys<T extends Record<string, unknown>>(rec: T, known: Set<string>): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(rec)) {
    if (known.has(k)) continue;
    out[k] = v;
  }
  return out;
}

const levels =
  (parseCsvArg('--levels') as BunproLevel[] | undefined) ??
  (['JLPT5', 'JLPT4', 'JLPT3', 'JLPT2', 'JLPT1', 'Non-JLPT'] as BunproLevel[]);
const perItem = parseIntArg('--perItem', 2);
const maxSentences = parseIntArg('--maxSentences', 2000);
const batchSize = parseIntArg('--batch', 64);

const bunproDir = fileURLToPath(new URL('../data/bunpro', import.meta.url));

const sentences: string[] = [];
let filesVisited = 0;
let itemsLoaded = 0;

for (const level of levels) {
  const dir = join(bunproDir, level);
  const files = readdirSync(dir)
    .filter((f) => f.endsWith('.json'))
    .sort((a, b) => a.localeCompare(b, 'en'));
  for (const f of files) {
    if (sentences.length >= maxSentences) break;
    filesVisited++;
    const item = loadBunproGrammarItemWithOptions(join(dir, f), level, { allowTrivialSlug: true });
    if (!item) continue;
    itemsLoaded++;
    for (const s of item.sentences.slice(0, perItem)) {
      if (sentences.length >= maxSentences) break;
      sentences.push(s.sentence);
    }
  }
}

const featsKeys: Record<string, KeySummary> = {};
const miscKeys: Record<string, KeySummary> = {};

// Also collect "unknown" keys excluding the ones we currently write into misc.
const knownMisc = new Set(['Inf', 'Reading', 'NE', 'ENE', 'BunsetuBILabel', 'BunsetuPositionType', 'ClauseHead']);
const otherMiscKeys: Record<string, KeySummary> = {};

const client = new GinzaClient({ python: 'python3' });
await client.start();
const meta = await client.meta();
try {
  for (let i = 0; i < sentences.length; i += batchSize) {
    const batch = sentences.slice(i, i + batchSize);
    const docs = await client.analyze(batch);
    for (const doc of docs) {
      for (const sent of doc.sentences) {
        for (const tok of sent.tokens) {
          // Raw-ish worker output keys (named `feats` + `misc` in transport).
          collectFromRecord(featsKeys, tok.feats);
          collectFromRecord(miscKeys, tok.misc);
          if (tok.misc) {
            collectFromRecord(otherMiscKeys, takeOtherKeys(tok.misc, knownMisc));
          }
        }
      }
    }
  }
} finally {
  await client.stop();
}

const out = {
  meta,
  config: { levels, perItem, maxSentences, batchSize },
  corpus: { filesVisited, itemsLoaded, sentences: sentences.length },
  featsKeys,
  miscKeys,
  otherMiscKeys,
};

process.stdout.write(JSON.stringify(out, null, 2) + '\n');


