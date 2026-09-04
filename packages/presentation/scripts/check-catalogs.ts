import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { EN_CATALOG } from '../src/catalogs/en.js';
import { ZH_HANS_CATALOG } from '../src/catalogs/zh-Hans.js';
import { SAMPLE_IDS, UI_MESSAGE_IDS } from '../src/schema.js';

function keys(value: object): string[] { return Object.keys(value).sort(); }

function exactKeys(label: string, source: object, target: object): void {
  const expected = keys(source);
  const actual = keys(target);
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    const missing = expected.filter(key => !actual.includes(key));
    const extra = actual.filter(key => !expected.includes(key));
    throw new Error(`${label} catalog coverage mismatch; missing=${missing.join(',')} extra=${extra.join(',')}`);
  }
}

function placeholders(value: string): string[] {
  return [...value.matchAll(/\{([A-Za-z][A-Za-z0-9]*)\}/g)].map(match => match[1]!).sort();
}

function canonicalSource(): string {
  const rows: string[] = [];
  for (const namespace of ['ui', 'samples', 'pos', 'fields', 'conjugations', 'suffixes'] as const) {
    for (const key of keys(EN_CATALOG[namespace])) {
      rows.push(`${namespace}.${key}\0${String(EN_CATALOG[namespace][key as never])}`);
    }
  }
  return rows.join('\n');
}

exactKeys('ui schema', Object.fromEntries(UI_MESSAGE_IDS.map(id => [id, true])), EN_CATALOG.ui);
exactKeys('sample schema', Object.fromEntries(SAMPLE_IDS.map(id => [id, true])), EN_CATALOG.samples);

for (const namespace of ['ui', 'samples', 'pos', 'fields', 'conjugations', 'suffixes'] as const) {
  exactKeys(`zh-Hans ${namespace}`, EN_CATALOG[namespace], ZH_HANS_CATALOG[namespace]);
  for (const key of keys(EN_CATALOG[namespace])) {
    const source = String(EN_CATALOG[namespace][key as never]);
    const translation = String(ZH_HANS_CATALOG[namespace][key as never]);
    if (!source.trim() || !translation.trim()) throw new Error(`${namespace}.${key} is empty`);
    if (JSON.stringify(placeholders(source)) !== JSON.stringify(placeholders(translation))) {
      throw new Error(`${namespace}.${key} does not preserve placeholders`);
    }
  }
}

const sourceHash = createHash('sha256').update(canonicalSource()).digest('hex');
if (process.argv.includes('--print-source-hash')) {
  console.log(sourceHash);
  process.exit(0);
}

const review = JSON.parse(await readFile(new URL('../review/zh-Hans.json', import.meta.url), 'utf8')) as {
  locale?: string;
  status?: string;
  reviewer?: string;
  sourceHash?: string;
};
if (review.locale !== 'zh-Hans' || review.status !== 'reviewed' || !review.reviewer) {
  throw new Error('zh-Hans must have a named, reviewed LQA record');
}
if (review.sourceHash !== sourceHash) {
  throw new Error(`zh-Hans review is stale; expected sourceHash ${sourceHash}`);
}
console.log(`catalogs: ${sourceHash} (${keys(EN_CATALOG.ui).length} UI, ${keys(EN_CATALOG.pos).length} POS, ${keys(EN_CATALOG.fields).length} fields, ${keys(EN_CATALOG.suffixes).length} suffixes)`);
