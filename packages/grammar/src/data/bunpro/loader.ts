import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
import type { BunproGrammarItem, BunproLevel, BunproIndex } from './types.js';

// Resolve data directory relative to this module
function getBunproDataDir(): string {
  let dir = dirname(fileURLToPath(import.meta.url));
  // Walk up to find package root (works from both src/ and dist/)
  for (let i = 0; i < 8; i++) {
    const candidate = join(dir, 'data', 'bunpro', '_index.json');
    if (existsSync(candidate)) return join(dir, 'data', 'bunpro');
    dir = dirname(dir);
  }
  throw new Error('Could not find bunpro data directory');
}

let cachedIndex: BunproIndex | null = null;
let cachedDataDir: string | null = null;

export function getBunproIndex(): BunproIndex {
  if (cachedIndex) return cachedIndex;
  cachedDataDir = getBunproDataDir();
  cachedIndex = JSON.parse(readFileSync(join(cachedDataDir, '_index.json'), 'utf8')) as BunproIndex;
  return cachedIndex;
}

export function loadBunproRaw(grammarId: string): any | null {
  const index = getBunproIndex();
  const entry = index[grammarId];
  if (!entry) return null;
  const dataDir = cachedDataDir ?? getBunproDataDir();
  const filePath = join(dataDir, entry.level, entry.filename);
  try {
    return JSON.parse(readFileSync(filePath, 'utf8'));
  } catch {
    return null;
  }
}

const BunproSchema = z.object({
  data: z.object({
    attributes: z.object({
      slug: z.string(),
      title: z.string().optional(),
      meaning: z.string().optional(),
    }),
  }),
  included: z.array(z.any()).optional(),
});

function cleanHtml(text: string): string {
  return text
    .replace(/<[^>]+>/g, '')
    .replace(/（[^）]*）/g, '')
    .replace(/\([^)]*\)/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}

function extractSentence(content: string, answer: string): string {
  return cleanHtml(content.replaceAll('____', answer));
}

function isNonTrivialSlug(slug: string): boolean {
  const bad = new Set(['は', 'が', 'を', 'に', 'で', 'と', 'の', 'も', 'へ', 'や', 'か']);
  if (bad.has(slug)) return false;
  if (slug.length <= 1) return false;
  return true;
}

export function loadBunproGrammarItem(filePath: string, level: BunproLevel): BunproGrammarItem | null {
  return loadBunproGrammarItemWithOptions(filePath, level, {});
}

export function loadBunproGrammarItemWithOptions(
  filePath: string,
  level: BunproLevel,
  opts: { allowTrivialSlug?: boolean }
): BunproGrammarItem | null {
  const raw = readFileSync(filePath, 'utf8');
  const parsed = BunproSchema.safeParse(JSON.parse(raw));
  if (!parsed.success) return null;

  const attrs = parsed.data.data.attributes;
  const slug = attrs.slug;
  if (!opts.allowTrivialSlug && !isNonTrivialSlug(slug)) return null;

  const answerForms = new Set<string>();
  const sentences: Array<{ sentence: string; answer: string }> = [];

  for (const item of parsed.data.included ?? []) {
    if (item?.type !== 'study_question') continue;
    const a = item?.attributes ?? {};
    const content = typeof a.content === 'string' ? a.content : '';
    const answer = typeof a.answer === 'string' ? a.answer : '';
    const alternates = Array.isArray(a.alternate_grammar) ? a.alternate_grammar : [];
    const usedIn = typeof a.used_in === 'string' ? a.used_in : '';

    // Only include sentences marked as "examples" - exclude "writeups" examples
    // which are illustrative examples in grammar explanations, not test cases
    if (usedIn !== 'examples') continue;

    if (answer) answerForms.add(cleanHtml(answer));
    for (const alt of alternates) {
      if (typeof alt === 'string' && alt.trim()) answerForms.add(cleanHtml(alt));
    }

    if (content && answer) {
      const sentence = extractSentence(content, answer);
      if (sentence.length > 3) sentences.push({ sentence, answer: cleanHtml(answer) });
    }
  }

  const answerFormsList = [...answerForms].filter(Boolean);
  if (answerFormsList.length === 0 || sentences.length === 0) return null;

  return {
    id: slug,
    level,
    title: attrs.title,
    meaning: attrs.meaning,
    answerForms: answerFormsList,
    sentences: sentences.slice(0, 20),
  };
}

export function sampleBunproGrammars(params: {
  bunproDir: string;
  perLevel: number;
  levels: BunproLevel[];
}): BunproGrammarItem[] {
  const { bunproDir, perLevel, levels } = params;
  const out: BunproGrammarItem[] = [];

  for (const level of levels) {
    const dir = join(bunproDir, level);
    const files = readdirSync(dir)
      .filter((f) => f.endsWith('.json'))
      .sort((a, b) => a.localeCompare(b, 'en'));

    let picked = 0;
    for (const f of files) {
      if (picked >= perLevel) break;
      const item = loadBunproGrammarItem(join(dir, f), level);
      if (!item) continue;
      out.push(item);
      picked++;
    }

    if (picked < perLevel) {
      throw new Error(`Could not sample ${perLevel} grammars for ${level} (filtered too aggressively?)`);
    }
  }

  return out;
}

