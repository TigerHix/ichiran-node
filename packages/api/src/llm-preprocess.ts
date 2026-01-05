/**
 * LLM Preprocessing for Japanese text normalization and named entity extraction
 * Uses OpenRouter API (or any OpenAI-compatible endpoint)
 *
 * Ported from nemu's convex/japanese_learning.ts
 */

import { LRUCache } from 'lru-cache';
import type { EntityHint } from '@ichiran/core';

export interface NormalizeResult {
  normalized: string;
  properNouns: string[];
}

// Cache normalize results to avoid redundant LLM calls
const normalizeCache = new LRUCache<string, NormalizeResult>({ max: 1000 });

const NORMALIZE_SCHEMA = {
  type: 'object',
  properties: {
    normalized: {
      type: 'string',
      description: `
Normalized Japanese text with natural 、(touten) placement.
- Add 、only where a native writer would to improve readability (clause boundaries, after topic markers like は/も, before conjunctions).
- Do NOT tokenize every word; keep it natural.
- Do NOT add 、around existing punctuation (。！？、).
- Do NOT add copulas or endings (no だ／です／だよ).
- Remove stutters only (ぼ、ぼく→ぼく; あ、あの→あの), but do NOT remove discourse/filler words like あの／えっと.
- If elongation is present, normalize it without deleting the word (あのー→あの; えっとー→えっと).
- Collapse emphasis (ー/〜/repeated っ・kana).
- Reduce repeats (!!!→! ???→? ……→……).
- Dialect → standard Japanese (no appended copulas).
- No paraphrasing; keep meaning intact.
- Keep newlines exactly as in input (do not add or remove any).
      `.trim()
    },
    proper_nouns: {
      type: 'array',
      items: { type: 'string' },
      description: `
Proper nouns (people, places, orgs, titles, etc.) from ANY language.
- Must appear in the NORMALIZED text (surface-form substring match, ignoring inserted 、).
- Keep exact surface form (same spelling/case).
- Do NOT include furigana/ruby-only strings.
- Return [] if none.
      `.trim()
    }
  },
  required: ['normalized', 'proper_nouns']
} as const;

const NORMALIZE_PROMPT = `
Normalize the Japanese text and extract proper nouns.

NORMALIZATION:
- Add 、only where natural (clause breaks, after は/も topic markers, before conjunctions). NOT between every word.
- Clean stutters (e.g. あ、あの→あの) but do NOT remove filler/discourse words like あの／えっと.
- If elongation is present, normalize it without deleting the word (あのー→あの; えっとー→えっと).
- Collapse emphasis, reduce excessive punctuation.
- Keep the original meaning; no paraphrasing.
- Keep newlines exactly as in input (do not add or remove any).
- The last sentence could be user typing, so it might not be a complete sentence. If you think that is the case, keep the last sentence as is.

PROPER NOUNS:
- Return proper nouns that appear in your normalized output.
- Unique list (no duplicates).
- Focus on named entities (names, places, orgs, series titles).
- If unsure, omit.

TEXT:
`.trim();

interface OpenRouterResponse {
  choices: Array<{
    message: { content: string }
  }>;
  usage?: {
    prompt_tokens?: number;
    completion_tokens?: number;
    total_tokens?: number;
  };
  model?: string;
}

interface LLMCallResult {
  normalized: string;
  proper_nouns: string[];
  latencyMs: number;
  tokens?: {
    prompt: number;
    completion: number;
    total: number;
  };
  model: string;
}

async function callOpenRouter(
  text: string,
  apiKey: string,
  model: string
): Promise<LLMCallResult> {
  const startTime = performance.now();

  const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${apiKey}`,
    },
    body: JSON.stringify({
      model,
      messages: [
        {
          role: 'user',
          content: `${NORMALIZE_PROMPT}\n${text}`
        }
      ],
      response_format: {
        type: 'json_schema',
        json_schema: {
          name: 'normalize_response',
          strict: true,
          schema: NORMALIZE_SCHEMA
        }
      }
    })
  });

  const latencyMs = Math.round(performance.now() - startTime);

  if (!response.ok) {
    const errorText = await response.text();
    console.error(`[LLM] OpenRouter error after ${latencyMs}ms:`, errorText.substring(0, 500));
    throw new Error(`OpenRouter API error (${response.status}): ${errorText}`);
  }

  const data = await response.json() as OpenRouterResponse;

  const content = data.choices?.[0]?.message?.content;
  if (!content) {
    throw new Error('No response content from OpenRouter');
  }

  const parsed = JSON.parse(content) as { normalized: string; proper_nouns: string[] };

  return {
    ...parsed,
    latencyMs,
    tokens: data.usage ? {
      prompt: data.usage.prompt_tokens ?? 0,
      completion: data.usage.completion_tokens ?? 0,
      total: data.usage.total_tokens ?? 0
    } : undefined,
    model: data.model ?? model
  };
}


export interface NormalizeStats {
  latencyMs: number;
  tokens?: { prompt: number; completion: number; total: number };
  model: string;
  cached: boolean;
  retries: number;
}

export interface NormalizeResultWithStats extends NormalizeResult {
  stats: NormalizeStats;
}

/**
 * Normalize Japanese text and extract proper nouns using LLM
 */
export async function normalizeText(
  text: string,
  apiKey: string,
  model: string
): Promise<NormalizeResultWithStats> {
  const clean = (text ?? '').trim();
  if (!clean) {
    return {
      normalized: clean,
      properNouns: [],
      stats: { latencyMs: 0, model, cached: false, retries: 0 }
    };
  }

  const cached = normalizeCache.get(clean);
  if (cached) {
    console.log('[LLM] cache hit', { inputLen: clean.length });
    return {
      ...cached,
      stats: { latencyMs: 0, model, cached: true, retries: 0 }
    };
  }

  let retryCount = 0;
  let lastResult: LLMCallResult | undefined;

  const run = async (): Promise<NormalizeResult> => {
    console.log('[LLM] normalize start', {
      inputLen: clean.length,
      model,
      attempt: retryCount + 1
    });

    const result = await callOpenRouter(clean, apiKey, model);
    lastResult = result;

    // Sanitize proper nouns: unique, non-empty, must exist in normalized text (ignoring 、)
    const normalizedForCheck = result.normalized.replace(/、/g, '');
    const seen = new Set<string>();
    const properNouns: string[] = [];

    for (const raw of result.proper_nouns ?? []) {
      const s = (raw ?? '').trim();
      if (!s) continue;
      if (!normalizedForCheck.includes(s)) continue;
      if (seen.has(s)) continue;
      seen.add(s);
      properNouns.push(s);
    }

    const changed = result.normalized.trim() !== clean;
    console.log('[LLM] normalize done', {
      latencyMs: result.latencyMs,
      tokens: result.tokens,
      model: result.model,
      inputLen: clean.length,
      outputLen: result.normalized.length,
      changed,
      properNouns: properNouns.length > 0 ? properNouns : undefined
    });

    return { normalized: result.normalized.trim(), properNouns };
  };

  const runWithRetryTracking = async (): Promise<NormalizeResult> => {
    let lastErr: unknown = null;
    for (let attempt = 0; attempt <= 2; attempt++) {
      try {
        return await run();
      } catch (err) {
        lastErr = err;
        retryCount = attempt;
        if (attempt < 2) {
          console.warn(`[LLM] retry ${attempt + 1}/2 after error:`, err instanceof Error ? err.message : err);
          await new Promise(r => setTimeout(r, 200 * (attempt + 1)));
        }
      }
    }
    throw lastErr instanceof Error ? lastErr : new Error(String(lastErr));
  };

  const result = await runWithRetryTracking();
  normalizeCache.set(clean, result);

  return {
    ...result,
    stats: {
      latencyMs: lastResult?.latencyMs ?? 0,
      tokens: lastResult?.tokens,
      model: lastResult?.model ?? model,
      cached: false,
      retries: retryCount
    }
  };
}

/**
 * Build entity hints from proper nouns
 * Finds all occurrences of each proper noun in the text
 */
export function buildEntities(text: string, properNouns: string[]): EntityHint[] {
  const entities: EntityHint[] = [];
  for (const noun of properNouns) {
    if (!noun) continue;
    let startIndex = 0;
    // Find all occurrences of this proper noun in the text
    while ((startIndex = text.indexOf(noun, startIndex)) !== -1) {
      entities.push({
        start: startIndex,
        end: startIndex + noun.length,
        boost: 1000
      });
      startIndex += noun.length;
    }
  }
  return entities;
}

/**
 * Check if LLM preprocessing is enabled via environment
 */
export function isLLMEnabled(): boolean {
  return process.env.USE_LLM === 'true';
}

/**
 * Get LLM configuration from environment
 */
export function getLLMConfig(): { apiKey: string; model: string } | null {
  const apiKey = process.env.OPENROUTER_API_KEY;
  const model = process.env.OPENROUTER_MODEL;

  if (!apiKey || !model) {
    return null;
  }

  return { apiKey, model };
}

export interface PreprocessResult {
  text: string;
  entities: EntityHint[];
  wasNormalized: boolean;
  properNouns: string[];
  stats?: NormalizeStats;
}

/**
 * Preprocess text using LLM if enabled
 * Returns original text and empty entities if LLM is disabled
 */
export async function preprocessText(text: string): Promise<PreprocessResult> {
  if (!isLLMEnabled()) {
    return { text, entities: [], wasNormalized: false, properNouns: [] };
  }

  const config = getLLMConfig();
  if (!config) {
    console.warn('[LLM] USE_LLM=true but OPENROUTER_API_KEY or OPENROUTER_MODEL not set');
    return { text, entities: [], wasNormalized: false, properNouns: [] };
  }

  try {
    const { normalized, properNouns, stats } = await normalizeText(text, config.apiKey, config.model);
    const entities = buildEntities(normalized, properNouns);
    const wasNormalized = normalized !== text;
    return { text: normalized, entities, wasNormalized, properNouns, stats };
  } catch (err) {
    console.error('[LLM] preprocessing failed, using original text:', err);
    return { text, entities: [], wasNormalized: false, properNouns: [] };
  }
}

