import type { Token, MatchHit, MatchOptions, MatchOutcome, PredicateContext } from './types.js';
import type { CompiledGrammar } from './runtime.js';
import { evaluatePredicates } from './predicates.js';
import { buildSegmentsFromTokens } from './segments.js';
import { performance } from 'node:perf_hooks';

/**
 * Cache structure for precomputed start gate checks.
 */
interface StartGateCache {
  anyTokenMatches: Map<string, boolean>;  // grammarId -> has any token match
  nearMatches: Map<string, boolean[]>;    // grammarId -> boolean array per token position
}

/**
 * Precompute start gate helpers for all grammars to avoid O(n^2) checks.
 * - anyToken: evaluate once across all tokens per grammar
 * - near: build a boolean array indicating which positions match any predicate
 */
async function precomputeStartGates(grammars: CompiledGrammar[], tokens: Token[]): Promise<StartGateCache> {
  const anyTokenMatches = new Map<string, boolean>();
  const nearMatches = new Map<string, boolean[]>();

  for (const compiled of grammars) {
    const gate = compiled.def.startGate;
    if (!gate) continue;

    // Precompute anyToken checks
    if (compiled.anyTokenPreds && compiled.anyTokenPreds.length > 0) {
      const preds = compiled.anyTokenPreds;
      let found = false;
      for (let i = 0; i < tokens.length && !found; i++) {
        const ctx: PredicateContext = { tokens, index: i, prev: tokens[i - 1], next: tokens[i + 1] };
        for (const pred of preds) {
          if (await pred(tokens[i], ctx)) {
            found = true;
            break;
          }
        }
      }
      anyTokenMatches.set(compiled.def.id, found);
    }

    // Precompute near checks: build boolean array indicating which positions match
    if (compiled.nearPreds && compiled.nearPreds.length > 0) {
      const preds = compiled.nearPreds;
      const matchArray: boolean[] = new Array(tokens.length).fill(false);
      
      for (let i = 0; i < tokens.length; i++) {
        const ctx: PredicateContext = { tokens, index: i, prev: tokens[i - 1], next: tokens[i + 1] };
        for (const pred of preds) {
          if (await pred(tokens[i], ctx)) {
            matchArray[i] = true;
            break;
          }
        }
      }
      nearMatches.set(compiled.def.id, matchArray);
    }
  }

  return { anyTokenMatches, nearMatches };
}

/**
 * Evaluate start gates using precomputed cache where possible.
 */
async function passesStartGatesWithCache(
  compiled: CompiledGrammar,
  tokens: Token[],
  startIndex: number,
  cache: StartGateCache
): Promise<boolean> {
  const gate = compiled.def.startGate;
  if (!gate) return true;

  // firstToken: all predicates on the start token must pass (AND semantics)
  if (compiled.firstTokenPreds && compiled.firstTokenPreds.length > 0) {
    const startToken = tokens[startIndex];
    if (!startToken) return false;
    const ctx: PredicateContext = { tokens, index: startIndex, prev: tokens[startIndex - 1], next: tokens[startIndex + 1] };
    const ok = await evaluatePredicates(startToken, ctx, compiled.firstTokenPreds);
    if (!ok) return false;
  }

  // anyToken: use precomputed result
  if (compiled.anyTokenPreds && compiled.anyTokenPreds.length > 0) {
    const found = cache.anyTokenMatches.get(compiled.def.id);
    if (!found) return false;
  }

  // near: use precomputed match array and check window
  if (compiled.nearPreds && compiled.nearPreds.length > 0 && compiled.nearWindow) {
    const matchArray = cache.nearMatches.get(compiled.def.id);
    if (!matchArray) return false;
    
    const { left, right } = compiled.nearWindow;
    const start = Math.max(0, startIndex - left);
    const end = Math.min(tokens.length, startIndex + right + 1);
    
    let ok = false;
    for (let i = start; i < end; i++) {
      if (matchArray[i]) {
        ok = true;
        break;
      }
    }
    if (!ok) return false;
  }

  return true;
}

/**
 * Select the best outcome from multiple matches using deterministic ranking:
 * 1. Longest span (outcome.index - startIndex)
 * 2. Higher preference score
 * 3. More captures
 * 
 * Returns null if no valid outcome found (all outcomes have index <= startIndex).
 */
export function selectBestOutcome(outcomes: MatchOutcome[], startIndex: number): MatchOutcome | null {
  let best: MatchOutcome | null = null;
  
  for (const outcome of outcomes) {
    // Skip outcomes that don't advance the index
    if (outcome.index <= startIndex) continue;
    
    // First valid outcome becomes the initial best
    if (!best) {
      best = outcome;
      continue;
    }

    // Calculate span lengths
    const outcomeSpan = outcome.index - startIndex;
    const bestSpan = best.index - startIndex;
    
    // 1. Prefer longest span
    if (outcomeSpan > bestSpan) {
      best = outcome;
      continue;
    }
    if (outcomeSpan < bestSpan) {
      continue;
    }
    
    // 2. Prefer higher preference (spans are equal)
    const outcomePref = outcome.preference ?? 0;
    const bestPref = best.preference ?? 0;
    if (outcomePref > bestPref) {
      best = outcome;
      continue;
    }
    if (outcomePref < bestPref) {
      continue;
    }
    
    // 3. Prefer more captures (span and preference are equal)
    if (outcome.captures.length > best.captures.length) {
      best = outcome;
    }
  }
  
  return best;
}

interface ProfileData {
  matcherMs: number;
  matcherCalls: number;
  selectBestOutcomeMs: number;
  buildSegmentsMs: number;
  matchTotalMs: number;
}

/**
 * Match grammars against a single sentence's tokens.
 * Assumes tokens represent a single sentence (no sentence splitting done here).
 * 
 * @param tokens - Filtered tokens for the sentence (punctuation already filtered)
 * @param grammars - Compiled grammar patterns to match
 * @param options - Match options
 * @param unfilteredTokens - Original unfiltered tokens for context/display
 * @param profile - Optional profile data accumulator
 * @returns Array of grammar matches
 */
export async function matchGrammars(
  tokens: Token[],
  grammars: CompiledGrammar[],
  options: MatchOptions = {},
  unfilteredTokens?: Token[],
  profile?: ProfileData
): Promise<MatchHit[]> {
  const hits: MatchHit[] = [];
  const maxMatches = options.maxMatches ?? Infinity;
  
  // Use unfiltered tokens for context if provided, otherwise use filtered tokens
  const tokensForContext = unfilteredTokens ?? tokens;

  const matchStart = profile ? performance.now() : 0;

  // Precompute start gate helpers for performance
  const startGateCache = await precomputeStartGates(grammars, tokens);

  // Match within the sentence (no sentence windowing - that's done at a higher level)
  for (const compiled of grammars) {
    const { def, matcher, minTokens } = compiled;
    for (let i = 0; i < tokens.length && hits.length < maxMatches; i++) {
      // Optimization: skip if not enough tokens remaining in this sentence
      const remaining = tokens.length - i;
      if (minTokens && remaining < minTokens) continue;
      
      // Start gates optimization (precomputed where possible)
      if (!(await passesStartGatesWithCache(compiled, tokens, i, startGateCache))) continue;

      const matcherStart = profile ? performance.now() : 0;
      const outcomes = await matcher(tokens, i);
      if (profile) {
        profile.matcherMs += (performance.now() - matcherStart);
        profile.matcherCalls++;
      }
      const selectStart = profile ? performance.now() : 0;
      const outcome = selectBestOutcome(outcomes, i);
      if (profile) {
        profile.selectBestOutcomeMs += (performance.now() - selectStart);
      }
      if (!outcome) continue;
      
      // Build captures with text, filtering out empty ones
      const captures = outcome.captures
        .map((capture) => {
          const captureTokens = tokens.slice(capture.start, capture.end);
          const text = captureTokens.map(t => t.text).join('');
          return {
            ...capture,
            tokens: captureTokens,
            text,
          };
        })
        .filter(capture => capture.tokens.length > 0);
      
      // Build segments for easy rendering from tokens
      const buildSegStart = profile ? performance.now() : 0;
      const segments = buildSegmentsFromTokens(tokensForContext, captures);
      if (profile) {
        profile.buildSegmentsMs += (performance.now() - buildSegStart);
      }
      
      hits.push({
        grammarId: def.id,
        level: def.level,
        description: def.description,
        captures,
        segments,
      });
      i = outcome.index - 1;
    }
  }
  if (profile) {
    profile.matchTotalMs += (performance.now() - matchStart);
  }
  return hits;
}

