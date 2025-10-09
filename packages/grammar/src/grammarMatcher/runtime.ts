import type { GrammarDefinition, MatchHit, MatchOptions, Capture, MatchOutcome, Token, MatchSegment, PredicateSpec } from './types.js';
import { compilePattern } from './compiler.js';
import { romanizeStar, type RomanizeStarResult, transformRomanizeStarResult, type TransformedRomanizeStarResult } from '@ichiran/core';
import { augmentTokens } from './augment.js';
import { parseGrammarInfoFromToken, parseTokenAlternatives } from './parsing.js';
import { resolvePredicate, evaluatePredicates } from './predicates.js';
import { performance } from 'node:perf_hooks';

export interface CompiledGrammar {
  def: GrammarDefinition;
  matcher: ReturnType<typeof compilePattern>;
  minTokens?: number;            // lower bound on tokens required
  firstTokenGate?: PredicateSpec[]; // derived FIRST-set fallback if no explicit gate
}

export interface SegmentationAlternative {
  tokens: Token[];
  score: number;
}

// ----------------------------------------------------------------------------
// Profiling & Caching (enabled with GRAMMAR_PROFILE=1|true)
// ----------------------------------------------------------------------------
const GRAMMAR_PROFILE = process.env.GRAMMAR_PROFILE === '1' || process.env.GRAMMAR_PROFILE === 'true';

function time<T>(label: string, fn: () => T): T {
  if (!GRAMMAR_PROFILE) return fn();
  const start = performance.now();
  try {
    return fn();
  } finally {
    const ms = performance.now() - start;
    // eslint-disable-next-line no-console
    console.log(`[GRAMMAR_PROFILE] ${label}: ${ms.toFixed(2)}ms`);
  }
}

async function timeAsync<T>(label: string, fn: () => Promise<T>): Promise<T> {
  if (!GRAMMAR_PROFILE) return fn();
  const start = performance.now();
  try {
    return await fn();
  } finally {
    const ms = performance.now() - start;
    // eslint-disable-next-line no-console
    console.log(`[GRAMMAR_PROFILE] ${label}: ${ms.toFixed(2)}ms`);
  }
}

const compiledGrammarCache: Map<string, ReturnType<typeof compilePattern>> = new Map();
let compiledCacheMisses = 0;
let compiledCacheHits = 0;

interface ProfileTotals {
  romanizeStarMs: number;
  transformMs: number;
  parseTokenAlternativesMs: number;
  augmentTokensMs: number;
  compileGrammarsMs: number;
  alternativesTried: number;
  grammarCount: number;
  matcherCalls: number;
  matcherMs: number;
  selectBestOutcomeMs: number;
  matchTotalMs: number;
  buildSegmentsMs: number;
  buildCharPositionMapMs: number;
  sentenceBoundaryMs: number;
  filteredTokenCounts: number[];
  unfilteredTokenCounts: number[];
}

let currentProfile: ProfileTotals | null = null;

/**
 * Segment text using ichiran with multiple alternative segmentations.
 * 
 * @param text - Input Japanese text
 * @param limit - Number of segmentation alternatives to generate (default: 5)
 * @returns Array of segmentation alternatives, each with tokens and a score
 */
export async function segmentSentenceWithAlternatives(
  text: string,
  limit: number = 5,
  normalizePunctuation: boolean = false
): Promise<SegmentationAlternative[]> {
  // Get multiple segmentations with gloss data (includes POS tags)
  const result: RomanizeStarResult = await timeAsync('romanizeStar', () => romanizeStar(text, { limit, normalizePunctuation }));
  const transformedResult: TransformedRomanizeStarResult = await timeAsync('transformRomanizeStarResult', () => transformRomanizeStarResult(result));
  
  // Parse alternative tokenizations with their scores
  const tokenAlternativesWithScores = time('parseTokenAlternatives', () => parseTokenAlternatives(result, transformedResult));
  
  // Convert to Token format and augment
  const alternatives: SegmentationAlternative[] = [];
  for (const { tokens: romanizeStarResultTokens, score } of tokenAlternativesWithScores) {
    const tokens = romanizeStarResultTokens.map((romanizeStarResultToken) => {
      if (!romanizeStarResultToken.infoJson) {
        return {
          text: romanizeStarResultToken.word,
        } as Token;
      } else {
        return {
          text: romanizeStarResultToken.word,
          wordInfo: romanizeStarResultToken.info,
          grammarInfo: parseGrammarInfoFromToken(romanizeStarResultToken.infoJson),
        } as Token;
      }
    });
    
    // Augment tokens with POS from gloss and lemma
    await timeAsync('augmentTokens', () => augmentTokens(tokens));
    
    alternatives.push({
      tokens,
      score,
    });
  }
  
  return alternatives;
}

export function compileGrammars(defs: GrammarDefinition[]): CompiledGrammar[] {
  const start = GRAMMAR_PROFILE ? performance.now() : 0;
  const compiled = defs
    .map(def => {
      // Use id as cache key; fall back to JSON string of pattern if id missing
      const cacheKey = def.id ?? JSON.stringify(def.pattern);
      let matcher = compiledGrammarCache.get(cacheKey);
      if (!matcher) {
        matcher = compilePattern(def.pattern);
        compiledGrammarCache.set(cacheKey, matcher);
        compiledCacheMisses++;
      } else {
        compiledCacheHits++;
      }
      // Derive a conservative minTokens (best-effort): 1 for token, sum of mins for sequence, min of mins for alt, 0 for optional, min* for repeat
      const minTokens = computeMinTokens(def.pattern);
      // Use explicit startGate.firstToken only; avoid auto-derivation to prevent false negatives
      const firstTokenGate = def.startGate?.firstToken; 
      return { def, matcher, minTokens, firstTokenGate } as CompiledGrammar;
    })
    .sort((a, b) => (b.def.priority ?? 0) - (a.def.priority ?? 0));

  if (GRAMMAR_PROFILE) {
    const ms = performance.now() - start;
    if (currentProfile) {
      currentProfile.compileGrammarsMs += ms;
      currentProfile.grammarCount = defs.length;
    }
    // eslint-disable-next-line no-console
    console.log(`[GRAMMAR_PROFILE] compileGrammars: ${ms.toFixed(2)}ms (hits=${compiledCacheHits}, misses=${compiledCacheMisses})`);
  }

  return compiled;
}

// Best-effort minimum token count of a pattern
function computeMinTokens(node: any): number {
  if (!node) return 0;
  if (node.token) return 1;
  if (node.sequence) return node.sequence.reduce((sum: number, n: any) => sum + computeMinTokens(n), 0);
  if (node.alt) return node.alt.reduce((min: number, n: any) => Math.min(min, computeMinTokens(n)), Number.POSITIVE_INFINITY) || 0;
  if (node.optional) return 0;
  if (node.capture) return computeMinTokens(node.pattern);
  if (node.peek) return 0; // lookahead doesn't consume
  if (node.not) return 0;
  if (node.anchor) return 0;
  if (node.macro) return 1; // conservative default per macro
  if (node.repeat) {
    const min = node.repeat.min ?? 0;
    return min * computeMinTokens(node.repeat.pattern);
  }
  return 0;
}

// Evaluate start gates for a compiled grammar at a given start index
async function passesStartGates(compiled: CompiledGrammar, tokens: Token[], startIndex: number): Promise<boolean> {
  const gate = compiled.def.startGate;
  if (!gate) return true;

  // firstToken: all predicates on the start token must pass (AND semantics)
  if (gate.firstToken && gate.firstToken.length > 0) {
    const startToken = tokens[startIndex];
    if (!startToken) return false;
    const ctx = { tokens, index: startIndex, prev: tokens[startIndex - 1], next: tokens[startIndex + 1] } as any;
    const preds = gate.firstToken.map(resolvePredicate);
    const ok = await evaluatePredicates(startToken, ctx, preds);
    if (!ok) return false;
  }

  // anyToken: there must exist some token satisfying ANY predicate (OR semantics across specs)
  if (gate.anyToken && gate.anyToken.length > 0) {
    const preds = gate.anyToken.map(resolvePredicate);
    let found = false;
    for (let i = 0; i < tokens.length && !found; i++) {
      const ctx = { tokens, index: i, prev: tokens[i - 1], next: tokens[i + 1] } as any;
      for (const pred of preds) {
        if (await pred(tokens[i], ctx)) { found = true; break; }
      }
    }
    if (!found) return false;
  }

  // near: within window [left,right] of startIndex, there must exist a token satisfying ANY predicate
  if (gate.near && Array.isArray(gate.near.predicates) && gate.near.predicates.length > 0) {
    const preds = gate.near.predicates.map(resolvePredicate);
    const { left, right } = gate.near.window;
    const start = Math.max(0, startIndex - left);
    const end = Math.min(tokens.length, startIndex + right + 1);
    let ok = false;
    for (let i = start; i < end && !ok; i++) {
      const ctx = { tokens, index: i, prev: tokens[i - 1], next: tokens[i + 1] } as any;
      for (const pred of preds) {
        if (await pred(tokens[i], ctx)) { ok = true; break; }
      }
    }
    if (!ok) return false;
  }

  return true;
}

/**
 * Match grammars against a single sentence's tokens.
 * Assumes tokens represent a single sentence (no sentence splitting done here).
 * 
 * @param tokens - Filtered tokens for the sentence (punctuation already filtered)
 * @param grammars - Compiled grammar patterns to match
 * @param options - Match options
 * @param unfilteredTokens - Original unfiltered tokens for context/display
 * @returns Array of grammar matches
 */
export async function matchGrammars(tokens: Token[], grammars: CompiledGrammar[], options: MatchOptions = {}, unfilteredTokens?: Token[]): Promise<MatchHit[]> {
  const hits: MatchHit[] = [];
  const maxMatches = options.maxMatches ?? Infinity;
  
  // Use unfiltered tokens for context if provided, otherwise use filtered tokens
  const tokensForContext = unfilteredTokens ?? tokens;

  const localProfile = GRAMMAR_PROFILE && currentProfile ? currentProfile : null;
  const matchStart = GRAMMAR_PROFILE ? performance.now() : 0;

  // Match within the sentence (no sentence windowing - that's done at a higher level)
  for (const { def, matcher } of grammars) {
    for (let i = 0; i < tokens.length && hits.length < maxMatches; i++) {
      // Optimization: skip if not enough tokens remaining in this sentence
      const remaining = tokens.length - i;
      const compiled = grammars.find(g => g.def.id === def.id)!;
      if (compiled.minTokens && remaining < compiled.minTokens) continue;
      // Start gates optimization (async, full predicates)
      if (!(await passesStartGates(compiled, tokens, i))) continue;

      const matcherStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const outcomes = await matcher(tokens, i);
      if (localProfile) {
        localProfile.matcherMs += (performance.now() - matcherStart);
        localProfile.matcherCalls++;
      }
      const selectStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const outcome = selectBestOutcome(outcomes, i);
      if (localProfile) {
        localProfile.selectBestOutcomeMs += (performance.now() - selectStart);
      }
      if (!outcome) continue;
      
      // Build captures with text
      const captures = outcome.captures.map((capture): Capture => {
        const captureTokens = tokens.slice(capture.start, capture.end);
        const text = captureTokens.map(t => t.text).join('');
        return {
          ...capture,
          tokens: captureTokens,
          text,
          textStart: 0,
          textEnd: 0,
        };
      });
      
      // Build segments for easy rendering from tokens
      const buildSegStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const segments = buildSegmentsFromTokens(tokensForContext, captures);
      if (localProfile) {
        localProfile.buildSegmentsMs += (performance.now() - buildSegStart);
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
  if (localProfile) {
    localProfile.matchTotalMs += (performance.now() - matchStart);
  }
  return hits;
}

/**
 * Build segments from captures for easy rendering with sentence context.
 * Creates alternating raw/capture segments covering the sentence containing the match.
 * Preserves original text (including original punctuation) when available.
 * 
 * @param unfilteredTokens - Full token array with all punctuation (for context)
 * @param filteredTokens - Filtered token array used for matching (indices reference this)
 * @param captures - Captured groups from the match
 * @param startIndex - Start index of match in filteredTokens
 * @param endIndex - End index of match in filteredTokens
 * @param originalText - Original input text (preserves punctuation)
 */
// NOTE: The previous non-optimized buildSegments has been replaced by
// buildSegmentsWithPrecomputed to avoid repeated recomputation.

/**
 * Build segments directly from tokens - simple and reliable.
 * Tokens are assumed to be from a single sentence (sentence splitting happens at a higher level).
 * No character position mapping, just use token text as-is.
 */
function buildSegmentsFromTokens(
  unfilteredTokens: Token[],
  captures: Capture[]
): MatchSegment[] {
  if (captures.length === 0) {
    return [];
  }

  // Sort captures by start position
  const sortedCaptures = [...captures].sort((a, b) => a.start - b.start);
  
  // Build segments from the full sentence (tokens are already from a single sentence)
  const segments: MatchSegment[] = [];
  let currentPos = 0;
  
  for (const capture of sortedCaptures) {
    // Use capture indices directly since captures reference the same token array
    const captureStart = capture.start;
    const captureEnd = capture.end;
    
    // Validate that the capture is within bounds
    if (captureStart < 0 || captureEnd > unfilteredTokens.length) {
      continue;
    }
    
    // Add raw text before capture
    if (currentPos < captureStart) {
      const rawText = unfilteredTokens.slice(currentPos, captureStart).map(t => t.text).join('');
      if (rawText) {
        segments.push({ type: 'raw', text: rawText });
      }
    }
    
    // Add capture
    segments.push({
      type: 'capture',
      text: capture.text,
      label: capture.label,
    });
    
    currentPos = captureEnd;
  }
  
  // Add remaining text
  if (currentPos < unfilteredTokens.length) {
    const rawText = unfilteredTokens.slice(currentPos).map(t => t.text).join('');
    if (rawText) {
      segments.push({ type: 'raw', text: rawText });
    }
  }
  
  return segments;
}

/**
 * Split raw text by sentence-ending punctuation.
 * Returns array of sentence strings (each includes its ending punctuation if present).
 */
function splitTextBySentences(text: string): string[] {
  if (!text) return [];
  
  const sentences: string[] = [];
  let currentSentence = '';
  
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    currentSentence += char;
    
    // Check if this is a sentence ender
    if (char === '。' || char === '.' || char === '！' || char === '!' || 
        char === '？' || char === '?' || char === '\n') {
      // Include any trailing whitespace/quotes with this sentence
      while (i + 1 < text.length && /[\s」』）】］"'\)]/.test(text[i + 1])) {
        i++;
        currentSentence += text[i];
      }
      sentences.push(currentSentence);
      currentSentence = '';
    }
  }
  
  // Add any remaining text as the final sentence
  if (currentSentence.trim()) {
    sentences.push(currentSentence);
  }
  
  return sentences;
}


function selectBestOutcome(outcomes: MatchOutcome[], startIndex: number): MatchOutcome | null {
  let best: MatchOutcome | null = null;
  for (const outcome of outcomes) {
    if (outcome.index <= startIndex) continue;
    if (!best) {
      best = outcome;
      continue;
    }

    const spanDiff = outcome.index - best.index;
    if (spanDiff !== 0) {
      const preferLonger = (outcome.preference ?? 0) >= (best.preference ?? 0);
      if (preferLonger ? spanDiff > 0 : spanDiff < 0) {
        best = outcome;
      }
      continue;
    }

    if (outcome.preference !== best.preference) {
      best = (outcome.preference ?? 0) > (best.preference ?? 0) ? outcome : best;
      continue;
    }

    if (outcome.captures.length > best.captures.length) {
      best = outcome;
    }
  }
  return best;
}

/**
 * Match a sentence against grammar patterns, trying multiple segmentations.
 * 
 * Strategy for selecting the "best" segmentation:
 * 1. Split input text by sentence boundaries (。, !, ?, etc.) first
 * 2. For each sentence, generate up to 5 different segmentations in parallel
 * 3. For each segmentation, attempt to match against all provided grammar patterns
 * 4. Within each sentence, select the segmentation that produces the most grammar matches
 * 5. If multiple segmentations produce the same number of matches, prefer:
 *    a. The one with matches of highest total priority (sum of grammar priorities)
 *    b. If priorities are equal, the one with highest ichiran segmentation score
 * 6. Combine matches from all sentences
 * 
 * This approach ensures that:
 * - Each sentence gets its own full set of tokenization alternatives (not limited by other sentences)
 * - Valid grammar patterns are detected even when a word can be segmented in multiple ways
 * - Tokenization is performed in parallel for better performance
 * 
 * @param text - Input Japanese text to match
 * @param defs - Grammar pattern definitions to match against
 * @param options - Matching options (e.g., maxMatches)
 * @returns Grammar matches from the best segmentation of each sentence
 */
export async function matchSentence(text: string, defs: GrammarDefinition[], options: MatchOptions = {}): Promise<MatchHit[]> {
  if (GRAMMAR_PROFILE) {
    currentProfile = {
      romanizeStarMs: 0,
      transformMs: 0,
      parseTokenAlternativesMs: 0,
      augmentTokensMs: 0,
      compileGrammarsMs: 0,
      alternativesTried: 0,
      grammarCount: 0,
      matcherCalls: 0,
      matcherMs: 0,
      selectBestOutcomeMs: 0,
      matchTotalMs: 0,
      buildSegmentsMs: 0,
      buildCharPositionMapMs: 0,
      sentenceBoundaryMs: 0,
      filteredTokenCounts: [],
      unfilteredTokenCounts: [],
    };
  }

  const altStart = GRAMMAR_PROFILE ? performance.now() : 0;
  
  // Step 1: Split text into sentences by stop marks
  const sentences = splitTextBySentences(text);
  
  // Step 2: Generate tokenization alternatives for each sentence in parallel
  const sentenceAlternatives = await Promise.all(
    sentences.map(sentence => segmentSentenceWithAlternatives(sentence, 5))
  );
  
  if (currentProfile) {
    currentProfile.alternativesTried = sentenceAlternatives.reduce((sum, alts) => sum + alts.length, 0);
  }
  
  const grammars = compileGrammars(defs);
  
  // Step 3: Match each sentence independently and collect results
  interface ScoredResult {
    hits: MatchHit[];
    matchCount: number;
    prioritySum: number;
    segmentationScore: number;
  }
  
  const allHits: MatchHit[] = [];
  
  for (const alternatives of sentenceAlternatives) {
    if (alternatives.length === 0) continue;
    
    const results: ScoredResult[] = [];
    
    // Try matching each segmentation alternative for this sentence
    for (const alternative of alternatives) {
      const tokens = alternative.tokens;
      if (currentProfile) {
        currentProfile.filteredTokenCounts.push(tokens.length);
        currentProfile.unfilteredTokenCounts.push(alternative.tokens.length);
      }
      
      // Pass unfiltered tokens for proper sentence context
      const hits = await matchGrammars(tokens, grammars, options, alternative.tokens);
      const prioritySum = hits.reduce((sum, hit) => {
        const priority = grammars.find(g => g.def.id === hit.grammarId)?.def.priority ?? 0;
        return sum + priority;
      }, 0);
      
      results.push({
        hits,
        matchCount: hits.length,
        prioritySum,
        segmentationScore: alternative.score,
      });
    }
    
    // Find the best result for this sentence
    // Prefer more matches, then higher priority sum, then higher segmentation score
    let best = results[0];
    for (const result of results) {
      if (result.matchCount > best.matchCount) {
        best = result;
      } else if (result.matchCount === best.matchCount) {
        if (result.prioritySum > best.prioritySum) {
          best = result;
        } else if (result.prioritySum === best.prioritySum && result.segmentationScore > best.segmentationScore) {
          best = result;
        }
      }
    }
    
    // Add this sentence's matches to the overall results
    if (best?.hits) {
      allHits.push(...best.hits);
    }
  }
  
  if (GRAMMAR_PROFILE && currentProfile) {
    const ms = performance.now() - altStart;
    // eslint-disable-next-line no-console
    console.log('[GRAMMAR_PROFILE] Summary:');
    // eslint-disable-next-line no-console
    console.log({
      totalMs: ms.toFixed(2),
      sentences: sentences.length,
      alternatives: currentProfile.alternativesTried,
      grammars: currentProfile.grammarCount,
      matcherCalls: currentProfile.matcherCalls,
      matcherMs: currentProfile.matcherMs.toFixed(2),
      selectBestOutcomeMs: currentProfile.selectBestOutcomeMs.toFixed(2),
      buildSegmentsMs: currentProfile.buildSegmentsMs.toFixed(2),
      buildCharPositionMapMs: currentProfile.buildCharPositionMapMs.toFixed(2),
      compileGrammarsMs: currentProfile.compileGrammarsMs.toFixed(2),
      filteredTokenCounts: currentProfile.filteredTokenCounts,
      unfilteredTokenCounts: currentProfile.unfilteredTokenCounts,
      compiledCacheHits,
      compiledCacheMisses,
    });
  }
  
  return allHits;
}

/**
 * Grammar detail information for a matched pattern.
 */
export interface GrammarDetail {
  /** Human-readable label of the pattern */
  label?: string;
  /** Formation/structure of the grammar pattern */
  formation?: string;
  /** Brief description of the pattern */
  description?: string;
  /** Detailed explanation of usage and nuances */
  explanation?: string;
  /** Example sentences demonstrating the pattern */
  examples?: Array<{ jp: string; en?: string }>;
}

/**
 * Combined analysis result containing both grammar matches and segmentation data.
 */
export interface AnalysisResult {
  /** Grammar pattern matches found in the text */
  grammarMatches: MatchHit[];
  /** Best segmentation result (transformed for presentation) */
  segments: TransformedRomanizeStarResult;
  /** Tokens from the best segmentation */
  tokens: Token[];
  /** Detailed information about each matched grammar pattern, keyed by grammarId */
  grammarDetails: Record<string, GrammarDetail>;
}

/**
 * Analyze text for both grammar patterns and segmentation.
 * Splits text by sentences and processes each independently, then combines results.
 * 
 * @param text - Input Japanese text to analyze
 * @param defs - Grammar pattern definitions to match against
 * @param options - Matching options (e.g., maxMatches)
 * @param limit - Number of segmentation alternatives to generate per sentence (default: 5)
 * @param normalizePunctuation - Whether to normalize Japanese punctuation to English (default: false)
 * @returns Combined result with grammar matches and segmentation data from all sentences
 */
export async function analyzeSentence(
  text: string, 
  defs: GrammarDefinition[], 
  options: MatchOptions = {},
  limit: number = 5,
  normalizePunctuation: boolean = false
): Promise<AnalysisResult> {
  if (GRAMMAR_PROFILE) {
    currentProfile = {
      romanizeStarMs: 0,
      transformMs: 0,
      parseTokenAlternativesMs: 0,
      augmentTokensMs: 0,
      compileGrammarsMs: 0,
      alternativesTried: 0,
      grammarCount: 0,
      matcherCalls: 0,
      matcherMs: 0,
      selectBestOutcomeMs: 0,
      matchTotalMs: 0,
      buildSegmentsMs: 0,
      buildCharPositionMapMs: 0,
      sentenceBoundaryMs: 0,
      filteredTokenCounts: [],
      unfilteredTokenCounts: [],
    };
  }

  const analyzeStart = GRAMMAR_PROFILE ? performance.now() : 0;

  // Step 1: Split text into sentences
  const sentences = splitTextBySentences(text);
  
  // Step 2: Process each sentence independently in parallel
  const sentenceResults = await Promise.all(
    sentences.map(async (sentence) => {
      // Get segmentation alternatives for this sentence
      const romanizeStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const rawResult: RomanizeStarResult = await romanizeStar(sentence, { limit, normalizePunctuation });
      if (currentProfile) currentProfile.romanizeStarMs += (performance.now() - romanizeStart);
      
      const transformStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const transformedResult: TransformedRomanizeStarResult = await transformRomanizeStarResult(rawResult);
      if (currentProfile) currentProfile.transformMs += (performance.now() - transformStart);
      
      const parseStart = GRAMMAR_PROFILE ? performance.now() : 0;
      const tokenAlternativesWithScores = parseTokenAlternatives(rawResult, transformedResult);
      if (currentProfile) currentProfile.parseTokenAlternativesMs += (performance.now() - parseStart);
      
      // Convert to Token format and augment
      const alternatives: SegmentationAlternative[] = [];
      for (const { tokens: romanizeStarResultTokens, score } of tokenAlternativesWithScores) {
        const tokens = romanizeStarResultTokens.map((romanizeStarResultToken) => {
          if (!romanizeStarResultToken.infoJson) {
            return {
              text: romanizeStarResultToken.word,
            } as Token;
          } else {
            return {
              text: romanizeStarResultToken.word,
              wordInfo: romanizeStarResultToken.info,
              grammarInfo: parseGrammarInfoFromToken(romanizeStarResultToken.infoJson),
            } as Token;
          }
        });
        
        const augStart = GRAMMAR_PROFILE ? performance.now() : 0;
        await augmentTokens(tokens);
        if (currentProfile) currentProfile.augmentTokensMs += (performance.now() - augStart);
        
        alternatives.push({
          tokens,
          score,
        });
      }
      
      return {
        alternatives,
        transformedResult,
      };
    })
  );
  
  // Step 3: Match grammars for each sentence using its alternatives
  const grammars = compileGrammars(defs);
  
  interface ScoredResult {
    hits: MatchHit[];
    matchCount: number;
    prioritySum: number;
    segmentationScore: number;
    alternativeIndex: number;
  }
  
  const sentenceProcessingResults = await Promise.all(
    sentenceResults.map(async ({ alternatives, transformedResult }) => {
      if (alternatives.length === 0) {
        return { hits: [], tokens: [], segments: [], alternativesTried: 0, profileData: { filteredTokenCounts: [], unfilteredTokenCounts: [] } };
      }
      
      const profileData = { filteredTokenCounts: [] as number[], unfilteredTokenCounts: [] as number[] };
      
      // Try matching each segmentation alternative for this sentence
      const results: ScoredResult[] = await Promise.all(
        alternatives.map(async (alternative, altIdx) => {
          const tokens = alternative.tokens;
          if (currentProfile) {
            profileData.filteredTokenCounts.push(tokens.length);
            profileData.unfilteredTokenCounts.push(alternative.tokens.length);
          }
          
          const hits = await matchGrammars(tokens, grammars, options, alternative.tokens);
          const prioritySum = hits.reduce((sum, hit) => {
            const priority = grammars.find(g => g.def.id === hit.grammarId)?.def.priority ?? 0;
            return sum + priority;
          }, 0);
          
          return {
            hits,
            matchCount: hits.length,
            prioritySum,
            segmentationScore: alternative.score,
            alternativeIndex: altIdx,
          };
        })
      );
      
      // Find the best result for this sentence
      let best = results[0];
      for (const result of results) {
        if (result.matchCount > best.matchCount) {
          best = result;
        } else if (result.matchCount === best.matchCount) {
          if (result.prioritySum > best.prioritySum) {
            best = result;
          } else if (result.prioritySum === best.prioritySum && result.segmentationScore > best.segmentationScore) {
            best = result;
          }
        }
      }
      
      const bestAlternative = alternatives[best.alternativeIndex];
      const bestSegmentation: TransformedRomanizeStarResult = transformedResult.map((segment) => {
        if (typeof segment === 'string') {
          return segment;
        } else if (Array.isArray(segment) && segment.length > 0) {
          return [segment[0]];
        }
        return segment;
      });
      
      return {
        hits: best?.hits || [],
        tokens: bestAlternative?.tokens || [],
        segments: bestSegmentation,
        alternativesTried: alternatives.length,
        profileData,
      };
    })
  );
  
  // Aggregate results
  const allHits: MatchHit[] = [];
  const allSegments: TransformedRomanizeStarResult = [];
  const allTokens: Token[] = [];
  
  for (const result of sentenceProcessingResults) {
    allHits.push(...result.hits);
    allTokens.push(...result.tokens);
    allSegments.push(...result.segments);
    
    if (currentProfile) {
      currentProfile.alternativesTried += result.alternativesTried;
      currentProfile.filteredTokenCounts.push(...result.profileData.filteredTokenCounts);
      currentProfile.unfilteredTokenCounts.push(...result.profileData.unfilteredTokenCounts);
    }
  }
  
  // Build grammar details map for all matched patterns
  const grammarDetails: Record<string, GrammarDetail> = {};
  const matchedGrammarIds = new Set(allHits.map(hit => hit.grammarId));
  
  for (const grammarId of matchedGrammarIds) {
    const grammarDef = defs.find(def => def.id === grammarId);
    if (grammarDef) {
      grammarDetails[grammarId] = {
        label: grammarDef.label,
        formation: grammarDef.formation,
        description: grammarDef.description,
        explanation: grammarDef.explanation,
        examples: grammarDef.examples,
      };
    }
  }
  
  if (GRAMMAR_PROFILE && currentProfile) {
    const totalMs = performance.now() - analyzeStart;
    // eslint-disable-next-line no-console
    console.log('[GRAMMAR_PROFILE] analyzeSentence summary:', {
      totalMs: totalMs.toFixed(2),
      romanizeStarMs: currentProfile.romanizeStarMs.toFixed(2),
      transformMs: currentProfile.transformMs.toFixed(2),
      parseTokenAlternativesMs: currentProfile.parseTokenAlternativesMs.toFixed(2),
      augmentTokensMs: currentProfile.augmentTokensMs.toFixed(2),
      compileGrammarsMs: currentProfile.compileGrammarsMs.toFixed(2),
      matcherCalls: currentProfile.matcherCalls,
      matcherMs: currentProfile.matcherMs.toFixed(2),
      selectBestOutcomeMs: currentProfile.selectBestOutcomeMs.toFixed(2),
      buildSegmentsMs: currentProfile.buildSegmentsMs.toFixed(2),
      buildCharPositionMapMs: currentProfile.buildCharPositionMapMs.toFixed(2),
      sentences: sentences.length,
      alternatives: currentProfile.alternativesTried,
      filteredTokenCounts: currentProfile.filteredTokenCounts,
      unfilteredTokenCounts: currentProfile.unfilteredTokenCounts,
      compiledCacheHits,
      compiledCacheMisses,
    });
  }

  return {
    grammarMatches: allHits,
    segments: allSegments,
    tokens: allTokens,
    grammarDetails,
  };
}
