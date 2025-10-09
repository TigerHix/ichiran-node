import type { GrammarDefinition, MatchHit, MatchOptions, Token, PatternNode } from './types.js';
import type { TransformedRomanizeStarResult } from '@ichiran/core/src/presentation/transformers.js';
import { resolvePredicate, type AsyncPredicateFn } from './predicates.js';
import { performance } from 'node:perf_hooks';
import { segmentText, splitTextBySentences, type SegmentationAlternative } from './segmentation.js';
import { GRAMMAR_PROFILE } from './profile.js';
import { matchGrammars } from './matcher.js';
import { getCompiledMatcher, resetCacheStats, getCacheStats } from './cache.js';
import { MACRO_MIN_TOKENS } from './macros.js';

export interface CompiledGrammar {
  def: GrammarDefinition;
  matcher: (tokens: Token[], index: number) => Promise<any[]>;
  minTokens?: number;            // lower bound on tokens required
  // Pre-resolved start-gate predicates for performance
  firstTokenPreds?: AsyncPredicateFn[];
  anyTokenPreds?: AsyncPredicateFn[];
  nearPreds?: AsyncPredicateFn[];
  nearWindow?: { left: number; right: number };
}

interface ProfileTotals {
  romanizeStarMs: number;
  transformMs: number;
  parseTokenAlternativesMs: number;
  compileGrammarsMs: number;
  alternativesTried: number;
  grammarCount: number;
  matcherCalls: number;
  matcherMs: number;
  selectBestOutcomeMs: number;
  matchTotalMs: number;
  buildSegmentsMs: number;
  filteredTokenCounts: number[];
  unfilteredTokenCounts: number[];
}

let currentProfile: ProfileTotals | null = null;

// segmentation moved to segmentation.ts


export function compileGrammars(defs: GrammarDefinition[]): CompiledGrammar[] {
  const start = GRAMMAR_PROFILE ? performance.now() : 0;
  // Reset counters per compile run (cache entries persist)
  resetCacheStats();
  const compiled = defs
    .map(def => {
      const matcher = getCompiledMatcher(def);
      // Derive a conservative minTokens (best-effort): 1 for token, sum of mins for sequence, min of mins for alt, 0 for optional, min* for repeat
      const minTokens = computeMinTokens(def.pattern);
      // Pre-resolve start gate predicates for this grammar
      const gate = def.startGate;
      const firstTokenPreds = gate?.firstToken && gate.firstToken.length > 0
        ? gate.firstToken.map(resolvePredicate)
        : undefined;
      const anyTokenPreds = gate?.anyToken && gate.anyToken.length > 0
        ? gate.anyToken.map(resolvePredicate)
        : undefined;
      const nearPreds = gate?.near && Array.isArray(gate.near.predicates) && gate.near.predicates.length > 0
        ? gate.near.predicates.map(resolvePredicate)
        : undefined;
      const nearWindow = gate?.near?.window;
      return { def, matcher, minTokens, firstTokenPreds, anyTokenPreds, nearPreds, nearWindow } as CompiledGrammar;
    })
    .sort((a, b) => (b.def.priority ?? 0) - (a.def.priority ?? 0));

  if (GRAMMAR_PROFILE) {
    const ms = performance.now() - start;
    const { hits, misses } = getCacheStats();
    if (currentProfile) {
      currentProfile.compileGrammarsMs += ms;
      currentProfile.grammarCount = defs.length;
    }
    console.log(`[GRAMMAR_PROFILE] compileGrammars: ${ms.toFixed(2)}ms (hits=${hits}, misses=${misses})`);
  }

  return compiled;
}

// Best-effort minimum token count of a pattern
function computeMinTokens(node: PatternNode | undefined): number {
  if (!node) return 0;
  if ('token' in node) return 1;
  if ('sequence' in node) return node.sequence.reduce((sum, n) => sum + computeMinTokens(n), 0);
  if ('alt' in node) return node.alt.reduce((min, n) => Math.min(min, computeMinTokens(n)), Number.POSITIVE_INFINITY) || 0;
  if ('optional' in node) return 0;
  if ('capture' in node) return computeMinTokens(node.pattern);
  if ('peek' in node) return 0; // lookahead doesn't consume
  if ('not' in node) return 0;
  if ('anchor' in node) return 0;
  if ('macro' in node) return MACRO_MIN_TOKENS[node.macro] ?? 1;
  if ('repeat' in node) {
    const min = node.repeat.min ?? 0;
    return min * computeMinTokens(node.repeat.pattern);
  }
  return 0;
}


/**
 * Match text against grammar patterns, trying multiple segmentations.
 * 
 * Strategy for selecting the "best" segmentation:
 * 1. Split input text by sentence boundaries (。, !, ?, etc.) first
 * 2. For each sentence, generate multiple segmentations in parallel
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
 * @param options - Matching options (e.g., maxMatches, limit, normalizePunctuation)
 * @returns Grammar matches from the best segmentation of each sentence
 */
export async function matchText(
  text: string,
  defs: GrammarDefinition[],
  options: MatchOptions & { limit?: number; normalizePunctuation?: boolean } = {}
): Promise<MatchHit[]> {
  const limit = options.limit ?? 5;
  const normalizePunctuation = options.normalizePunctuation ?? false;
  if (GRAMMAR_PROFILE) {
    currentProfile = {
      romanizeStarMs: 0,
      transformMs: 0,
      parseTokenAlternativesMs: 0,
      compileGrammarsMs: 0,
      alternativesTried: 0,
      grammarCount: 0,
      matcherCalls: 0,
      matcherMs: 0,
      selectBestOutcomeMs: 0,
      matchTotalMs: 0,
      buildSegmentsMs: 0,
      filteredTokenCounts: [],
      unfilteredTokenCounts: [],
    };
  }

  const altStart = GRAMMAR_PROFILE ? performance.now() : 0;
  
  // Step 1: Split text into sentences by stop marks
  const sentences = splitTextBySentences(text);
  
  // Step 2: Generate tokenization alternatives for each sentence in parallel
  const sentenceAlternatives = await Promise.all(
    sentences.map(async (sentence: string) => {
      const { alternatives } = await segmentText(sentence, limit, normalizePunctuation);
      return alternatives;
    })
  );
  
  if (currentProfile) {
    currentProfile.alternativesTried = sentenceAlternatives.reduce((sum: number, alts: SegmentationAlternative[]) => sum + alts.length, 0);
  }
  
  const grammars = compileGrammars(defs);
  const priorityById = new Map<string, number>(grammars.map(g => [g.def.id, g.def.priority ?? 0]));
  
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
        const contentCount = tokens.filter((t: Token) => t.wordInfo !== undefined || t.grammarInfo !== undefined).length;
        currentProfile.filteredTokenCounts.push(contentCount);
        currentProfile.unfilteredTokenCounts.push(tokens.length);
      }
      
      // Pass unfiltered tokens for proper sentence context
      const hits = await matchGrammars(tokens, grammars, options, alternative.tokens, currentProfile || undefined);
      const prioritySum = hits.reduce((sum, hit) => sum + (priorityById.get(hit.grammarId) ?? 0), 0);
      
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
    const { hits, misses } = getCacheStats();
    console.log('[GRAMMAR_PROFILE] Summary:');
    console.log({
      totalMs: ms.toFixed(2),
      sentences: sentences.length,
      alternatives: currentProfile.alternativesTried,
      grammars: currentProfile.grammarCount,
      matcherCalls: currentProfile.matcherCalls,
      matcherMs: currentProfile.matcherMs.toFixed(2),
      selectBestOutcomeMs: currentProfile.selectBestOutcomeMs.toFixed(2),
      buildSegmentsMs: currentProfile.buildSegmentsMs.toFixed(2),
      compileGrammarsMs: currentProfile.compileGrammarsMs.toFixed(2),
      filteredTokenCounts: currentProfile.filteredTokenCounts,
      unfilteredTokenCounts: currentProfile.unfilteredTokenCounts,
      compiledCacheHits: hits,
      compiledCacheMisses: misses,
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
 * This is a higher-level API that returns both grammar matches and segmentation data.
 * For just grammar matches, use matchText() instead.
 * 
 * @param text - Input Japanese text to analyze
 * @param defs - Grammar pattern definitions to match against
 * @param options - Matching options (e.g., maxMatches, limit, normalizePunctuation)
 * @returns Combined result with grammar matches and segmentation data from all sentences
 */
export async function analyzeText(
  text: string,
  defs: GrammarDefinition[],
  options: MatchOptions & { limit?: number; normalizePunctuation?: boolean } = {}
): Promise<AnalysisResult> {
  const limit = options.limit ?? 5;
  const normalizePunctuation = options.normalizePunctuation ?? false;
  if (GRAMMAR_PROFILE) {
    currentProfile = {
      romanizeStarMs: 0,
      transformMs: 0,
      parseTokenAlternativesMs: 0,
      compileGrammarsMs: 0,
      alternativesTried: 0,
      grammarCount: 0,
      matcherCalls: 0,
      matcherMs: 0,
      selectBestOutcomeMs: 0,
      matchTotalMs: 0,
      buildSegmentsMs: 0,
      filteredTokenCounts: [],
      unfilteredTokenCounts: [],
    };
  }

  const analyzeStart = GRAMMAR_PROFILE ? performance.now() : 0;

  // Step 1: Split text into sentences
  const sentences = splitTextBySentences(text);
  
  // Step 2: Process each sentence independently in parallel using unified segmentation
  const sentenceResults = await Promise.all(
    sentences.map(async (sentence: string) => {
      const { alternatives, transformedResult } = await segmentText(sentence, limit, normalizePunctuation);
      return { alternatives, transformedResult } as { alternatives: SegmentationAlternative[]; transformedResult: TransformedRomanizeStarResult };
    })
  );
  
  // Step 3: Match grammars for each sentence using its alternatives
  const grammars = compileGrammars(defs);
  const priorityById = new Map<string, number>(grammars.map(g => [g.def.id, g.def.priority ?? 0]));
  
  interface ScoredResult {
    hits: MatchHit[];
    matchCount: number;
    prioritySum: number;
    segmentationScore: number;
    alternativeIndex: number;
  }
  
  const sentenceProcessingResults = await Promise.all(
    sentenceResults.map(async ({ alternatives, transformedResult }: { alternatives: SegmentationAlternative[]; transformedResult: TransformedRomanizeStarResult }) => {
      if (alternatives.length === 0) {
        return { hits: [], tokens: [], segments: [], alternativesTried: 0, profileData: { filteredTokenCounts: [], unfilteredTokenCounts: [] } };
      }
      
      const profileData = { filteredTokenCounts: [] as number[], unfilteredTokenCounts: [] as number[] };
      
      // Try matching each segmentation alternative for this sentence
      const results: ScoredResult[] = await Promise.all(
        alternatives.map(async (alternative: SegmentationAlternative, altIdx: number) => {
          const tokens = alternative.tokens;
          if (currentProfile) {
            const contentCount = tokens.filter((t: Token) => t.wordInfo !== undefined || t.grammarInfo !== undefined).length;
            profileData.filteredTokenCounts.push(contentCount);
            profileData.unfilteredTokenCounts.push(tokens.length);
          }
          
          const hits = await matchGrammars(tokens, grammars, options, alternative.tokens, currentProfile || undefined);
          const prioritySum = hits.reduce((sum, hit) => sum + (priorityById.get(hit.grammarId) ?? 0), 0);
          
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
      const bestSegmentation: TransformedRomanizeStarResult = transformedResult.map((segment: string | any[]) => {
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
    const { hits, misses } = getCacheStats();
    console.log('[GRAMMAR_PROFILE] analyzeText summary:', {
      totalMs: totalMs.toFixed(2),
      romanizeStarMs: currentProfile.romanizeStarMs.toFixed(2),
      transformMs: currentProfile.transformMs.toFixed(2),
      parseTokenAlternativesMs: currentProfile.parseTokenAlternativesMs.toFixed(2),
      compileGrammarsMs: currentProfile.compileGrammarsMs.toFixed(2),
      matcherCalls: currentProfile.matcherCalls,
      matcherMs: currentProfile.matcherMs.toFixed(2),
      selectBestOutcomeMs: currentProfile.selectBestOutcomeMs.toFixed(2),
      buildSegmentsMs: currentProfile.buildSegmentsMs.toFixed(2),
      sentences: sentences.length,
      alternatives: currentProfile.alternativesTried,
      filteredTokenCounts: currentProfile.filteredTokenCounts,
      unfilteredTokenCounts: currentProfile.unfilteredTokenCounts,
      compiledCacheHits: hits,
      compiledCacheMisses: misses,
    });
  }

  return {
    grammarMatches: allHits,
    segments: allSegments,
    tokens: allTokens,
    grammarDetails,
  };
}
