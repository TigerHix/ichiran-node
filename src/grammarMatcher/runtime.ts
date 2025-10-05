import type { GrammarDefinition, MatchHit, MatchOptions, Capture, MatchOutcome, Token } from './types.js';
import { compilePattern } from './compiler.js';
import { romanizeStar, RomanizeStarResult } from '../romanize.js';
import { augmentTokens } from './augment.js';
import { TransformedRomanizeStarResult, transformRomanizeStarResult } from '../presentation/transformers.js';
import { parseGrammarInfoFromToken, parseTokenAlternatives } from './parsing.js';

export interface CompiledGrammar {
  def: GrammarDefinition;
  matcher: ReturnType<typeof compilePattern>;
}

export interface SegmentationAlternative {
  tokens: Token[];
  score: number;
}

/**
 * Segment text using ichiran with multiple alternative segmentations.
 * 
 * @param text - Input Japanese text
 * @param limit - Number of segmentation alternatives to generate (default: 5)
 * @returns Array of segmentation alternatives, each with tokens and a score
 */
export async function segmentSentenceWithAlternatives(
  text: string,
  limit: number = 5
): Promise<SegmentationAlternative[]> {
  // Get multiple segmentations with gloss data (includes POS tags)
  const result: RomanizeStarResult = await romanizeStar(text, { limit });
  const transformedResult: TransformedRomanizeStarResult = await transformRomanizeStarResult(result);
  
  // Parse alternative tokenizations with their scores
  const tokenAlternativesWithScores = parseTokenAlternatives(result, transformedResult);
  
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
    await augmentTokens(tokens);
    
    alternatives.push({
      tokens,
      score,
    });
  }
  
  return alternatives;
}

export function compileGrammars(defs: GrammarDefinition[]): CompiledGrammar[] {
  return defs
    .map(def => ({ def, matcher: compilePattern(def.pattern) }))
    .sort((a, b) => (b.def.priority ?? 0) - (a.def.priority ?? 0));
}

/**
 * Check if a token is a stop mark (important punctuation to keep).
 * Stop marks include sentence boundaries and quotation marks.
 */
function isStopMark(token: Token): boolean {
  // Trim the token text to handle cases like ' " with spaces
  const text = token.text.trim();
  const stopMarks = ['。', '!', '?', '「', '」', '『', '』', '（', '）', '【', '】', '［', '］', '"', '\''];
  return stopMarks.includes(text);
}

/**
 * Check if a token is punctuation/whitespace (has no POS and no word info).
 */
function isPunctuation(token: Token): boolean {
  return !token.grammarInfo?.partOfSpeech && !token.wordInfo;
}

/**
 * Filter out non-stop-mark punctuation tokens from a token array.
 * Keeps: normal tokens, stop marks
 * Removes: commas, whitespace, and other non-stop-mark punctuation
 */
function filterNonStopMarkPunctuation(tokens: Token[]): Token[] {
  return tokens.filter(token => {
    // Keep all non-punctuation tokens
    if (!isPunctuation(token)) return true;
    // For punctuation, only keep stop marks
    return isStopMark(token);
  });
}

export async function matchGrammars(tokens: Token[], grammars: CompiledGrammar[], options: MatchOptions = {}): Promise<MatchHit[]> {
  const hits: MatchHit[] = [];
  const maxMatches = options.maxMatches ?? Infinity;

  for (const { def, matcher } of grammars) {
    for (let i = 0; i < tokens.length && hits.length < maxMatches; i++) {
      const outcomes = await matcher(tokens, i);
      const outcome = selectBestOutcome(outcomes, i);
      if (!outcome) continue;
      hits.push({
        grammarId: def.id,
        level: def.level,
        description: def.description,
        captures: outcome.captures.map((capture): Capture => ({
          ...capture,
          tokens: tokens.slice(capture.start, capture.end),
        })),
      });
      i = outcome.index - 1;
    }
  }

  return hits;
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
 * 1. Try up to 5 different segmentations of the input text
 * 2. For each segmentation, attempt to match against all provided grammar patterns
 * 3. Select the segmentation that produces the most grammar matches
 * 4. If multiple segmentations produce the same number of matches, prefer:
 *    a. The one with matches of highest total priority (sum of grammar priorities)
 *    b. If priorities are equal, the one with highest ichiran segmentation score
 * 5. If no segmentation produces any matches, return results from the highest-scored segmentation
 * 
 * This approach ensures that valid grammar patterns are detected even when a word can be
 * segmented in multiple ways (e.g., "よりよい" as one word vs "より" + "よい").
 * 
 * @param text - Input Japanese text to match
 * @param defs - Grammar pattern definitions to match against
 * @param options - Matching options (e.g., maxMatches)
 * @returns Grammar matches from the best segmentation
 */
export async function matchSentence(text: string, defs: GrammarDefinition[], options: MatchOptions = {}): Promise<MatchHit[]> {
  // Get multiple segmentation alternatives
  const alternatives = await segmentSentenceWithAlternatives(text, 5);
  const grammars = compileGrammars(defs);
  
  // Try matching each segmentation alternative
  interface ScoredResult {
    hits: MatchHit[];
    matchCount: number;
    prioritySum: number;
    segmentationScore: number;
  }
  
  const results: ScoredResult[] = [];
  
  for (const alternative of alternatives) {
    // Filter out non-stop-mark punctuation (commas, etc.) to simplify pattern matching
    const filteredTokens = filterNonStopMarkPunctuation(alternative.tokens);
    
    const hits = await matchGrammars(filteredTokens, grammars, options);
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
  
  // Find the best result
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
  
  return best?.hits ?? [];
}
