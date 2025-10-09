import type { Token } from './types.js';
import { romanizeStar, type RomanizeStarResult } from '@ichiran/core/src/romanize.js';
import { transformRomanizeStarResult, type TransformedRomanizeStarResult } from '@ichiran/core/src/presentation/transformers.js';
import { parseGrammarInfoFromToken, parseTokenAlternatives } from './parsing.js';
import { time, timeAsync } from './profile.js';

export interface SegmentationAlternative {
  tokens: Token[];
  score: number;
}

function convertToTokens(romanizeStarResultTokens: any[]): Token[] {
  return romanizeStarResultTokens.map((romanizeStarResultToken) => {
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
}

export async function segmentText(
  text: string,
  limit: number = 5,
  normalizePunctuation: boolean = false
): Promise<{ alternatives: SegmentationAlternative[]; transformedResult: TransformedRomanizeStarResult }> {
  const result: RomanizeStarResult = await timeAsync('romanizeStar', () => romanizeStar(text, { limit, normalizePunctuation }));
  const transformedResult: TransformedRomanizeStarResult = await timeAsync('transformRomanizeStarResult', () => transformRomanizeStarResult(result));

  const tokenAlternativesWithScores = time('parseTokenAlternatives', () => parseTokenAlternatives(result, transformedResult));

  const alternatives: SegmentationAlternative[] = tokenAlternativesWithScores.map(({ tokens: romanizeStarResultTokens, score }) => ({
    tokens: convertToTokens(romanizeStarResultTokens),
    score,
  }));

  return { alternatives, transformedResult };
}

export function splitTextBySentences(text: string): string[] {
  if (!text) return [];

  const sentences: string[] = [];
  let currentSentence = '';

  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    currentSentence += char;

    if (char === '。' || char === '.' || char === '！' || char === '!' || 
        char === '？' || char === '?' || char === '\n') {
      while (i + 1 < text.length && /[\s」』）】］"'\)]/.test(text[i + 1])) {
        i++;
        currentSentence += text[i];
      }
      sentences.push(currentSentence);
      currentSentence = '';
    }
  }

  if (currentSentence.trim()) {
    sentences.push(currentSentence);
  }

  return sentences;
}


