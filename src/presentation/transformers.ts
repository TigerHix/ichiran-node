/**
 * Presentation layer transformers
 * Converts internal data structures to user-friendly JSON formats
 */

import type { WordInfoGlossJson } from '../dict/presentation.js';
import { wordInfoGlossJson } from '../dict/presentation.js';
import { WordInfo } from '../dict/wordInfo.js';

export type TransformedRomanizeStarResultTokenTuple = [string, WordInfoGlossJson, any]

export type TransformedRomanizeStarResultSegment = Array<[TransformedRomanizeStarResultTokenTuple[], number]>

export type TransformedRomanizeStarResult = (TransformedRomanizeStarResultSegment | string)[]

/**
 * Transform romanizeStar result to user-friendly JSON format
 * Converts WordInfo objects to WordInfoGlossJson with gloss and conj fields
 *
 * This function takes the raw output from romanizeStar and transforms it into
 * a format suitable for JSON serialization and API responses.
 *
 * @param result - Raw result from romanizeStar containing strings and word segments
 * @returns Transformed result with WordInfo objects converted to WordInfoGlossJson
 */
export async function transformRomanizeStarResult(
  result: Array<string | Array<[Array<[string, WordInfo, any]>, number]>>
): Promise<TransformedRomanizeStarResult> {
  const transformed = await Promise.all(
    result.map(async (segment) => {
      if (typeof segment === 'string') {
        // Non-word segment - keep as-is
        return segment;
      } else {
        // Word segment - transform each alternative
        return Promise.all(
          segment.map(async ([wordList, score]) => {
            const transformedWords = await Promise.all(
              wordList.map(async ([romanized, wordInfo, prop]) => {
                const glossJson = await wordInfoGlossJson(wordInfo);
                return [romanized, glossJson, prop] as TransformedRomanizeStarResultTokenTuple;
              })
            );
            return [transformedWords, score] as [TransformedRomanizeStarResultTokenTuple[], number];
          })
        );
      }
    })
  );
  return transformed;
}
