import { describe, expect, test } from 'bun:test';

import {
  MAX_ANALYZER_ENTITIES,
  MAX_ANALYZER_TEXT_LENGTH,
  MAX_ANALYZER_WORD_LENGTH,
  validatePortableAnalyzeRequest
} from '../src/analyzer.js';

describe('portable analyzer request bounds', () => {
  test('accepts every option value used by the parity corpus', () => {
    expect(validatePortableAnalyzeRequest('未知', {
      limit: 10,
      entities: [{ start: 0, end: 2, boost: 100 }],
      normalizePunctuation: true
    }).options).toEqual({
      limit: 10,
      entities: [{ start: 0, end: 2, boost: 100 }],
      normalizePunctuation: true
    });
    const paragraph = '猫。'.repeat(MAX_ANALYZER_TEXT_LENGTH / 2);
    expect(validatePortableAnalyzeRequest(paragraph).input).toBe(paragraph);
  });

  test('bounds every input dimension that multiplies analyzer work', () => {
    expect(() => validatePortableAnalyzeRequest('猫', { limit: 11 })).toThrow('1 to 10');
    expect(() => validatePortableAnalyzeRequest('猫'.repeat(MAX_ANALYZER_TEXT_LENGTH + 1)))
      .toThrow(`at most ${MAX_ANALYZER_TEXT_LENGTH}`);
    expect(validatePortableAnalyzeRequest('猫'.repeat(MAX_ANALYZER_WORD_LENGTH + 1)).input)
      .toHaveLength(MAX_ANALYZER_WORD_LENGTH + 1);
    expect(() => validatePortableAnalyzeRequest('猫', {
      entities: Array.from(
        { length: MAX_ANALYZER_ENTITIES + 1 },
        () => ({ start: 0, end: 1 })
      )
    })).toThrow(`at most ${MAX_ANALYZER_ENTITIES}`);
  });

  test('rejects invalid spans and non-finite or extreme boosts', () => {
    expect(() => validatePortableAnalyzeRequest('猫', {
      entities: [{ start: 0, end: 2 }]
    })).toThrow('within the input');
    expect(() => validatePortableAnalyzeRequest('猫', {
      entities: [{ start: 0, end: 1, boost: Number.POSITIVE_INFINITY }]
    })).toThrow('boost must be finite');
    expect(() => validatePortableAnalyzeRequest('猫', {
      entities: [{ start: 0, end: 1, boost: 1_000_001 }]
    })).toThrow('boost must be finite');
  });
});
