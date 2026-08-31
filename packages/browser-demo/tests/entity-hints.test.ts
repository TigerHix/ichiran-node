import { describe, expect, test } from 'bun:test';
import { MAX_ANALYZER_ENTITIES } from '@ichiran/core';

import {
  MAX_ENTITY_SPEC_LENGTH,
  parseEntityHints
} from '../src/entity-hints.js';

describe('entity hint UI parser', () => {
  test('rejects oversized raw input before tokenizing it', () => {
    expect(() => parseEntityHints('x'.repeat(MAX_ENTITY_SPEC_LENGTH + 1), 10))
      .toThrow(`at most ${MAX_ENTITY_SPEC_LENGTH} text units`);
  });

  test('rejects more than the canonical hint count before mapping tokens', () => {
    const value = Array.from(
      { length: MAX_ANALYZER_ENTITIES + 1 },
      () => '0:1'
    ).join(' ');
    expect(() => parseEntityHints(value, 10))
      .toThrow(`at most ${MAX_ANALYZER_ENTITIES} hints`);
  });

  test('parses bounded spans and boosts', () => {
    expect(parseEntityHints('0:2:120, 3:5', 5)).toEqual([
      { start: 0, end: 2, boost: 120 },
      { start: 3, end: 5 }
    ]);
  });
});
