// Parse Number tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { parseNumber, numberToKanji } from '../src/dict/numbers.js';

describe('Parse Number Tests', () => {
  test('100万 → 1000000', () => {
    expect(parseNumber("100万")).toBe(1000000);
  });

  test('100万500 → 1000500', () => {
    expect(parseNumber("100万500")).toBe(1000500);
  });

  test('round-trip: 0', () => {
    const n = 0;
    expect(parseNumber(numberToKanji(n))).toBe(n);
  });

  test('round-trip: 10001', () => {
    const n = 10001;
    expect(parseNumber(numberToKanji(n))).toBe(n);
  });

  test('round-trip: 20020001', () => {
    const n = 20020001;
    expect(parseNumber(numberToKanji(n))).toBe(n);
  });

  test('round-trip: 12423000430', () => {
    const n = 12423000430;
    expect(parseNumber(numberToKanji(n))).toBe(n);
  });
});