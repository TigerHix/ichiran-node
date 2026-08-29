import { describe, expect, test } from 'bun:test';
import type { AnalyzerSupportCounterVariant } from '../src/analyzer-support.js';
import { materializeAnalyzerCounter } from '../src/analyzer-counters.js';

function variant(
  overrides: Partial<AnalyzerSupportCounterVariant> = {}
): AnalyzerSupportCounterVariant {
  return {
    className: 'CounterText',
    text: '本',
    kana: 'ほん',
    suffix: null,
    source: null,
    ordinal: false,
    foreign: false,
    common: null,
    suffixDescriptions: [],
    digitOptions: [],
    digitSet: [],
    allowed: [],
    ...overrides
  };
}

describe('portable counter materialization', () => {
  test('applies standard gemination, handakuten, and special people readings', () => {
    expect(materializeAnalyzerCounter('1', variant())?.reading).toBe('いっぽん');
    expect(materializeAnalyzerCounter('3', variant())?.reading).toBe('さんぽん');
    expect(materializeAnalyzerCounter('6', variant())?.reading).toBe('ろっぽん');
    expect(materializeAnalyzerCounter('2', variant({
      className: 'CounterPeople', text: '人', kana: 'にん'
    }))?.reading).toBe('ふたり');
  });

  test('honors compiler-resolved digit options and validity constraints', () => {
    expect(materializeAnalyzerCounter('4', variant({
      text: '時', kana: 'じ', digitOptions: [[4, 'よ']]
    }))?.reading).toBe('よじ');
    expect(materializeAnalyzerCounter('10', variant({
      className: 'CounterTsu', text: 'つ', kana: 'つ'
    }))).toBeNull();
    expect(materializeAnalyzerCounter('3', variant({ allowed: [1, 2] }))).toBeNull();
  });
});
