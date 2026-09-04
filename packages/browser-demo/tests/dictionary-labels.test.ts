import { describe, expect, test } from 'bun:test';
import {
  conjugationLabel,
  partOfSpeechCategory,
  partOfSpeechLabel
} from '../src/dictionary-labels.js';

describe('dictionary presentation labels', () => {
  test('uses Komi learner-facing labels for transitivity', () => {
    expect(partOfSpeechLabel('vi')).toBe('Intransitive Verb');
    expect(partOfSpeechLabel('vt')).toBe('Transitive Verb');
  });

  test('matches exact classes instead of flattening prefixed codes', () => {
    expect(partOfSpeechLabel('v5k')).toBe('Godan Verb (-ku)');
    expect(partOfSpeechLabel('v5k-s')).toBe('Godan Verb (-ku Special)');
    expect(partOfSpeechLabel('v5r-i')).toBe('Godan Verb (-ru Irregular)');
  });

  test('preserves unknown future tags and categorizes known tags', () => {
    expect(partOfSpeechLabel('future-jmdict-code')).toBe('future-jmdict-code');
    expect(partOfSpeechCategory('v2k-k')).toBe('verb');
    expect(partOfSpeechCategory('adj-na')).toBe('adjective');
  });

  test('uses the analyzer conjugation vocabulary', () => {
    expect(conjugationLabel(2)).toBe('Past (~ta)');
    expect(conjugationLabel(8)).toBe('Causative-Passive');
    expect(conjugationLabel(54)).toBe('Old/literary form');
  });
});
