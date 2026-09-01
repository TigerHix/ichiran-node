import { describe, expect, test } from 'bun:test';
import { replayConjugationReading } from '../src/source-compiler/conjugation-reading-replay.js';

describe('chronological addConjReading replay', () => {
  test('replaces only the non-shared root prefix', () => {
    expect(replayConjugationReading('でかい', 'デカい', 'でかえ')).toBe('デカえ');
    expect(replayConjugationReading('こける', 'コケる', 'こけさせた')).toBe('コケさせた');
  });

  test('preserves the upstream length-edge behavior', () => {
    expect(replayConjugationReading('abc', 'xabc', 'abc!')).toBe('xabc!');
    expect(replayConjugationReading('xabc', 'abc', 'xabc!')).toBe('abc!');
  });
});
