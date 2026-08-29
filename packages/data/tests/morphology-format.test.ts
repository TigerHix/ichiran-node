import { describe, expect, test } from 'bun:test';
import {
  encodeMorphologyArtifact,
  type CompiledMorphologyArtifact
} from '../src/browser-pack/morphology-format.js';

const EMPTY: CompiledMorphologyArtifact = {
  positions: [],
  rules: [],
  templates: [],
  rootKeys: [],
  rootGroups: [],
  patches: [],
  tombstones: []
};

describe('morphology binary encoder', () => {
  test('is deterministic for canonical input', () => {
    expect(encodeMorphologyArtifact(EMPTY)).toEqual(encodeMorphologyArtifact(EMPTY));
  });

  test('rejects non-canonical position order', () => {
    expect(() => encodeMorphologyArtifact({ ...EMPTY, positions: ['v1', 'adj-i'] }))
      .toThrow('positions must be strictly sorted');
  });
});
