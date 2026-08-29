import { describe, expect, test } from 'bun:test';
import {
  encodeMorphologyArtifact,
  type CompiledMorphologyArtifact
} from '../../data/src/browser-pack/morphology-format.js';
import { MorphologyFormatError, openMorphology } from '../src/morphology.js';

function fixture(): CompiledMorphologyArtifact {
  return {
    positions: ['adj-i', 'cop', 'v1'],
    rules: [
      {
        pos: 'adj-i', type: 51, negative: null, formal: null, ordinal: 1,
        stem: 1, okuri: '', euphr: '', euphk: ''
      },
      {
        pos: 'cop', type: 1, negative: true, formal: false, ordinal: 1,
        stem: 0, okuri: '', euphr: '', euphk: ''
      },
      {
        pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1,
        stem: 1, okuri: 'た', euphr: '', euphk: ''
      }
    ],
    templates: [
      { suffix: '', removed: 'い', firstRule: 0, secondRule: null },
      { suffix: 'た', removed: 'る', firstRule: 2, secondRule: null }
    ],
    rootKeys: [
      {
        route: 'kana', pos: 'adj-i', sourceText: 'ない',
        records: [{ rootGroup: 1, sourceForm: 'ない', sourceReading: 'ない', ord: 0, common: 0 }]
      },
      {
        route: 'kanji', pos: 'v1', sourceText: '食べる',
        records: [{ rootGroup: 0, sourceForm: '食べる', sourceReading: 'たべる', ord: 2, common: 7 }]
      }
    ],
    rootGroups: [
      { seq: 100, forms: ['食べる', 'たべる'] },
      { seq: 200, forms: ['ない'] }
    ],
    patches: [
      {
        route: 'kana', surface: 'じゃない', rootSeq: 300,
        sourceText: 'だ', sourceForm: 'だ', sourceReading: 'だ',
        form: 'じゃない', reading: 'じゃない', firstRule: 1,
        secondRule: null, intermediate: null, ord: 0, common: 0
      }
    ],
    tombstones: [
      { route: 'kana', surface: 'な', rootSeq: 200, firstRule: 0, secondRule: null }
    ]
  };
}

describe('portable reverse morphology', () => {
  test('decodes rule candidates with exact source facts and generated reading', () => {
    const bytes = encodeMorphologyArtifact(fixture());
    const reader = openMorphology(bytes);
    const candidates = reader.lookup('食べた', 'kanji');

    expect(reader.stats.templates).toBe(2);
    expect(candidates).toEqual([{
      route: 'kanji',
      surface: '食べた',
      rootSeq: 100,
      sourceText: '食べる',
      sourceForm: '食べる',
      sourceReading: 'たべる',
      form: '食べた',
      reading: 'たべた',
      intermediate: null,
      ruleIds: [2],
      path: [{ pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1 }],
      ord: 2,
      common: 7,
      compatibility: 'rule'
    }]);
  });

  test('preserves intentional manual forms and applies negative tombstones', () => {
    const reader = openMorphology(encodeMorphologyArtifact(fixture()));
    expect(reader.lookup('な', 'kana')).toEqual([]);
    expect(reader.lookup('じゃない', 'kana')).toEqual([{
      route: 'kana',
      surface: 'じゃない',
      rootSeq: 300,
      sourceText: 'だ',
      sourceForm: 'だ',
      sourceReading: 'だ',
      form: 'じゃない',
      reading: 'じゃない',
      intermediate: null,
      ruleIds: [1],
      path: [{ pos: 'cop', type: 1, negative: true, formal: false, ordinal: 1 }],
      ord: 0,
      common: 0,
      compatibility: 'manual'
    }]);
  });

  test('suppresses a generated form found anywhere on the lexical root', () => {
    const source = fixture();
    source.rootGroups[0]!.forms = ['たべる', '食べた', '食べる'];
    const reader = openMorphology(encodeMorphologyArtifact(source));
    expect(reader.lookup('食べた', 'kanji')).toEqual([]);
  });

  test('orders semantic candidates by code unit rather than host locale', () => {
    const source = fixture();
    source.rootKeys[1]!.records = [
      { rootGroup: 0, sourceForm: 'z', sourceReading: 'z', ord: 2, common: 7 },
      { rootGroup: 0, sourceForm: 'ä', sourceReading: 'ä', ord: 2, common: 7 }
    ];
    const reader = openMorphology(encodeMorphologyArtifact(source));
    expect(reader.lookup('食べた', 'kanji').map(candidate => candidate.sourceForm)).toEqual(['z', 'ä']);
  });

  test('reads from a non-zero Uint8Array offset and rejects corrupt layout', () => {
    const encoded = encodeMorphologyArtifact(fixture());
    const wrapped = new Uint8Array(encoded.byteLength + 6);
    wrapped.set(encoded, 3);
    expect(openMorphology(wrapped.subarray(3, 3 + encoded.byteLength)).lookup('食べた', 'kanji')).toHaveLength(1);

    const corrupt = encoded.slice();
    new DataView(corrupt.buffer).setUint32(68, 148, true);
    expect(() => openMorphology(corrupt)).toThrow(MorphologyFormatError);
  });
});
