import { describe, expect, test } from 'bun:test';
import {
  canonicalConjugationRelationKey,
  compareSortedRelations,
  packedRelationKey,
  parseConjugationRelationKey,
  type ConjugationRelationKey,
  type ReviewedRelationDelta
} from '../src/source-compiler/conjugation-relation-proof.js';
import { packedMorphologySurfaces } from '../src/source-compiler/packed-morphology-relation.js';
import { parseCanonicalEntryJson } from '../src/source-compiler/canonical-entry-ndjson.js';
import type { MorphologyCandidate } from '@ichiran/core';

function key(overrides: Partial<ConjugationRelationKey> = {}): ConjugationRelationKey {
  return {
    rootSeq: 1519210,
    route: 'kanji',
    surface: '忘れた',
    sourceText: '忘れる',
    sourceForm: '忘れる',
    sourceReading: 'わすれる',
    first: { pos: 'v1', type: 2, negative: false, formal: false },
    second: null,
    intermediate: null,
    sourceOrdinal: 0,
    sourceCommon: null,
    ...overrides
  };
}

async function* lines(values: readonly string[]): AsyncGenerator<string> {
  for (const value of [...values].sort((left, right) =>
    Buffer.compare(Buffer.from(left), Buffer.from(right)))) yield value;
}

function tinySurfaceIndex(): Uint8Array {
  // Morphology terminals: ASCII "a" and UTF-8 "あ" (e3 81 82).
  const stateCount = 4;
  const edgeCount = 4;
  const statesOffset = 64;
  const edgesOffset = statesOffset + (stateCount + 1) * 8;
  const bytes = new Uint8Array(edgesOffset + edgeCount * 4);
  const view = new DataView(bytes.buffer);
  bytes.set(new TextEncoder().encode('ICHISURF'));
  view.setUint16(8, 1, true);
  view.setUint16(10, 64, true);
  view.setUint32(16, stateCount, true);
  view.setUint32(20, edgeCount, true);
  view.setUint32(24, 2, true);
  view.setUint32(28, 0, true);
  view.setUint32(32, 2, true);
  view.setUint32(36, 0, true);
  view.setUint32(40, 2, true);
  view.setUint32(44, 3, true);
  view.setUint32(48, statesOffset, true);
  view.setUint32(52, edgesOffset, true);
  view.setUint32(56, bytes.byteLength, true);
  view.setUint16(60, 8, true);
  view.setUint16(62, 4, true);

  const state = (index: number, firstEdge: number, flags: number): void => {
    view.setUint32(statesOffset + index * 8, firstEdge, true);
    view.setUint32(statesOffset + index * 8 + 4, flags, true);
  };
  state(0, 0, 0x8000_0000);
  state(1, 0, 0);
  state(2, 1, 0);
  state(3, 2, 0);
  state(4, 4, 0);
  const edge = (index: number, label: number, target: number): void => {
    const at = edgesOffset + index * 4;
    bytes[at] = label;
    bytes[at + 1] = target & 0xff;
    bytes[at + 2] = (target >>> 8) & 0xff;
    bytes[at + 3] = (target >>> 16) & 0xff;
  };
  edge(0, 0x82, 0);
  edge(1, 0x81, 1);
  edge(2, 0x61, 0);
  edge(3, 0xe3, 2);
  return bytes;
}

describe('independent conjugation relation proof', () => {
  test('validates canonical-entry NDJSON at the CLI boundary', () => {
    const order = { event: 1, ordinal: 0 };
    const value = parseCanonicalEntryJson({
      seq: 1,
      source: { sourceId: 'fixture', ordinal: 0 },
      kanji: [],
      kana: [{
        text: 'する', ordinal: 0, sourceOrder: order, common: null,
        priorityTags: [], conjugatable: true, noKanji: true, best: null
      }],
      senses: [{
        ordinal: 0,
        glosses: ['do'],
        properties: [{ tag: 'pos', text: 'vs-i', ordinal: 0, sourceOrder: order }]
      }],
      restrictions: [],
      primaryNoKanji: true
    });
    expect(value.seq).toBe(1);
    expect(value.senses[0]?.properties[0]?.text).toBe('vs-i');
    expect(() => parseCanonicalEntryJson({ ...value, primaryNoKanji: 'yes' })).toThrow();
  });

  test('round-trips complete semantic keys including explicit nulls', () => {
    const value = key();
    expect(parseConjugationRelationKey(canonicalConjugationRelationKey(value))).toEqual(value);
  });

  test('projects packed candidates without physical ids or rule ordinals', () => {
    const candidate: MorphologyCandidate = {
      route: 'kanji',
      surface: '熟さしなさい',
      rootSeq: 1337800,
      sourceText: '熟す',
      sourceForm: '熟す',
      sourceReading: 'じゅくす',
      form: '熟さしなさい',
      reading: 'じゅくさしなさい',
      intermediate: '熟さす',
      ruleIds: [10, 11],
      path: [
        { pos: 'v5s', type: 53, negative: false, formal: false, ordinal: 1 },
        { pos: 'v5s', type: 10, negative: false, formal: true, ordinal: 1 }
      ],
      ord: 0,
      common: null,
      compatibility: 'rule'
    };
    expect(packedRelationKey(candidate)).toEqual(key({
      rootSeq: 1337800,
      surface: '熟さしなさい',
      sourceText: '熟す',
      sourceForm: '熟す',
      sourceReading: 'じゅくす',
      first: { pos: 'v5s', type: 53, negative: false, formal: false },
      second: { pos: 'v5s', type: 10, negative: false, formal: true },
      intermediate: '熟さす'
    }));
  });

  test('walks the packed morphology language independently', () => {
    expect([...packedMorphologySurfaces(tinySurfaceIndex())]).toEqual(['a', 'あ']);
  });

  test('reports duplicates, omissions, packed-only keys and exact reviews', async () => {
    const shared = canonicalConjugationRelationKey(key());
    const inactive = canonicalConjugationRelationKey(key({
      surface: 'わすれた',
      route: 'kanji'
    }));
    const reviewedPacked = canonicalConjugationRelationKey(key({
      rootSeq: 2089020,
      route: 'kana',
      surface: 'じゃない',
      sourceText: 'だ',
      first: { pos: 'cop', type: 1, negative: true, formal: false }
    }));
    const reviewed: ReviewedRelationDelta[] = [{
      side: 'packed-only',
      key: reviewedPacked,
      category: 'chronological-da-errata',
      provenance: 'dict-errata.lisp add-deha-ja-readings',
      preservedBehavior: 'retain the colloquial じゃ alternative'
    }];
    const report = await compareSortedRelations(
      lines([shared, shared, inactive]),
      lines([shared, reviewedPacked]),
      reviewed
    );

    expect(report.forward).toEqual(expect.objectContaining({ rows: 3, unique: 2, duplicates: 1 }));
    expect(report.packed).toEqual(expect.objectContaining({ rows: 2, unique: 2, duplicates: 0 }));
    expect(report.common).toBe(1);
    expect(report.omissions).toBe(1);
    expect(report.packedOnly).toBe(1);
    expect(report.reviewedDeltas).toBe(1);
    expect(report.unreviewedDeltas).toBe(1);
    expect(report.categories.map(value => [value.side, value.category, value.count])).toEqual([
      ['omission', 'inactive-route', 1],
      ['packed-only', 'chronological-da-errata', 1]
    ]);
    expect(report.forward.sha256).toBe('eb2befb13356bc79df5627691e37c0955ce1b9e1a6e855c29b88d60667b8b2b7');
    expect(report.packed.sha256).toBe('37d33dd703b0e9e3b20569d6b309b875e0b094aff657e48f5a0d5b638317f588');
    expect(report.differenceSha256).toBe('f46141eed73771f0220ee57183699b93897c71918fd21c633f862f06ccbf3193');
    expect(report.passed).toBe(false);
  });
});
