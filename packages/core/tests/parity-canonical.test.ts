import { describe, expect, test } from 'bun:test';

import {
  canonicalizeAnalyzerOutput,
  firstCanonicalDifference,
  legacyPathSkeleton,
  normalizeLegacyIdentities,
  projectCoreCleanAnalysis,
  projectPortableCleanAnalysis,
  semanticCandidateKey
} from '../tools/parity-canonical.js';
import type {
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisToken
} from '../src/analyzer.js';

describe('oracle parity canonicalization', () => {
  test('sorts object keys but preserves differently scored alternatives', () => {
    const value = {
      z: 1,
      alternative: [
        { seq: 3, score: 20 },
        { seq: 2, score: 10 },
        { seq: 1, score: 10 }
      ],
      a: 2
    };
    expect(canonicalizeAnalyzerOutput(value)).toEqual({
      a: 2,
      alternative: [
        { score: 20, seq: 3 },
        { score: 10, seq: 1 },
        { score: 10, seq: 2 }
      ],
      z: 1
    });
  });

  test('sorts only complete paths tied on score', () => {
    const value = [
      [[["same", { route: 'kana', text: 'b', reading: 'b', seq: 2 }, []]], 10],
      [[["same", { route: 'kana', text: 'a', reading: 'a', seq: 1 }, []]], 10],
      [[["same", { route: 'kana', text: 'c', reading: 'c', seq: 3 }, []]], 5]
    ];
    expect(canonicalizeAnalyzerOutput(value)).toEqual([
      [[["same", { reading: 'a', route: 'kana', seq: 1, text: 'a' }, []]], 10],
      [[["same", { reading: 'b', route: 'kana', seq: 2, text: 'b' }, []]], 10],
      [[["same", { reading: 'c', route: 'kana', seq: 3, text: 'c' }, []]], 5]
    ]);
  });

  test('semantic tie key excludes gloss presentation and cannot hide its order', () => {
    const first = {
      route: 'kanji', text: '猫', reading: '猫 【ねこ】', seq: 1, score: 10,
      gloss: [{ gloss: 'cat' }]
    };
    const second = { ...first, gloss: [{ gloss: 'feline' }] };
    expect(semanticCandidateKey(first)).toBe(semanticCandidateKey(second));
    expect(firstCanonicalDifference(
      { alternative: [first, second] },
      { alternative: [second, first] }
    )?.path).toBe('$.alternative[0].gloss[0].gloss');
  });

  test('semantic tie key recursively distinguishes equal-score compounds', () => {
    const compound = (seq: number, reading: string, gloss: string) => ({
      route: 'kana', text: 'いって', reading: 'いって', score: 10,
      components: [{
        route: 'kana', text: 'いって', reading, seq,
        conj: [{ reading }],
        gloss: [{ gloss }]
      }]
    });
    const entering = compound(1465580, '入る 【いる】', 'to enter');
    const going = compound(1578850, '行く 【いく】', 'to go');

    expect(firstCanonicalDifference(
      { alternative: [entering, going] },
      { alternative: [going, entering] }
    )).toBeNull();

    const changedEntering = compound(1465580, '入る 【いる】', 'different gloss');
    expect(firstCanonicalDifference(
      { alternative: [entering, going] },
      { alternative: [going, changedEntering] }
    )).toMatchObject({
      kind: 'value',
      expected: 'to enter',
      actual: 'different gloss'
    });
  });

  test('reports the first exact path', () => {
    expect(firstCanonicalDifference({ value: [1, 2] }, { value: [1, 3] })).toEqual({
      path: '$.value[1]',
      kind: 'value',
      expected: 2,
      actual: 3
    });
  });

  test('path skeleton removes dictionary presentation but retains segmentation and scores', () => {
    const token = [
      'neko',
      { reading: '猫 【ねこ】', text: '猫', kana: 'ねこ', score: 42, seq: 1, gloss: [] },
      []
    ];
    const output = [[[[token], 42]]];
    expect(legacyPathSkeleton(output)).toEqual([[[[
      ['neko', { reading: '猫 【ねこ】', text: '猫', kana: 'ねこ', score: 42 }, []]
    ], 42]]]);
  });

  test('normalizes unique roots and makes multiple-root identity explicit', async () => {
    const surfaces: Array<string | undefined> = [];
    const normalized = await normalizeLegacyIdentities(
      [{ seq: 10, text: 'one' }, { seq: 20, text: 'two' }, { seq: 30 }],
      {
        async roots(seq, surface) {
          surfaces.push(surface);
          if (seq === 10) return [1];
          if (seq === 20) return [2, 3];
          return [seq];
        }
      }
    );
    expect(normalized.value).toEqual([
      { seq: 1, text: 'one' },
      { seq: [2, 3], text: 'two' },
      { seq: 30 }
    ]);
    expect(normalized.rewritten).toBe(2);
    expect(normalized.multipleRoots).toEqual({ '20:two': [2, 3] });
    expect(surfaces.map(value => value ?? '<none>').sort()).toEqual([
      '<none>', 'one', 'two'
    ]);
  });

  test('uses detailed conjugation source lineage when a suffix changes the displayed surface', async () => {
    const normalized = await normalizeLegacyIdentities(
      {
        text: 'おもわざる',
        seq: 10542052,
        conj: [{
          prop: [{ pos: 'v5u', type: 'Non-past', neg: true }],
          reading: '思う 【おもう】'
        }]
      },
      {
        async roots(seq, surface, sources) {
          expect(seq).toBe(10542052);
          expect(surface).toBe('おもわざる');
          expect(sources).toEqual([{ form: '思う', reading: 'おもう' }]);
          return sources?.some(source =>
            source.form === '思う' && source.reading === 'おもう')
            ? [1589350]
            : [seq];
        }
      }
    );
    expect(normalized.value).toEqual({
      text: 'おもわざる',
      seq: 1589350,
      conj: [{
        prop: [{ pos: 'v5u', type: 'Non-past', neg: true }],
        reading: '思う 【おもう】'
      }]
    });
    expect(normalized.rewritten).toBe(1);
  });

  test('compares raw core semantics to portable chunks and global top-N paths', async () => {
    const core = await projectCoreCleanAnalysis({
      input: '猫,雨',
      normalized: '猫,雨',
      limit: 2,
      segments: [
        { type: 'word', text: '猫' },
        { type: 'misc', text: ',' },
        { type: 'word', text: '雨' }
      ],
      raw: [
        [
          [[['neko', {
            type: 'kanji', text: '猫', trueText: '猫', kana: 'ねこ', seq: 1,
            score: 10, start: 0, end: 1, skipped: 0
          }, []]], 10],
          [[['byō', {
            type: 'kanji', text: '猫', trueText: '猫', kana: 'びょう', seq: 2,
            score: 10, start: 0, end: 1, skipped: 0
          }, []]], 10]
        ],
        ',',
        [
          [[['ame', {
            type: 'kanji', text: '雨', trueText: '雨', kana: 'あめ', seq: 3,
            score: 7, start: 0, end: 1, skipped: 0
          }, []]], 7]
        ]
      ],
      async resolveWord(word) {
        const seq = typeof word.seq === 'number' ? word.seq : 0;
        const reading = Array.isArray(word.kana) ? word.kana[0]! : word.kana;
        return { root: { seq, form: word.text, reading }, inflection: [] };
      }
    });

    const token = (
      text: string,
      reading: string,
      seq: number,
      score: number,
      start: number,
      end: number
    ): PortableAnalysisToken => ({
      candidateId: seq, start, end, text, trueText: null, route: 'kanji', reading,
      romanized: '', pos: [], score, entryIndex: seq,
      root: { seq, form: text, reading }, inflection: [], components: [],
      alternatives: [{
        candidateId: seq, text, trueText: null, route: 'kanji', reading,
        romanized: '', pos: [], score, entryIndex: seq,
        root: { seq, form: text, reading }, inflection: [], components: [], counter: null
      }],
      skipped: 0, entity: false, counter: null
    });
    const gap: PortableAnalysisToken = {
      candidateId: null, start: 1, end: 2, text: ',', trueText: null,
      route: 'gap', reading: ',', romanized: ',', pos: [], score: 0,
      entryIndex: null, root: null, inflection: [], components: [], alternatives: [],
      skipped: 0, entity: false, counter: null
    };
    const cats = [
      { score: 10, tokens: [token('猫', 'ねこ', 1, 10, 0, 1)] },
      { score: 10, tokens: [token('猫', 'びょう', 2, 10, 0, 1)] }
    ] satisfies PortableAnalysisPath[];
    const rain = {
      score: 7,
      tokens: [token('雨', 'あめ', 3, 7, 2, 3)]
    } satisfies PortableAnalysisPath;
    const portable: PortableAnalysisResult = {
      input: '猫,雨', normalized: '猫,雨', computeMs: 99,
      chunks: [
        { type: 'word', start: 0, end: 1, text: '猫', paths: cats },
        { type: 'misc', start: 1, end: 2, text: ',' },
        { type: 'word', start: 2, end: 3, text: '雨', paths: [rain] }
      ],
      paths: cats.map(path => ({
        score: 17,
        tokens: [...path.tokens, gap, ...rain.tokens]
      }))
    };
    const projected = projectPortableCleanAnalysis(portable);
    expect(firstCanonicalDifference(core, projected)).toBeNull();
    const renumbered = structuredClone(portable);
    for (const path of renumbered.paths) {
      for (const value of path.tokens) {
        if (value.candidateId !== null) (value as { candidateId: number }).candidateId += 100;
        for (const alternative of value.alternatives) {
          (alternative as { candidateId: number }).candidateId += 100;
        }
      }
    }
    expect(projectPortableCleanAnalysis(renumbered)).toEqual(projected);
    expect(firstCanonicalDifference(portable, renumbered)?.path).toContain('candidateId');
    expect(core.paths.map(path => path.score)).toEqual([17, 17]);
    expect(core.paths[0]!.tokens.map(value => [value.start, value.end])).toEqual([
      [0, 1], [1, 2], [2, 3]
    ]);
  });

  test('normalizes the core counter suffix trueText at both candidate and token levels', async () => {
    const projected = await projectCoreCleanAnalysis({
      input: '1倍',
      normalized: '1倍',
      limit: 1,
      segments: [{ type: 'word', text: '1倍' }],
      raw: [[[[['ichibai', {
        type: 'kanji', text: '1倍', trueText: '倍', kana: 'いちばい', seq: 1473230,
        score: 136, start: 0, end: 2, counter: ['Value: 1', false], skipped: 0
      }, []]], 136]]],
      async resolveWord(word) {
        return {
          root: {
            seq: word.seq as number,
            form: '倍',
            reading: 'ばい'
          },
          inflection: []
        };
      }
    });
    const token = projected.paths[0]!.tokens[0]!;
    expect(token.trueText).toBeNull();
    expect(token.alternatives[0]!.trueText).toBeNull();
    expect(token.counter).toEqual(['Value: 1', false]);
  });

  test('distinguishes synthetic entities from boosted dictionary entries', async () => {
    const resolveWord = async (word: { readonly seq?: number | readonly number[] | null; readonly text: string; readonly kana: string | readonly string[] }) => ({
      root: typeof word.seq === 'number' && word.seq >= 0
        ? {
            seq: word.seq,
            form: word.text,
            reading: Array.isArray(word.kana) ? word.kana[0]! : word.kana
          }
        : null,
      inflection: []
    });
    const synthetic = await projectCoreCleanAnalysis({
      input: '田中太郎', normalized: '田中太郎', limit: 1,
      segments: [{ type: 'word', text: '田中太郎' }],
      raw: [[[[['田中太郎', {
        type: 'kana', text: '田中太郎', kana: '田中太郎', seq: -1,
        score: 50, start: 0, end: 4, isEntity: true
      }, []]], 50]]],
      resolveWord
    });
    const syntheticToken = synthetic.paths[0]!.tokens[0]!;
    expect(syntheticToken.route).toBe('kanji');
    expect(syntheticToken.root).toBeNull();
    expect(syntheticToken.alternatives).toEqual([]);

    const boosted = await projectCoreCleanAnalysis({
      input: '東京', normalized: '東京', limit: 1,
      segments: [{ type: 'word', text: '東京' }],
      raw: [[[[['toukyou', {
        type: 'kanji', text: '東京', kana: 'とうきょう', seq: 1447690,
        score: 325, start: 0, end: 2, isEntity: true
      }, []]], 325]]],
      resolveWord
    });
    const boostedToken = boosted.paths[0]!.tokens[0]!;
    expect(boostedToken.route).toBe('kanji');
    expect(boostedToken.root?.seq).toBe(1447690);
    expect(boostedToken.alternatives).toHaveLength(1);
  });

  test('detects a portable morphology rule-ordinal mismatch', async () => {
    const expected = await projectCoreCleanAnalysis({
      input: '食べた', normalized: '食べた', limit: 1,
      segments: [{ type: 'word', text: '食べた' }],
      raw: [[[[['tabeta', {
        type: 'kanji', text: '食べた', kana: 'たべた', seq: 99,
        conjugations: [99], score: 100, start: 0, end: 3, skipped: 0
      }, []]], 100]]],
      async resolveWord() {
        return {
          root: { seq: 1, form: '食べる', reading: 'たべる' },
          inflection: [{
            pos: 'v1', type: 2, negative: false, formal: false, ordinal: 1
          }]
        };
      }
    });
    const alternative = {
      candidateId: 1, text: '食べた', trueText: null, route: 'kanji' as const,
      reading: 'たべた', romanized: 'tabeta', pos: ['v1'], score: 100,
      entryIndex: 0, root: { seq: 1, form: '食べる', reading: 'たべる' },
      inflection: [{
        pos: 'v1', type: 2, negative: false, formal: false, ordinal: 2
      }],
      components: [], counter: null
    };
    const actual = projectPortableCleanAnalysis({
      input: '食べた', normalized: '食べた', computeMs: 0,
      chunks: [{
        type: 'word', start: 0, end: 3, text: '食べた',
        paths: [{
          score: 100,
          tokens: [{
            ...alternative, start: 0, end: 3, alternatives: [alternative],
            skipped: 0, entity: false
          }]
        }]
      }],
      paths: [{
        score: 100,
        tokens: [{
          ...alternative, start: 0, end: 3, alternatives: [alternative],
          skipped: 0, entity: false
        }]
      }]
    });
    expect(firstCanonicalDifference(expected, actual)?.path).toContain('ordinal');
  });
});
