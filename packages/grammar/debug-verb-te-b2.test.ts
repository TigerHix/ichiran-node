import { describe, test, expect } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('Debug: verbて-b2 parsing', () => {
  const engine = useSharedEngine([]);

  test('check WORKING example: 帰って悲しい', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('メアリーがアメリカに帰って悲しいです。');
    console.log('\n=== メアリーがアメリカに帰って悲しいです。 (WORKING) ===');
    if (doc && doc.sentences && doc.sentences[0]) {
      doc.sentences[0].tokens.forEach((t, i) => {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      });
    }
  });

  test('check noun + de parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('病気で参加できませんでした。');
    console.log('\n=== 病気で参加できませんでした。 ===');
    if (doc && doc.sentences && doc.sentences[0]) {
      doc.sentences[0].tokens.forEach((t, i) => {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      });
    }
  });

  test('check verb te-form parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('日本語能力試験に合格して嬉しかったです。');
    console.log('\n=== 日本語能力試験に合格して嬉しかったです。 ===');
    if (doc && doc.sentences && doc.sentences[0]) {
      doc.sentences[0].tokens.forEach((t, i) => {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      });
    }
  });

  test('check adj te-form parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('文字もちいさすぎて読めないよ。');
    console.log('\n=== 文字もちいさすぎて読めないよ。 ===');
    if (doc && doc.sentences && doc.sentences[0]) {
      doc.sentences[0].tokens.forEach((t, i) => {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      });
    }
  });

  test('check na-adj de parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('部屋がきれいでのんびりできます。');
    console.log('\n=== 部屋がきれいでのんびりできます。 ===');
    if (doc && doc.sentences && doc.sentences[0]) {
      doc.sentences[0].tokens.forEach((t, i) => {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      });
    }
  });
});
