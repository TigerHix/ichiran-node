import { describe, test, expect } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('Debug: verbて-b2 parsing', () => {
  const engine = useSharedEngine([]);

  test('check noun + de parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('病気で参加できませんでした。');
    console.log('\n=== 病気で参加できませんでした。 ===');
    doc.tokens.forEach((t, i) => {
      if (t.pos === 'NOUN' || t.pos === 'SCONJ' || t.pos === 'ADP' || t.text === 'で') {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      }
    });
  });

  test('check verb te-form parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('日本語能力試験に合格して嬉しかったです。');
    console.log('\n=== 日本語能力試験に合格して嬉しかったです。 ===');
    doc.tokens.forEach((t, i) => {
      if (t.pos === 'VERB' || t.pos === 'AUX' || t.pos === 'SCONJ' || t.text === 'て') {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      }
    });
  });

  test('check adj te-form parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('文字もちいさすぎて読めないよ。');
    console.log('\n=== 文字もちいさすぎて読めないよ。 ===');
    doc.tokens.forEach((t, i) => {
      if (t.pos === 'ADJ' || t.pos === 'AUX' || t.pos === 'SCONJ' || t.text === 'て') {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      }
    });
  });

  test('check na-adj de parse', async () => {
    const eng = engine.get();
    const doc = await eng.analyze('部屋がきれいでのんびりできます。');
    console.log('\n=== 部屋がきれいでのんびりできます。 ===');
    doc.tokens.forEach((t, i) => {
      if (t.pos === 'NOUN' || t.pos === 'ADJ' || t.pos === 'AUX' || t.pos === 'SCONJ' || t.pos === 'ADP' || t.text === 'で') {
        console.log(`[${i}] ${t.text}: pos=${t.pos} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
      }
    });
  });
});
