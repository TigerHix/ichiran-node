import { describe, beforeEach, afterEach } from 'bun:test';
import { GiNZAEngine } from './packages/grammar/src/engine/ginza.js';

async function analyze() {
  const engine = new GiNZAEngine();

  const sentences = [
    '今朝、雪が降りはじめた。',
    '来月からお金をためはじめます。',
    '歌を歌いはじめる。',
    '雨がふりはじめてきた。',
    '高校生の時にお金をためはじめた。',
    '練習しはじめてください。',
    'また騒ぎはじめた。',
    '９時になってから、働きはじめた。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`SENTENCE: ${sentence}`);
    console.log('='.repeat(80));

    const doc = await engine.analyze(sentence);

    // Find tokens with はじめる
    const hajimeruTokens = doc.tokens.filter(t =>
      t.lemma === 'はじめる' || t.text.includes('はじめ') || t.text.includes('ハジメ')
    );

    if (hajimeruTokens.length > 0) {
      console.log('\nTokens related to はじめる:');
      for (const t of hajimeruTokens) {
        const idx = doc.tokens.indexOf(t);
        console.log(`  [${idx}] ${t.text}`);
        console.log(`      lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}`);
        console.log(`      inflectionForm=${t.inflectionForm}, head=${t.head}`);
      }
    } else {
      console.log('\nNo はじめる tokens found!');
    }

    // Show all tokens for context
    console.log('\nAll tokens:');
    doc.tokens.forEach((t, i) => {
      console.log(`  [${i}] ${t.text.padEnd(15)} lemma=${t.lemma.padEnd(15)} pos=${t.pos.padEnd(6)} dep=${t.dep.padEnd(6)} inflectionForm=${t.inflectionForm || 'N/A'}`);
    });
  }

  await engine.cleanup();
  process.exit(0);
}

analyze().catch(console.error);
