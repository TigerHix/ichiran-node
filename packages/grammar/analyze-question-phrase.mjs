#!/usr/bin/env bun
import { GrammarEngine } from './src/program.js';

const engine = new GrammarEngine([]);

async function analyzeSentence(sentence) {
  console.log('\n========================================');
  console.log(`Sentence: ${sentence}`);
  console.log('========================================');

  const doc = await engine.analyze(sentence);

  console.log('\nTokens:');
  doc.tokens.forEach((token, i) => {
    const extras = [];
    if (token.inflectionForm) extras.push(`inf=${token.inflectionForm}`);
    if (token.conjugationClass) extras.push(`conj=${token.conjugationClass}`);
    console.log(`  [${i}] ${token.text.padEnd(10)} lemma=${token.lemma.padEnd(10)} pos=${token.pos.padEnd(6)} dep=${token.dep.padEnd(8)} ${extras.join(' ')}`);
  });
}

// Test the failing sentences
await analyzeSentence('彼女の嫌いなレストランがどこか確認してください。');
await analyzeSentence('何でこのビルを壊すか知っていますか。');
await analyzeSentence('忘年会に社長が来るか分かりますか？');
await analyzeSentence('いつ美術館に行くか決めましたか？');
await analyzeSentence('彼の好みの味が何かわかる？');
await analyzeSentence('誰が一番上手かわかるかな。');
