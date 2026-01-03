import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5]);
  
  const sentences = [
    // Valid sentence-ending よ
    '今日は水曜日だよ。',
    'トムは足が早いよ。',
    'これはお風呂ですよ。',
    'それは熱いよ。',
    'あれは先生だよね。',
    // Invalid cases
    '四月よふみは誰ですか。',
    '良い天気です。',
    '四月よ',
  ];
  
  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await engine.analyze(sentence);
    if (doc && doc.sentences[0]) {
      const sent = doc.sentences[0];
      for (let i = 0; i < sent.tokens.length; i++) {
        const t = sent.tokens[i];
        console.log(`[${i}] text="${t.text}" lemma="${t.lemma}" pos="${t.pos}" dep="${t.dep}" head=${t.head}`);
      }
    }
  }
  
  await engine.close();
}

main().catch(console.error);
