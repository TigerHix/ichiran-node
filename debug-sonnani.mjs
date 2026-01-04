#!/usr/bin/env bun
import { GrammarEngine } from './packages/grammar/src/program.ts';

async function main() {
  const engine = await GrammarEngine.create([]);

  const sentences = [
    'そんなに食べたらお腹を壊すよ。',
    'このケーキはそんなに甘いんですか。',
    'そんなに痛いなら、病院に行ったほうがいいんじゃないですか。',
    'そんなに頑張っても、彼みたいに出来ない。',
    'もうお金がそんなにないから、買うのをやめましょう。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Analyzing: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);

    if (!doc || !doc.sentences[0]) continue;

    const tokens = doc.sentences[0].tokens;
    for (const token of tokens) {
      if (token.text.includes('そんな') || token.text === 'に') {
        console.log(`  [${token.i}] ${token.text} (lemma=${token.lemma}, pos=${token.pos}, dep=${token.dep}, head=${token.head}, inflectionForm=${token.inflectionForm || 'N/A'})`);
      }
    }
  }

  await engine.close();
}

main().catch(console.error);
