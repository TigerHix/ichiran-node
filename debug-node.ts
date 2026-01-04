import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });
  const sentences = [
    '私は弱いので、ジムへ行きます。',
    '今日は寒いので、コートを着ます。',
    '景色が綺麗なので、写真を撮ります。',
  ];

  for (const sentence of sentences) {
    console.log('\n===', sentence, '===');
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc.tokens.map(t => ({
      text: t.text,
      lemma: t.lemma,
      pos: t.pos,
      dep: t.dep,
      head: t.head
    })), null, 2));
  }

  await engine.close();
}

main().catch(console.error);
