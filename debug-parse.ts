import { getSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

async function debug() {
  const engine = await getSharedEngine([BUNPRO_JLPT4]);

  const sentences = [
    '電車は乗り物のひとつです。',
    'キリスト教は宗教のひとつだ。',
    '彼も家族のひとりだ。',
    'このクレヨンはこのセットのいっぽんだ。',
    'トマトはフルーツのいっしゅだ。',
  ];

  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await engine.analyze(sentence);
    for (const token of doc.sentences[0].tokens) {
      console.log(`${token.i}: ${token.text} (lemma=${token.lemma}, pos=${token.pos}, dep=${token.dep})`);
    }
  }

  await engine.client.stop();
}

debug().catch(console.error);
