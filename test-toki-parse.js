// Test script to analyze how GiNZA parses とき sentences
import { analyze } from './packages/grammar/src/engine/ginza.js';

async function test() {
  const sentences = [
    '散歩をするときに音楽を聴く。',
    '授業のときは静かにしなくてはいけない。',
    '大変なときに彼の親は亡くなった。',
    'あの映画を見たとき、泣いた。',
    '雨のときは家でゴロゴロしています。',
    '寒いときは、お風呂に入りたくなる。',
    '写真を撮るときは、笑ってください。',
  ];

  for (const sent of sentences) {
    console.log('\n' + '='.repeat(60));
    console.log('SENTENCE:', sent);
    console.log('='.repeat(60));
    const doc = await analyze(sent);
    doc.tokens.forEach((t, i) => {
      console.log(`[${i}] ${t.text.padEnd(10)} POS=${t.pos.padEnd(6)} lemma=${t.lemma.padEnd(10)} dep=${t.dep.padEnd(6)} head=${t.head} inflection=${t.inflectionForm || '-'}`);
    });
  }
}

test().catch(console.error);
