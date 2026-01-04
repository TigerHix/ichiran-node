import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], { ginza: { python: 'python3' } });

  // Test sentences from the JSON data
  const sentences = [
    'バスは今大阪にきています。',
    'E.T.は家に帰っている。',
    'ピアノが落ちている。',
    'クラスは始まっている。',
    'あの犬は死んでいるだろう、悲しいね。',
    'パーティーは始まっている。',
    '電車は東京にいっています。',
    'ななさんのバナナは腐っています。',
    '喫茶店はもう閉まっています。',
    'サスケさんは結婚していないでしょう？',
    'お前はもう死んでる。',
    'あの人たちは、あそこに並んでいるのですか？',
    'あの映画のタイトル、知ってる？',
    'テストに失敗したからお母さんは怒っている。',
    '先生がめちゃ怒っている。',
    'お母さんは今買い物に行っています。',
    'あのカバは太っていない。',
  ];

  for (const sentence of sentences.slice(0, 5)) {
    console.log('\n' + '='.repeat(100));
    console.log(`SENTENCE: ${sentence}`);
    console.log('='.repeat(100));
    const doc = await engine.analyze(sentence);
    if (doc && doc.sentences[0]) {
      const sent = doc.sentences[0];
      for (const tok of sent.tokens) {
        console.log(`  [${tok.id}] ${tok.text.padEnd(10)} pos=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(6)} head=${tok.head} inflectionForm=${tok.inflectionForm || 'N/A'}`);
      }
    }
  }

  await engine.close();
}

main();
