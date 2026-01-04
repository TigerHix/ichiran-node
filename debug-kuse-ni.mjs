import { GrammarEngine } from './packages/grammar/src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const sentences = [
    '見てたくせに、なんで「見てない」って嘘をつくの？',
    '若いくせに何ダラダラしているんだ。',
    'あの人は先輩のくせに、いつも僕におごらせようとする。',
    'この子は犬のくせにニャーと鳴く。',
    '暑がりな癖に、あの人は毎日セーターを着て仕事に行く。',
    'お金もないくせに、カードで気軽に高い物を買うのはやめた方がいいよ。',
    '男のくせに泣くな',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(60));
    console.log(`Sentence: ${sentence}`);
    const doc = await engine.analyze(sentence);

    // Find くせに token and show context
    for (const token of doc.sentences[0].tokens) {
      if (token.text === 'くせに' || token.lemma === 'くせに') {
        console.log(`Token ${token.i}: "${token.text}" (pos=${token.pos}, tag=${token.tag}, lemma=${token.lemma}, dep=${token.dep}, head=${token.head})`);
        // Show previous token and its head
        if (token.i > 0) {
          const prev = doc.sentences[0].tokens[token.i - 1];
          console.log(`  Previous: "${prev.text}" (pos=${prev.pos}, lemma=${prev.lemma}, dep=${prev.dep}, head=${prev.head}, inflectionForm=${prev.inflectionForm})`);
          // Show the token that previous depends on
          if (prev.head >= 0 && prev.head < doc.sentences[0].tokens.length) {
            const head = doc.sentences[0].tokens[prev.head];
            console.log(`    Prev head: "${head.text}" (pos=${head.pos}, dep=${head.dep})`);
          }
        }
        // Show head of くせに
        if (token.head >= 0 && token.head < doc.sentences[0].tokens.length) {
          const head = doc.sentences[0].tokens[token.head];
          console.log(`  くせに head: "${head.text}" (pos=${head.pos}, dep=${head.dep})`);
        }
      }
    }
  }

  await engine.close();
}

debug().catch(console.error);
