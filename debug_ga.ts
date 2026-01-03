import { GiNZA } from './packages/grammar/src/ginza/index.js';

async function main() {
  const ginza = new GiNZA();

  const sentences = [
    '毎日走るが、運動はきらいです。',
    'お金は大切だが、時間も大切だ。',
    'このカレーは辛いが、美味しい。',
    '大変ですが、面白いです。',
    'すみませんが、この漢字の意味は何ですか。',
    // Subject marker for comparison
    '私が行きます。',
  ];

  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await ginza.parse(sentence);

    for (let i = 0; i < doc.tokens.length; i++) {
      const token = doc.tokens[i];
      if (token.text === 'が') {
        console.log(`Token ${i}: が`, {
          text: token.text,
          pos: token.pos,
          dep: token.dep,
          lemma: token.lemma,
          head: token.head,
          headText: doc.tokens[token.head]?.text,
          features: token.features,
        });
      }
    }
  }
}

main().catch(console.error);
