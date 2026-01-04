import { GrammarEngine } from './src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  const testSentences = [
    'やっと木でできている茶碗が買えて、よかった。',
    '今日は卵と鶏肉でできる料理を紹介します。',
    '最近はコンクリートでできる家が人気だ。',
    'この箸は竹でできている。',
    'スーパーは木曜日にミルクからできている製品のセールをするよ！',
    '重金属は超新星爆発からできる。',
    '味噌は大豆からできると職人さんが教えてくれた。',
    '飛行機は軽い素材でできている。',
    'プラスチックやアスファルトは石油からできます。',
    '日本にには木でできている家がたくさんあります。',
    '裏面がガラスでできているスマホは最近売行きがいい。',
    'それはプラスチックからできているとわかっているのに、信じられない。',
    'アレルギーがあるから、ナッツとかナッツからできているものとか食べてはだめよ。',
    '彼の爪はこれでできている。アダマンティウム。非常に硬いもの。',
    'このスマホの裏面はプラスチックでできているみたいだけど、実は塗装したメタルです。',
    'このワインはトップクラスのブドウからできています。どうぞ飲んでください。',
  ];

  for (const sentence of testSentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    const doc = await engine.analyze(sentence);

    if (doc && doc.sentences[0]) {
      const tokens = doc.sentences[0].tokens;
      tokens.forEach(t => {
        console.log(`  ${t.i}: ${t.text} [lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head}]`);
      });
    }
  }

  await engine.close();
}

debug();
