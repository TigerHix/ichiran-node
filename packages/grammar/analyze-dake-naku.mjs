import { GrammarEngine } from './dist/program.js';
import { BUNPRO_JLPT3 } from './dist/rules/bunpro/jlpt3/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT3]);

const failingSentences = [
  'アイスクリームだけじゃなくて、プリンも買いました。',
  '君だけじゃなく、僕も悪かったと思っている。',
  '実は、それだけじゃなくて、もっと悪い報告もあるんです…',
  'お年玉は子供だけじゃなく、大人ももらえたらいいのにね。',
  '遊園地だけじゃなくて動物園にも連れて行ってほしい。',
  '机だけじゃなくて、椅子もほしい。',
  '体だけじゃなく、心も疲れてしまった。',
  '人間だけじゃなく、動物も涙を流すらしい。',
  '彼は強いだけではなく、とても優しい人だ。',
  '車だけじゃなく、船も運転できます。',
  '鳥だけじゃなくて、猫にも餌をあげています。',
  '原因はそれだけじゃなく、他にもあると思います。',
  '私だけじゃなく、何人もの社員が反対しています。',
];

const falsePositive = '日本語だけでなく、韓国語も話せる。';

async function main() {
  console.log('=== FAILING SENTENCES ===\n');
  for (const sentence of failingSentences) {
    const doc = await engine.analyze(sentence);
    console.log(`Sentence: ${sentence}`);
    console.log('Tokens:');
    doc.tokens.forEach((t, i) => {
      console.log(`  [${i}] ${t.text} (pos: ${t.pos}, lemma: ${t.lemma}, inflectionForm: ${t.inflectionForm}, head: ${t.head})`);
    });
    console.log();
  }

  console.log('\n=== FALSE POSITIVE ===\n');
  const doc = await engine.analyze(falsePositive);
  console.log(`Sentence: ${falsePositive}`);
  console.log('Tokens:');
  doc.tokens.forEach((t, i) => {
    console.log(`  [${i}] ${t.text} (pos: ${t.pos}, lemma: ${t.lemma}, inflectionForm: ${t.inflectionForm}, head: ${t.head})`);
  });
}

main().catch(console.error);
