import { GrammarEngine } from './src/index.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/index.js';

const engine = new GrammarEngine([BUNPRO_JLPT3]);

const skippedSentences = {
  'noun-型': [
    'このかたのスマホはよく壊れます。',
  ],
  'particle-の': [
    '今までの対立の原因は広く知られていない。',
    'あちゃんとの握手会に行こう！',
  ],
  'verb-volitionalとする': [
    '禁煙区域でタバコをすおうとする人が多いので、監視装置を設置することになった。',
  ],
  'かけ': [
    '私は、何冊もよみかけの本がある。',
    'のみかけのジュースがあるのを忘れていた。',
  ],
  'かというと2': [
    'イチカさん：「ミクちゃんは好きな人がいるみたいに見えるよ。」イツキさん：「誰が好きかっていうときっとフウタロウくんでしょう。」',
  ],
  'くせに': [
    '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。',
  ],
  'ことがある': [
    '頑張ってもうまくいかないことがある。',
    'ルームメイトの無作法な振る舞いに注意したいことがよくある。でも、私は気が弱すぎる・・・',
  ],
  'さ-interjection': [
    '「さ、食べて。冷めないうちに。」',
    '「さ、早く乗ってください。」「うん。」',
    '「さ、そろそろ帰る時間だぞ。」',
  ],
  'すると': [
    '久しぶりに押入れの掃除をした。すると、無くしたと思っていた服が出てきた。',
  ],
  'たて': [
    'ここにあるのは全部揚げたてですよ。',
  ],
  'ても-なくても': [
    'つれてもつれなくても、釣りは楽しいらしい。',
  ],
  'ないうちに': [
    'いくらもおよがないうちに、向こう岸についてしまった。',
  ],
};

async function analyzeSentence(ruleName: string, sentence: string) {
  try {
    const result = await engine.analyze(sentence);
    console.log(`\n=== ${ruleName}: ${sentence} ===`);
    for (const seg of result.segments) {
      console.log(`  [${seg.text}]`);
      if (seg.conjugation) {
        console.log(`    Conjugation: ${JSON.stringify(seg.conjugation)}`);
      }
      for (const [key, value] of Object.entries(seg.grammar || {})) {
        console.log(`    Grammar: ${key} = ${value}`);
      }
    }
  } catch (e) {
    console.log(`\n=== ${ruleName}: ${sentence} ===`);
    console.log(`  ERROR: ${e}`);
  }
}

async function main() {
  for (const [ruleName, sentences] of Object.entries(skippedSentences)) {
    for (const sentence of sentences.slice(0, 1)) { // Just first one for now
      await analyzeSentence(ruleName, sentence);
    }
  }
}

main().catch(console.error);
