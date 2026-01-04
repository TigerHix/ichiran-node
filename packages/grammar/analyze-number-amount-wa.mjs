/**
 * Analyze GiNZA parses for number-amount-は test sentences
 */
import { GrammarEngine } from './dist/program.js';
import { BUNPRO_JLPT4 } from './dist/rules/bunpro/jlpt4/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT4]);

const sentences = [
  'ディズニーランドには年に５回は行っている。',
  '私は毎日、テレビを５時間は見ている。',
  '２キロくらいはあると思う。',
  '倉敷には１回ぐらいは行った方がいいよ。',
  '囚人のうち少なくとも６人は逃げたらしい。',
  'クリスマスぐらいは家に帰って来てね？',
  '肉ぐらいは少し食べてよ。',
  'この食べ物は少なくとも床に３秒は落ちていたから食べられない。',
  '彼氏は１日８回は電話をかける。',
  'スティーブン・ホーキングに一回は会いたかったのに…',
  '怒った親：「話す前にちょっとは考えるのが普通だろ！」',
  '沖縄に一度は行こうと思っています。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  for (const [i, t] of doc.tokens.entries()) {
    console.log(`#${i} ${t.text}\tpos=${t.pos}\tlemma=${t.lemma}\ttag=${t.tag}\tdep=${t.dep}\thead=${t.head}`);
  }
}
