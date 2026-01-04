import { GrammarEngine } from './packages/grammar/src/engine/compiler.js';

const engine = new GrammarEngine([]);

const sentences = [
  '退院しても、まだ車椅子を使わなくてはいけない。',
  '映画館に行っても、見たい映画がない。',
  'あの人に言っても何も変わりません。',
  'お茶は冷たくても美味しいから好きです。',
  '安い電子レンジでも弁当は温められます。',
  'これは明日までに終わらなくてもいいから。',
  '頭が痛くなくてもこの薬を飲んでください。',
  '野菜は好きじゃなくても食べた方がいい。',
  '運転手じゃなくてもシートベルトをしなくてはいけない。',
  '子供に何回も起きてと言っても、起きなかった。',
  '彼女は仕事が大変でも諦めません。',
  '試合に負けても、諦めません。',
  'トレーニングをしても、痩せない。',
  '靴下を履いても、足が冷たいです。',
  'たくさんサラダを食べても、お腹いっぱいにならないんですよ。',
  '遅くなっても、是非家に寄ってください。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));
  const doc = await engine.analyze(sent);
  console.log(JSON.stringify(doc, null, 2));
}
