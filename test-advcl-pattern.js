import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = [
  'お酒もありだけど今日はジュースを飲もうかな。',
  'この香りもありだけど、あの香りの方が好き！',
  'あの白いドレスもありだし、ピンクのドレスも君に似合うよ。',
  'あの職場は近いからありだが雰囲気がイマイチだ。',
];

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      if (tok.text === 'あり' || tok.text === 'だ' || tok.lemma === 'だ' || tok.text === 'けど' || tok.text === 'だし' || tok.text === 'だが') {
        console.log(`text=${tok.text.padEnd(5)} lemma=${tok.lemma.padEnd(8)} pos=${tok.pos.padEnd(6)} dep=${tok.dep.padEnd(8)} head=${tok.head}`);
      }
    }
  }
}

await engine.close();
