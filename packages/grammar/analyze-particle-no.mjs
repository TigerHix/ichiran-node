import { GrammarEngine } from './dist/engine/lang.js';

async function main() {
  const engine = new GrammarEngine();
  
  const sentences = [
    'これはアメリカからのお土産です。',
    '別れた後、彼との関係はどうなるんだろう。',
    '海外への手紙はこちらのポストにお入れください。',
    '環境に悪いので車での通勤は控えてください。',
    '出発までの時間、何をします？',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    for (const tok of doc.tokens) {
      console.log(`[${tok.i}] ${tok.text}`, `pos=${tok.pos}`, `dep=${tok.dep}`, `head=${tok.head}`, `lemma=${tok.lemma}`);
    }
  }
}

main().catch(console.error);
