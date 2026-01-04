import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentences = [
  'スマホをつかわせてくれないか？',
  '夜遅くに電話しないでくれない？',
  'うちの犬を見たら電話してくれないか？',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(60));
  console.log('SENTENCE:', sent);
  console.log('='.repeat(60));
  const doc = await engine.analyze(sent);
  if (!doc || !doc.sentences[0]) {
    console.log('Failed to parse');
    continue;
  }
  const tokens = doc.sentences[0].tokens;
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    const inflection = t.inflectionForm || 'N/A';
    console.log(`[${i}] text="${t.text}" lemma="${t.lemma}" pos="${t.pos}" dep="${t.dep}" head=${t.head} inflection="${inflection}"`);
  }
  await engine.close();
  break;
}

process.exit(0);
