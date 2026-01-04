import { GrammarEngine } from './program.js';
import { rules } from './rules/bunpro/index.js';

const ginza = new (await import('./ginza/client.js')).GinzaClient();
const engine = await GrammarEngine.create(rules, { client });

const sentences = [
  '見てたくせに、なんで「見てない」って嘘をつくの？',
  '若いくせに何ダラダラしているんだ。',
  'この子は犬のくせにニャーと鳴く。',
  '休みで時間があるくせに、家の手伝いを何もしない。',
  '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。',
];

for (const s of sentences) {
  console.log('=== ' + s + ' ===');
  const result = engine.analyze(s);
  for (const t of result.tokens) {
    if (t.text.includes('くせ') || t.text === 'に' || t.lemma?.includes('くせ')) {
      console.log(`  ${t.text} | pos=${t.pos} | tag=${t.tag} | dep=${t.dep} | lemma=${t.lemma}`);
    }
  }
}
