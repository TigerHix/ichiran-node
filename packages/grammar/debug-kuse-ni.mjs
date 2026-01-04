import { GrammarEngine } from './dist/program.js';
import { rules } from './dist/rules/bunpro/index.js';

const ginza = new (await import('./dist/ginza/client.js')).GinzaClient();
await ginza.start();

const sentences = [
  '見てたくせに、なんで「見てない」って嘘をつくの？',
  '若いくせに何ダラダラしているんだ。',
  'この子は犬のくせにニャーと鳴く。',
  '休みで時間があるくせに、家の手伝いを何もしない。',
  '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。',
];

for (const s of sentences) {
  console.log('=== ' + s + ' ===');
  const doc = await ginza.analyze([s]);
  if (doc && doc[0]) {
    for (const t of doc[0].tokens) {
      if (t.text.includes('くせ') || t.text === 'に' || t.lemma?.includes('くせ')) {
        console.log(`  ${t.text} | pos=${t.pos} | tag=${t.tag} | dep=${t.dep} | lemma=${t.lemma}`);
      }
    }
  }
}

await ginza.stop();
