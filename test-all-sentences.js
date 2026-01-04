import { GrammarEngine } from './packages/grammar/src/program.js';
import { readFileSync } from 'fs';
const data = JSON.parse(readFileSync('./packages/grammar/data/bunpro/JLPT3/あり.json', 'utf-8'));

const engine = await GrammarEngine.create([], {
  ginza: { python: 'python3' },
});

const sentences = data.included
  .filter(item => item.type === 'study_question')
  .map(item => item.attributes.content.replace(/<[^>]*>/g, '').replace(/【[^】]*】/g, '').replace(/（[^）]*）/g, '').replace(/\r\n/g, '').trim())
  .filter(s => s.includes('____'))
  .map(s => s.replace('____', 'あり'))
  .slice(0, 15);

console.log(`Testing ${sentences.length} sentences...\n`);

for (const s of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(s);
  console.log('='.repeat(80));
  const doc = await engine.analyze(s);
  if (doc && doc.sentences[0]) {
    for (const tok of doc.sentences[0].tokens) {
      if (tok.text === 'あり' || tok.lemma === 'あり' || tok.lemma === 'ある') {
        console.log(`✓ ${tok.text}: POS=${tok.pos} lemma=${tok.lemma} dep=${tok.dep} inflectionForm=${tok.inflectionForm || 'UNSET'} head=${tok.head}`);
      }
    }
  }
}

await engine.close();
