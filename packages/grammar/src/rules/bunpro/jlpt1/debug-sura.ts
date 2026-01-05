import { getSharedEngine } from '../_test/engine.js';
import { BUNPRO_JLPT1 } from './index.js';

async function debug() {
  const engine = await getSharedEngine([BUNPRO_JLPT1]);
  
  const sentence = '何日たったのかすら';
  console.log('Analyzing:', sentence);
  console.log('');
  
  const doc = await (engine as any).client.analyze(sentence);
  console.log('Tokens:');
  doc.tokens.forEach((t: any, i: number) => {
    console.log(`  [${i}] ${t.text} (pos=${t.pos}, lemma=${t.lemma}, dep=${t.dep})`);
  });
  console.log('');
  
  const hits = await engine.match(sentence);
  console.log('Hits:', hits.length);
  hits.forEach(hit => {
    console.log(`  - ${hit.ruleId}:`, hit.captures);
  });
}

debug().then(() => process.exit(0));
