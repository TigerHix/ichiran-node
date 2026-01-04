import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

const client = new GinzaClient();
await client.start();
const engine = GrammarEngine.create([BUNPRO_JLPT4], { client });

const sentences = [
  '一緒にたべにいってほしいんです。',
  'もう一回説明してほしいです。',
  '車できてほしかった。',
  '一緒に勉強してほしいです。',
  'コーラを買ってほしいです。',
  '来てほしい。',
  '私に見てほしい。',
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`Sentence: ${sentence}`);
  console.log('='.repeat(80));

  // Get GiNZA parse
  const docs = await client.analyze([sentence]);
  const doc = docs[0];
  const parsed = doc.sentences[0];
  console.log('\nGiNZA Parse:');
  for (let i = 0; i < parsed.tokens.length; i++) {
    const t = parsed.tokens[i];
    console.log(`  [${i}] ${t.text.padEnd(12)} lemma=${t.lemma.padEnd(12)} pos=${t.pos.padEnd(8)} dep=${t.dep.padEnd(10)} inflection=${t.inflectionForm || '-'} head=${t.head}`);
  }

  // Try to match
  const hits = await engine.match(sentence);
  const hit = hits.find(h => h.ruleId === 'てほしい');
  console.log('\nMatch:', hit ? 'YES' : 'NO');

  if (!hit) {
    const explain = await engine.explainMatch(sentence, 'てほしい');
    console.log('Reason:', explain.reason);
    if (explain.partialBinding) {
      console.log('Partial bindings:', JSON.stringify(explain.partialBinding, null, 2));
    }
  } else {
    console.log('Captures:', JSON.stringify(hit.captures, null, 2));
  }
}

await client.close();
