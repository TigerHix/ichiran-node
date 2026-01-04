import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();
await client.start();

async function debugParse(text) {
  const docs = await client.analyze([text]);
  const sent = docs[0].sentences[0];
  console.log('\n===', text, '===');
  console.log('Tokens:');
  for (const tok of sent.tokens) {
    console.log(`  "${tok.text}"`);
    console.log(`    lemma: ${tok.lemma}, pos: ${tok.pos}, tag: ${tok.tag}, dep: ${tok.dep}`);
    console.log(`    head: ${tok.head}, inflectionForm: ${tok.inflectionForm || '-'}`);
  }
}

await debugParse('電話します');
await debugParse('お電話します');
await debugParse('おやすみします');
await debugParse('勉強します');

await client.stop();
