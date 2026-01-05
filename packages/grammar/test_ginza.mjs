import { GinzaClient } from './src/ginza/client.js';

async function main() {
  const client = new GinzaClient();
  await client.ready();
  
  const sentence = '授業の終了を告げるチャイムが鳴るがはやいか、生徒たちは教室を足速に去っていった。';
  const doc = await client.parse(sentence);
  
  console.log('Sentence:', sentence);
  console.log('\nTokens:');
  for (let i = 0; i < doc.tokens.length; i++) {
    const t = doc.tokens[i];
    console.log(`${i}: "${t.text}" [pos=${t.pos}, lemma="${t.lemma}"]`);
  }
  
  // Find the pattern
  console.log('\nLooking for がはやいか pattern:');
  for (let i = 0; i < doc.tokens.length; i++) {
    if (doc.tokens[i].text === 'が') {
      console.log(`\nFound が at index ${i}`);
      for (let j = i; j < Math.min(i + 6, doc.tokens.length); j++) {
        const t = doc.tokens[j];
        console.log(`  ${j}: "${t.text}" [pos=${t.pos}, lemma="${t.lemma}"]`);
      }
    }
  }
  
  await client.close();
}

main().catch(console.error);
