import { describe } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

async function main() {
  const { get } = useSharedEngine([]);
  const engine = await get();

  const sentence = '毎日走るが、運動はきらいです。';
  const doc = await engine.analyze(sentence);

  console.log('\n=== ' + sentence + ' ===');
  console.log('All tokens:');
  for (let i = 0; i < doc.tokens.length; i++) {
    const token = doc.tokens[i];
    console.log(`  [${i}] ${token.text}: pos=${token.pos}, dep=${token.dep}, lemma=${token.lemma}`);
  }

  // Now test with subject marker
  const sentence2 = '私が行きます。';
  const doc2 = await engine.analyze(sentence2);

  console.log('\n=== ' + sentence2 + ' ===');
  console.log('All tokens:');
  for (let i = 0; i < doc2.tokens.length; i++) {
    const token = doc2.tokens[i];
    console.log(`  [${i}] ${token.text}: pos=${token.pos}, dep=${token.dep}, lemma=${token.lemma}`);
  }
}

main().catch(console.error);
