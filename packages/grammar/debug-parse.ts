import { GrammarEngine } from './packages/grammar/src/engine/index.ts';
import { loadGinza } from './packages/grammar/src/ginza/client.ts';

async function main() {
  const ginza = await loadGinza();
  const engine = new GrammarEngine(ginza);

  const testSentence = '元気ではないことはないけど、すごく元気なわけでもない。';
  console.log('=== Testing:', testSentence, '===\n');

  const doc = await engine.analyze(testSentence);
  console.log(JSON.stringify(doc, null, 2));

  console.log('\n\n=== Testing match ===');
  const result = await engine.explainMatch(testSentence, 'ないことはない');
  console.log(JSON.stringify(result, null, 2));
}

main().catch(console.error);
