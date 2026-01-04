import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import { GrammarEngine } from './packages/grammar/src/program.js';

async function main() {
  const client = new GinzaClient({ python: 'python3' });
  await client.start();
  const engine = await GrammarEngine.create([], { client });

  const testSentences = [
    'とまれ',
    'だせ',
    'がんばれ',
    'かえせ',
    'はなれろ',
    'すてろ',
    'ねろ',
    'のめ',
    'あやまれ',
    'にげろ',
    'たべろ',
    'しろ',
    'きをつけろ',
    'きれ',
    'もってこい',
  ];

  for (const sentence of testSentences) {
    const doc = await engine.analyze(sentence);
    console.log(`\n=== ${sentence} ===`);
    if (!doc) {
      console.log('  (null doc)');
      continue;
    }
    for (const s of doc.sentences) {
      for (const token of s.tokens) {
        console.log(`  text: ${token.text.padEnd(10)} lemma: ${token.lemma.padEnd(10)} pos: ${token.pos.padEnd(6)} inflectionForm: ${token.inflectionForm || '(none)'}`);
      }
    }
  }
}

main().catch(console.error);
