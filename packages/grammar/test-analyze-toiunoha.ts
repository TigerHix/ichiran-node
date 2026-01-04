import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

const engine = useSharedEngine([BUNPRO_JLPT3]);

const testSentences = [
  '彼女が二十歳だというのはじじつだ。',
  'この人が私の母親を殺したというのはじじつだ。',
  '彼女と仲直りしたのは事実だ。',
  '赤信号を無視したのは事実だ。',
];

async function main() {
  console.log('=== ANALYZING SENTENCES ===\n');
  for (const sentence of testSentences) {
    const doc = await engine.analyze(sentence);
    console.log(`Sentence: ${sentence}`);
    console.log('Tokens:');
    doc.tokens.forEach((t, i) => {
      console.log(`  [${i}] ${t.text} (pos: ${t.pos}, lemma: ${t.lemma}, inflectionForm: ${t.inflectionForm}, head: ${t.head})`);
    });
    console.log();
  }
}

main().catch(console.error);
