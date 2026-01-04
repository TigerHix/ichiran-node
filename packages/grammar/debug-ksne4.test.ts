import { describe, it, expect } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/index.js';

const failingSentences = [
  '給料を沢山もらっているくせに、貯金がないらしい。',
];

describe('Debug くせに', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  for (const sentence of failingSentences) {
    it(`Analyze: ${sentence}`, async () => {
      const eng = engine.get();
      const result = await eng.analyze(sentence);

      console.log(`\n=== ${sentence} ===`);

      // Look for tokens that might match くせ/癖/くせに/癖に
      const tokens = result?.sentences?.[0]?.tokens || [];
      const kuseTokens = tokens.filter(t =>
        t.text === 'くせ' ||
        t.text === '癖' ||
        t.text === 'くせに' ||
        t.text === '癖に' ||
        t.text === 'なくせ'
      );

      if (kuseTokens.length > 0) {
        console.log('Found potential kuse tokens:');
        kuseTokens.forEach((token, i) => {
          console.log(`  ${i}: ${token.text} (pos: ${token.pos}, lemma: ${token.lemma}, tag: ${token.tag})`);
        });
      } else {
        console.log('No kuse-related tokens found');
      }

      // Find the token before くせ
      const kuseIdx = tokens.findIndex(t => t.text === 'くせ');
      if (kuseIdx > 0) {
        const prevToken = tokens[kuseIdx - 1];
        console.log(`\nToken before くせ (at ${kuseIdx - 1}):`);
        console.log(`  ${prevToken.text} (pos: ${prevToken.pos}, lemma: ${prevToken.lemma}, tag: ${prevToken.tag})`);
      }
    });
  }
});