import { describe, it, expect } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/index.js';

const sentence = '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。';

describe('Debug くせに', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

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

    // Find the token before and after kuse
    const kuseIdx = tokens.findIndex(t => t.text === 'くせ');
    if (kuseIdx >= 0) {
      if (kuseIdx > 0) {
        const prevToken = tokens[kuseIdx - 1];
        console.log(`\nToken before くせ (at ${kuseIdx - 1}):`);
        console.log(`  ${prevToken.text} (pos: ${prevToken.pos}, lemma: ${prevToken.lemma}, tag: ${prevToken.tag})`);
      }
      if (kuseIdx < tokens.length - 1) {
        const nextToken = tokens[kuseIdx + 1];
        console.log(`\nToken after くせ (at ${kuseIdx + 1}):`);
        console.log(`  ${nextToken.text} (pos: ${nextToken.pos}, lemma: ${nextToken.lemma}, tag: ${nextToken.tag})`);
      }
    }

    // Look for な tokens
    const naTokens = tokens.filter(t => t.text === 'な');
    if (naTokens.length > 0) {
      console.log('\nFound な tokens:');
      naTokens.forEach((token, i) => {
        console.log(`  ${i}: ${token.text} (pos: ${token.pos}, lemma: ${token.lemma}, tag: ${token.tag})`);
      });
    }

    // Look for がる tokens
    const garuTokens = tokens.filter(t => t.lemma === 'がる');
    if (garuTokens.length > 0) {
      console.log('\nFound がる tokens:');
      garuTokens.forEach((token, i) => {
        console.log(`  ${i}: ${token.text} (pos: ${token.pos}, lemma: ${token.lemma}, tag: ${token.tag})`);
      });
    }
  });
});