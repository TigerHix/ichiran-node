import { describe, it } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT2 } from './src/rules/bunpro/index.js';

const sentence = '走って転んだあげくに、壁に衝突した。';

describe('Debug あげく', () => {
  it(`Analyze: ${sentence}`, async () => {
    const eng = useSharedEngine([BUNPRO_JLPT2]).get();
    const result = await eng.analyze(sentence);

    console.log(`\n=== ${sentence} ===\n`);

    const tokens = result.segments.flatMap(s => s.tokens);
    tokens.forEach(token => {
      console.log(`  ${token.text}: pos=${token.pos}, lemma=${token.lemma}, dep=${token.dep}, head=${token.head}`);
    });

    // Try to explain the match
    const explain = await eng.explainMatch(sentence, 'あげく');
    console.log('\n=== Explain Match ===');
    console.log(JSON.stringify(explain, null, 2));
  });
});
