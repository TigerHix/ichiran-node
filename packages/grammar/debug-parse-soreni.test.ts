import { describe } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('debug-soreni', () => {
  const engine = useSharedEngine([]).get();

  it('should parse sentences correctly', async () => {
    const result = await engine.analyze('日本は住みやすい。それに病院代が安い。');
    console.log('\n=== Sentence: 日本は住みやすい。それに病院代が安い。 ===');
    for (const tok of result.tokens) {
      console.log(`text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" dep="${tok.dep}"`);
    }

    const result2 = await engine.analyze('それに彼は家事もしないんでしょう？');
    console.log('\n=== Sentence: それに彼は家事もしないんでしょう？ ===');
    for (const tok of result2.tokens) {
      console.log(`text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" dep="${tok.dep}"`);
    }
  });
});
