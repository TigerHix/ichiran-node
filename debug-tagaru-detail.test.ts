import { describe, test } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

describe('Debug: Detailed たがる analysis', () => {
  const engine = useSharedEngine([]);

  test('present form - たがる', async () => {
    const e = engine.get();
    const doc = await e.analyze('彼は動物園に行きたがる。');

    const tokens = doc.sentences[0].tokens;
    const verbStem = tokens[4];  // 行き
    const tagaru = tokens[5];    // たがる

    console.log('Verb stem (行き):');
    console.log(`  text: ${verbStem.text}, lemma: ${verbStem.lemma}, pos: ${verbStem.pos}, dep: ${verbStem.dep}`);
    console.log(`  inflectionForm: ${verbStem.inflectionForm}`);
    console.log(`  head index: ${verbStem.head} -> points to token ${verbStem.head}`);

    console.log('\nTagaru (たがる):');
    console.log(`  text: ${tagaru.text}, lemma: ${tagaru.lemma}, pos: ${tagaru.pos}, dep: ${tagaru.dep}`);
    console.log(`  inflectionForm: ${tagaru.inflectionForm}`);
    console.log(`  head index: ${tagaru.head} -> points to token ${tagaru.head} (${tokens[tagaru.head]?.text})`);
  });

  test('te-form - たがって', async () => {
    const e = engine.get();
    const doc = await e.analyze('彼はパソコンを買いたがって、週末も働いた。');

    const tokens = doc.sentences[0].tokens;
    tokens.forEach((t, i) => {
      if (t.lemma === 'たがる' || i === 6) {  // token 6 is kaitagatta
        console.log(`Token ${i} (${t.text}):`);
        console.log(`  lemma: ${t.lemma}, pos: ${t.pos}, dep: ${t.dep}`);
        console.log(`  inflectionForm: ${t.inflectionForm}`);
        console.log(`  head index: ${t.head} -> points to token ${t.head} (${tokens[t.head]?.text})`);
        console.log('');
      }
    });
  });
});
