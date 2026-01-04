import { describe, test } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

describe('Debug: Analyze all たがる conjugations', () => {
  const engine = useSharedEngine([]);

  const sentences = [
    '彼は動物園に行きたがる。',      // present
    'リサがあのサンドイッチを見て、食べたがった。',  // past
    '彼はパソコンを買いたがって、週末も働いた。',    // te-form
    '君のようになりたがっている人が居ますよ。',      // progressive (-teiru)
    'あの子が帰りたがるならいいんじゃない？',        // conditional
  ];

  for (const sentence of sentences) {
    test(sentence, async () => {
      const e = engine.get();
      const doc = await e.analyze(sentence);

      // Find tokens related to tagaru
      const tagaruTokens = doc.sentences[0].tokens.filter(t =>
        t.lemma === 'たがる' || t.text.includesたがる)
      ;

      console.log('\n=== ' + sentence + ' ===');
      tagaruTokens.forEach(t => {
        console.log(`  text: ${t.text}, lemma: ${t.lemma}, pos: ${t.pos}, dep: ${t.dep}, inflectionForm: ${t.inflectionForm}`);
      });
    });
  }
});
