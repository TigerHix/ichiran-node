import { describe, it } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('Analyze negatives for こそ', () => {
  const engineGetter = useSharedEngine([]);

  it('should analyze negative sentences', async () => {
    const engine = engineGetter.get();

    const sentences = [
      'ここに来てください。',  // こ as 'here' (not こそ)
      '子が遊んでいる。',       // こ as part of 'ko' (child)
      '私は学生です。',         // が as subject marker
      'これを見て。',           // こ as 'this' (demonstrative)
      'こっちに来て。',         // こ as 'this direction'
    ];

    for (const sentence of sentences) {
      console.log('\n' + '='.repeat(70));
      console.log('Sentence:', sentence);
      console.log('='.repeat(70));
      const doc = await engine.analyze(sentence);
      if (!doc) {
        console.log('DOC IS NULL');
        continue;
      }
      for (const sent of doc.sentences) {
        for (const tok of sent.tokens) {
          const repr = {
            i: tok.i,
            text: tok.text,
            lemma: tok.lemma,
            pos: tok.pos,
            dep: tok.dep,
            head: tok.head,
            inflectionForm: tok.inflectionForm,
          };
          console.log(JSON.stringify(repr));
        }
      }
    }
  });
});
