import { describe, it } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('Analyze こそ', () => {
  const engineGetter = useSharedEngine([]);

  it('should analyze sentences', async () => {
    const engine = engineGetter.get();

    const sentences = [
      'あなたこそリーダーにふさわしい！',
      '今度こそ達成してみせる！',
      '私こそ悪いことをしてしまいました。',
      'こちらこそいつもお世話になっております。',
      '愛こそすべてだ',
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
