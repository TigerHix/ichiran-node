import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('つまり', (r) => {
  // つまり (in other words / that is to say / to sum up / in short)
  // Conjunctive adverb used to summarize or rephrase what was said before
  // GiNZA parses it as: text="つまり" lemma="つまり" pos="ADV" dep="advmod"
  // It can also appear as 詰まり (kanji form)

  const tsumari = r.tok({
    lemmaOneOf: ['つまり', '詰まり'],
    pos: 'ADV',
  }, 'tsumari');

  r.capture(tsumari);
});
