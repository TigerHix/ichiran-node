import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たくさん', (r) => {
  // たくさん (takusan) / 沢山 is an adverbial noun meaning "many, a lot of, plenty"
  // It can modify verbs directly (advmod) or nouns with の (genitive case)
  // Matches both hiragana (たくさん) and kanji (沢山) forms

  r.either(
    // Branch 1: たくさん as adverb modifying verbs (dep='advmod')
    (branch) => {
      const takusan = branch.tok({
        lemmaOneOf: ['たくさん', '沢山'],
        dep: 'advmod'
      }, 'takusan');
      branch.capture(takusan);
    },
    // Branch 2: たくさん with の modifying nouns (noun/adv + genitive particle)
    // Use text constraint for の to avoid dep constraint issues
    (branch) => {
      const takusan = branch.tok({
        lemmaOneOf: ['たくさん', '沢山']
      }, 'takusan');
      const no = branch.tok({ text: 'の' }, 'no');
      branch.inOrder(takusan, no, 1);
      branch.captureSpan('たくさんの', takusan, no);
    }
  );
});
