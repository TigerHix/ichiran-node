import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あの', (r) => {
  // あの is a demonstrative adjective/pre-noun (連体詞) meaning "that (over there)"
  // It must be followed by a noun (DET + NOUN with dep=det)
  // Refers to something far from both speaker and listener, or mutually known
  //
  // Examples from Bunpro:
  // - あの人はトムです。
  // - あの犬が可愛いです。
  // - あのラーメン屋は美味しいです。

  r.either(
    // Branch 1: kana form あの
    (branch) => {
      const ano = branch.tok({ lemma: 'あの', pos: 'DET' }, 'ano');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, ano, 'det');
      branch.capture(ano);
    },
    // Branch 2: kanji form 彼の
    (branch) => {
      const ano = branch.tok({ textOneOf: ['彼の', 'あの'], pos: 'DET' }, 'ano');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, ano, 'det');
      branch.capture(ano);
    }
  );
});
