import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('この', (r) => {
  // この is a demonstrative adjective/pre-noun (連体詞) meaning "this"
  // It must be followed by a noun (DET + NOUN with dep=det)
  //
  // Examples from Bunpro:
  // - このケーキは大きいです。
  // - この本がいいです。
  // - 私の車はこの車です。

  r.either(
    // Branch 1: kana form この
    (branch) => {
      const kono = branch.tok({ lemma: 'この', pos: 'DET' }, 'kono');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, kono, 'det');
      branch.capture(kono);
    },
    // Branch 2: kanji form 此の
    (branch) => {
      const kono = branch.tok({ textOneOf: ['此の', 'この'], pos: 'DET' }, 'kono');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, kono, 'det');
      branch.capture(kono);
    }
  );
});
