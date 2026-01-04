import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('その', (r) => {
  // その is a demonstrative adjective/pre-noun (連体詞) meaning "that"
  // It must be followed by a noun (DET + NOUN with dep=det)
  // Refers to something close to the listener (not the speaker)
  //
  // Examples from Bunpro:
  // - その本がいいです。
  // - その映画を見る。
  // - その店は安いですか。

  r.either(
    // Branch 1: kana form その
    (branch) => {
      const sono = branch.tok({ lemma: 'その', pos: 'DET' }, 'sono');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, sono, 'det');
      branch.capture(sono);
    },
    // Branch 2: kanji form 其の
    (branch) => {
      const sono = branch.tok({ textOneOf: ['其の', 'その'], pos: 'DET' }, 'sono');
      const noun = branch.noun({}, 'noun');
      branch.headChild(noun, sono, 'det');
      branch.capture(sono);
    }
  );
});
