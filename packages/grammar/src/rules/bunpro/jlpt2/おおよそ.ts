import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おおよそ', (r) => {
  // おおよそ/およそ - adverb meaning "approximately, roughly, about"
  // Can also be written in kanji as 凡そ or 大凡
  // A slightly formal adverb often used in announcements to indicate
  // something that is not exactly accurate, but expected to be close to it.
  //
  // Patterns:
  // - おおよそ/およそ + number/quantity: おおよそ百人, およそ３０分
  // - おおよその/およその + noun: おおよその見当, およその人数
  // - おおよそ/およそ + verb: おおよそ理解した

  // Note: GiNZA tags おおよそ/およそ as:
  // - ADV when used as adverb: およそ一万冊, おおよそ理解した
  // - ADJ (形状詞-一般) when followed by の: およその人数
  // - NOUN when used as a noun meaning "outline/gist": 計画のおおよそ

  const oyoyoso = r.tok({
    textOneOf: ['おおよそ', 'およそ', '凡そ', '大凡'],
    posOneOf: ['ADV', 'ADJ', 'NOUN'],
  }, 'oyoyoso');

  r.either(
    // Pattern 0: noun + の + おおよそ (おおよそ as noun meaning "outline/gist")
    // まずは計画のおおよそを説明しよう。
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      const wo = b.particle('を', 'wo');
      b.inOrder(noun, no, 1);
      b.inOrder(no, oyoyoso, 1);
      b.inOrder(oyoyoso, wo, 1);
      b.captureSpan('おおよそ', noun, oyoyoso);
    },
    // Pattern 1: おおよそ/およそ + number (NUM + counter/noun)
    // およそ一万冊, おおよそ３０分, おおよそ１０キロ, およそ１２０人
    (b) => {
      const num = b.tok({
        pos: 'NUM',
      }, 'num');
      const counter = b.tok({
        posOneOf: ['NOUN', 'SYM', 'PROPN'],
      }, 'counter');
      b.inOrder(oyoyoso, num, 3);
      b.inOrder(num, counter, 2);
      b.captureSpan('おおよそ', oyoyoso, counter);
    },

    // Pattern 2: おおよその/およその + noun (attributive use)
    // おおよその見当, およその人数, およその見積もり
    (b) => {
      const no = b.particle('の', 'no');
      const noun = b.noun({}, 'noun');
      b.inOrder(oyoyoso, no, 1);
      b.inOrder(no, noun, 2);
      b.captureSpan('おおよそ', oyoyoso, noun);
    },

    // Pattern 3: おおよそ/およそ + noun (direct modification, like adverbial nouns)
    // 美術館では、凡そ高価な作品を展示している。
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(oyoyoso, noun, 3);
      b.captureSpan('おおよそ', oyoyoso, noun);
    },

    // Pattern 4: おおよそ/およそ + verb
    // おおよそ理解した, おおよそ見当がつく
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(oyoyoso, verb, 5);
      b.captureSpan('おおよそ', oyoyoso, verb);
    }
  );
});
