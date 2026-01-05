import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ない-はない', (r) => {
  // Double negative pattern: "There is no (Noun) that doesn't/ isn't (X)"
  // Meaning: "All (Nouns) are (X)" - emphatic positive through double negative
  //
  // Patterns:
  // 1. Verb［ない］+ Noun + は + ない
  //    - 書けない漢字はない (There is no kanji that can't be written)
  //    - 置いていない部屋はない (There is no room that doesn't have a TV)
  // 2. ［い］Adjective［ない］+ Noun + は + ない/ない/いない
  //    - 美味しくないコーヒーはない (There is no coffee that isn't delicious)
  //    - 安くない靴は買わない (I don't buy shoes that aren't cheap)
  // 3. ［な］Adjective + じゃない/ではない + Noun + は + ない/いない
  //    - 好きじゃない人はいない (There is no person who doesn't like it)
  //    - 好きじゃないクラスメイトはいない (There are no classmates I don't like)

  r.either(
    // Pattern 1: Verb［ない］+ Noun + は + ない/いない/ありません
    // - 书けない漢字はない (There's no kanji I can't write)
    // - 置いていない部屋はない (There's no room without a TV)
    // - 笑わない人は一人もいなかった (There wasn't a single person who didn't laugh)
    (b) => {
      const negativeVerb = b.tok({
        // Match negative verb or adjective stem (書けない, 美味しくない, いない)
        // Also match verbs in 未然形-一般 that can have negative auxiliary
        posOneOf: ['VERB', 'AUX', 'ADJ'],
        lemmaOneOf: ['ない', 'いる'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般', '未然形-一般', '連用形-一般'],
      }, 'negativeVerb');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalNai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemmaOneOf: ['ない', 'いる', 'ある'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般', '連用形-一般'],
      }, 'finalNai');

      b.inOrder(negativeVerb, noun, 3); // Noun follows negative verb/adj
      b.inOrder(noun, wa, 1); // Topic marker follows noun
      b.inOrder(wa, finalNai, 5); // Final negative within 5 tokens
      b.captureSpan('ない-はない', negativeVerb, finalNai);
    },

    // Pattern 1b: Verb［ない］+ Noun + は + ありません (polite)
    // - 紙を使わない会社は一つもありません (There's not a single company that doesn't use paper)
    (b) => {
      const negativeVerb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'negativeVerb');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const aru = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'aru');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');

      b.inOrder(negativeVerb, noun, 3);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, aru, 3);
      b.inOrder(aru, mase, 1);
      b.captureSpan('ない-はない', negativeVerb, mase);
    },

    // Pattern 1c: Verb［ていない］+ Noun + は + ない/いない
    // - 勉強していない文法はなかった (There was no grammar I hadn't studied)
    // - 日本語を勉強していない人はいません (There's no one not studying Japanese)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'verb');
      const te = b.tok({
        pos: 'SCONJ',
        lemma: 'て',
      }, 'te');
      const iru = b.verb({
        lemma: 'いる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'iru');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalNai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemmaOneOf: ['ない', 'いる', 'ある'],
      }, 'finalNai');

      b.inOrder(verb, te, 1);
      b.inOrder(te, iru, 1);
      b.inOrder(iru, nai, 1);
      b.inOrder(nai, noun, 5);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalNai, 3);
      b.captureSpan('ない-はない', verb, finalNai);
    },

    // Pattern 1d: Simple negative verb + noun + は + past negative (いなかった)
    // - 笑わない人は一人もいなかった (There wasn't a single person who didn't laugh)
    (b) => {
      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalInai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemma: 'いる',
        inflectionFormOneOf: ['未然形-一般', '連用形-促音便'],
      }, 'finalInai');
      const finalNai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
      }, 'finalNai');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');

      b.inOrder(nai, noun, 3);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalInai, 5); // Increase distance to allow for adverbs like 一人も
      b.inOrder(finalInai, finalNai, 1);
      b.inOrder(finalNai, ta, 1);
      b.captureSpan('ない-はない', verb, ta);
    },

    // Pattern 1d2: Simple negative verb + noun + は + present negative (いない)
    // - 夢を見ない人はいないだろう (There's probably no one who doesn't dream)
    // - 好きじゃないクラスメイトはいない (There are no classmates I don't like)
    (b) => {
      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalInai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemma: 'いる',
        inflectionForm: '未然形-一般',
      }, 'finalInai');
      const finalNai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'finalNai');
      b.auxOf(finalInai, finalNai);

      b.inOrder(nai, noun, 3);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalInai, 5); // Increase distance to allow for adverbs
      b.captureSpan('ない-はない', verb, finalNai);
    },

    // Pattern 1e: Verb［なかった］+ Noun + は + なかった
    // - 勉強していない文法はなかった (There was no grammar I hadn't studied)
    (b) => {
      const negativeVerb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'negativeVerb');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const nakat = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        inflectionForm: '連用形-促音便',
      }, 'nakat');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');

      b.inOrder(negativeVerb, noun, 3);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, nakat, 2);
      b.inOrder(nakat, ta, 1);
      b.captureSpan('ない-はない', negativeVerb, ta);
    },

    // Pattern 2: ［い］Adjective［ない］+ Noun + は + ない/いない
    // - 美味しくないコーヒーは絶対ない (There's absolutely no coffee that isn't delicious)
    // - 安くない靴は買わない (I don't buy shoes that aren't cheap)
    (b) => {
      const adjStem = b.tok({
        posOneOf: ['VERB', 'NOUN', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adjStem');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'nai');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalNai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemmaOneOf: ['ない', 'いる', 'ある'],
      }, 'finalNai');

      b.inOrder(adjStem, nai, 1);
      b.inOrder(nai, noun, 3);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalNai, 5);
      b.captureSpan('ない-はない', adjStem, finalNai);
    },

    // Pattern 3: ［な］Adjective + じゃない/ではない + Noun + は + ない/いない
    // - 好きじゃない人はいない (There's no person who doesn't like it)
    // - 好きじゃないクラスメイトはいない (There are no classmates I don't like)
    (b) => {
      const nounOrNa = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        // Remove strict tag requirements to handle more variations
      }, 'nounOrNa');
      const ja = b.aux({
        lemma: 'だ',
        textOneOf: ['じゃ', 'じゃっ'],
        inflectionForm: '連用形-融合',
      }, 'ja');
      const nai = b.tok({
        posOneOf: ['AUX', 'ADJ'],
        lemma: 'ない',
        conjugationClass: '形容詞',
        dep: 'fixed',
      }, 'nai');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalInai = b.tok({
        posOneOf: ['AUX', 'VERB', 'ADJ'],
        lemma: 'いる',
        inflectionForm: '未然形-一般',
      }, 'finalInai');
      const finalNai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'finalNai');
      b.auxOf(finalInai, finalNai);

      b.inOrder(nounOrNa, ja, 2);
      b.inOrder(ja, nai, 1);
      b.inOrder(nai, noun, 5);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalInai, 5);
      b.captureSpan('ない-はない', nounOrNa, finalNai);
    },

    // Pattern 3b: ［な］Adjective + ではない + Noun + は + ない/いない
    // - 好きではない人はいない (There's no person who doesn't like it)
    (b) => {
      const nounOrNa = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
        tagOneOf: ['名詞-普通名詞-一般', '形状詞-一般'],
      }, 'nounOrNa');
      const de = b.tok({
        text: 'で',
      }, 'de');
      const wa2 = b.particle('は', 'wa2');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '形容詞',
      }, 'nai');
      const noun = b.noun({}, 'noun');
      const wa = b.particle('は', 'wa');
      const finalNai = b.tok({
        posOneOf: ['AUX', 'VERB'],
        lemmaOneOf: ['ない', 'いる'],
      }, 'finalNai');

      b.inOrder(de, wa2, 1);
      b.inOrder(wa2, nai, 1);
      b.inOrder(nounOrNa, de, 3);
      b.inOrder(nai, noun, 5);
      b.inOrder(noun, wa, 1);
      b.inOrder(wa, finalNai, 3);
      b.captureSpan('ない-はない', nounOrNa, finalNai);
    }
  );
});
