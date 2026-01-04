import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (karatoitte) - Just because, although
 *
 * A phrase used to express that just because (A) is true, it doesn't
 * necessarily mean (B) is also true. Often used for criticism or strong
 * opinions. Similar to "just because X doesn't mean Y" in English.
 *
 * Structure:
 * - Verb + からといって
 * - い-Adj + からといって
 * - な-Adj/Noun + だ + からといって (often abbreviated as だからといって)
 * - Abbreviated forms: からって (colloquial), からとて (literary)
 *
 * Examples:
 * - 安いからといって買いすぎてしまい、買った食べ物を腐らせている。
 *   (Just because it's cheap, he buys too much and ends up spoiling the food.)
 * - 暑いからって、そんなに休憩ばかりしていたら仕事が進まないだろ。
 *   (If you keep taking breaks just because it's hot, you'll never get your work done.)
 * - 日本人だからといって、漢字を書けるとは限らない。
 *   (Just because someone is Japanese doesn't mean they can write kanji.)
 * - 丈夫だからといって、雑に扱えば必ず壊れます。
 *   (Just because it's sturdy, it will definitely break if you handle it roughly.)
 *
 * Key discriminators:
 * - Follows verbs, adjectives, or nouns+だ
 * - から is a conjunctive particle (SCONJ/ADP) indicating "because"
 * - と is a quote particle (ADP/PART)
 * - いって is the te-form of 言う (VERB)
 * - Expresses concessive reasoning (A doesn't imply B)
 * - Usually followed by negative or concessive conclusion
 *
 * GiNZA parse structure:
 * - Various POS tags for から (SCONJ, ADP)
 * - と as ADP or PART
 * - いって as VERB with lemma=言う, inflectionForm=連用形-一般
 *
 * Different from:
 * - から alone as "because" or "from"
 * - からして (judging from)
 * - からすると (more objective judgment)
 * - と言って (as in "called/say")
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Noun/PROPN/PRON + だ + から + と + いって (だからといって)
    // e.g., 日本人だからといって, 生意気だからといって, 丼物だからといって
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = r1.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r1.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r1.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r1.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r1.inOrder(noun, da, 1);
      r1.inOrder(da, kara, 1);
      r1.inOrder(kara, to, 1);
      r1.inOrder(to, itte, 1);

      r1.captureSpan('からといって', da, itte);
    },

    // Pattern 2: な-Adjective + だ + から + と + いって (だだからといって)
    // e.g., 丈夫だからといって, 便利だからといって, ご機嫌斜めだからといって
    (r2) => {
      const naAdj = r2.adj({}, 'naAdj');
      const da = r2.tok({ text: 'だ', pos: 'AUX' }, 'da');
      const kara = r2.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r2.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r2.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r2.inOrder(naAdj, da, 1);
      r2.inOrder(da, kara, 1);
      r2.inOrder(kara, to, 1);
      r2.inOrder(to, itte, 1);

      r2.captureSpan('からといって', da, itte);
    },

    // Pattern 3: い-Adjective + から + と + いって
    // e.g., 安いからといって, 暑いからといって, 貧しいからといって
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const kara = r3.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r3.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r3.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r3.inOrder(iAdj, kara, 1);
      r3.inOrder(kara, to, 1);
      r3.inOrder(to, itte, 1);

      r3.captureSpan('からといって', iAdj, itte);
    },

    // Pattern 4: Verb + (auxiliaries) + た/て + から + と + いって
    // e.g., したからといって, 住んでいたからとて, 慰められたからといって
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const kara = r4.tok({ text: 'から', posOneOf: ['SCONJ', 'ADP'] }, 'kara');
      const to = r4.tok({ text: 'と', posOneOf: ['ADP', 'PART'] }, 'to');
      const itte = r4.verb({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      r4.inOrder(verb, kara, 5);
      r4.inOrder(kara, to, 1);
      r4.inOrder(to, itte, 1);

      r4.captureSpan('からといって', verb, itte);
    },

    // Pattern 5: い-Adjective + からって (colloquial abbreviation)
    // e.g., 暑いからって
    (r5) => {
      const iAdj = r5.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const karatte = r5.tok({ lemma: 'からって' }, 'karatte');

      r5.inOrder(iAdj, karatte, 1);

      r5.captureSpan('からといって', iAdj, karatte);
    },

    // Pattern 6: Verb + からとて (literary abbreviation)
    // e.g., 住んでいたからとて
    (r6) => {
      const verb = r6.verb({}, 'verb');
      const karatote = r6.tok({ lemma: 'からとて' }, 'karatote');

      r6.inOrder(verb, karatote, 5);

      r6.captureSpan('からといって', verb, karatote);
    }
  );
});
