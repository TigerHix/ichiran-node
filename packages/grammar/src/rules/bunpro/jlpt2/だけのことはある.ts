import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: だけのことはある (dake no koto wa aru) - "it's true that at least X", "X lives up to its reputation"
 *
 * An expression indicating that something is exactly as expected based on its
 * nature or reputation. It emphasizes that a result is natural or deserved
 * given the circumstances or qualities of something/someone.
 *
 * Structure:
 * - Verb (dictionary form or ta-form) + だけのことはある
 * - I-adjective + だけのことはある
 * - Na-adjective + (な/だった) + だけのことはある
 * - Noun + (な/だった) + だけのことはある
 *
 * Examples:
 * - 練習しただけのことはある。
 *   (It shows that [I] practiced. / The practice paid off.)
 * - やっぱり有名なだけのことはある。
 *   (As expected from how famous it is.)
 * - さすが習字の先生だけのことはある。
 *   (As expected of a calligraphy teacher.)
 * - 昔ボクサーだっただけのことはある。
 *   (As expected from someone who used to be a boxer.)
 *
 * Key discriminators:
 * - Expresses positive affirmation of expectations
 * - Cannot be used with negative results
 * - Often appears with words like さすが, やっぱり, やはり
 * - は can be written as は (hiragana) or 波 (kanji - rare)
 * - Different from:
 *   - だけに (more flexible, used mid-sentence)
 *   - わけだ (expresses speaker's understanding/conclusion)
 *
 * GiNZA parse structure:
 * - Verb/Aux + だけ(ADP) + の(ADP) + こと(NOUN) + は(ADP) + ある(VERB/AUX)
 * - Adj + だけ(ADP) + の(ADP) + こと(NOUN) + は(ADP) + ある(VERB/AUX)
 * - Noun + な/だっだ + だけ(ADP) + の(ADP) + こと(NOUN) + は(ADP) + ある(VERB/AUX)
 */
export default bunproLinguisticRule('だけのことはある', (r) => {
  r.either(
    // Pattern 1: Verb (dictionary form or ta-form) + だけのことはある
    (b1) => {
      const verb = b1.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const dake = b1.particle('だけ', 'dake');
      const no = b1.particle('の', 'no');
      const koto = b1.tok({
        text: 'こと',
        pos: 'NOUN',
      }, 'koto');
      const wa = b1.particle('は', 'wa');
      const aru = b1.tok({
        lemma: 'ある',
        posOneOf: ['VERB', 'AUX'],
      }, 'aru');

      b1.inOrder(verb, dake, 5);
      b1.inOrder(dake, no, 1);
      b1.inOrder(no, koto, 1);
      b1.inOrder(koto, wa, 1);
      b1.inOrder(wa, aru, 1);

      b1.captureSpan('だけのことはある', verb, aru);
    },

    // Pattern 2: I-adjective + だけのことはある
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const dake = b2.particle('だけ', 'dake');
      const no = b2.particle('の', 'no');
      const koto = b2.tok({
        text: 'こと',
        pos: 'NOUN',
      }, 'koto');
      const wa = b2.particle('は', 'wa');
      const aru = b2.tok({
        lemma: 'ある',
        posOneOf: ['VERB', 'AUX'],
      }, 'aru');

      b2.inOrder(adj, dake, 5);
      b2.inOrder(dake, no, 1);
      b2.inOrder(no, koto, 1);
      b2.inOrder(koto, wa, 1);
      b2.inOrder(wa, aru, 1);

      b2.captureSpan('だけのことはある', adj, aru);
    },

    // Pattern 3: Na-adjective/Noun + な + だけのことはある
    (b3) => {
      const naAdj = b3.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'],
      }, 'naAdj');
      const na = b3.particle('な', 'na');
      const dake = b3.particle('だけ', 'dake');
      const no = b3.particle('の', 'no');
      const koto = b3.tok({
        text: 'こと',
        pos: 'NOUN',
      }, 'koto');
      const wa = b3.particle('は', 'wa');
      const aru = b3.tok({
        lemma: 'ある',
        posOneOf: ['VERB', 'AUX'],
      }, 'aru');

      b3.inOrder(naAdj, na, 3);
      b3.inOrder(na, dake, 1);
      b3.inOrder(dake, no, 1);
      b3.inOrder(no, koto, 1);
      b3.inOrder(koto, wa, 1);
      b3.inOrder(wa, aru, 1);

      b3.captureSpan('だけのことはある', naAdj, aru);
    },

    // Pattern 4: Na-adjective/Noun + だっただけのことはある (past form)
    (b4) => {
      const naAdj = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'],
      }, 'naAdj');
      const datta = b4.tok({
        lemma: 'だ',
        inflectionForm: '連用形-一般',
      }, 'datta');
      const dake = b4.particle('だけ', 'dake');
      const no = b4.particle('の', 'no');
      const koto = b4.tok({
        text: 'こと',
        pos: 'NOUN',
      }, 'koto');
      const wa = b4.particle('は', 'wa');
      const aru = b4.tok({
        lemma: 'ある',
        posOneOf: ['VERB', 'AUX'],
      }, 'aru');

      b4.inOrder(naAdj, datta, 3);
      b4.inOrder(datta, dake, 3);
      b4.inOrder(dake, no, 1);
      b4.inOrder(no, koto, 1);
      b4.inOrder(koto, wa, 1);
      b4.inOrder(wa, aru, 1);

      b4.captureSpan('だけのことはある', naAdj, aru);
    }
  );
});
