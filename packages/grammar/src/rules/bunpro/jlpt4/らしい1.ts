import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: らしい1 (rashii1) - Hearsay / "I heard that..."
 *
 * Expresses hearsay or information received from a third party.
 * Indicates "seems like" or "apparently" based on what the speaker heard or read.
 *
 * Structures:
 * - Verb (plain form) + らしい
 * - ［い］Adjective + らしい
 * - ［な］Adjective + らしい
 * - Noun + らしい
 *
 * Examples:
 * - 大統領がキューバに行くらしい (I heard the president is going to Cuba)
 * - 明日から雪が降るらしい (Apparently it will snow from tomorrow)
 * - 彼の彼氏はカッコいいらしい (I heard her boyfriend is handsome)
 * - あの人はフランス人らしい (I heard that person is French)
 * - O型血液は誰にでも使えるらしい (Apparently type O blood can be used by anyone)
 *
 * Key discriminators:
 * - This is the HEARSAY usage, distinct from らしい2 (characteristic/typical of)
 * - らしい1 attaches to verbs, adjectives, and nouns with hearsay meaning
 * - らしい2 ONLY attaches to nouns with "typical of" meaning
 * - Both parse grammatically the same way - distinction is purely semantic
 *
 * Negative tests should include:
 * - らしい2 examples: Noun + らしい where meaning is "typical of" not "heard"
 *   e.g., 男らしい (manly), 春らしい (spring-like), 子供らしい (childlike)
 * - These are characteristic meanings, not hearsay
 *
 * GiNZA parse structure:
 * - 行くらしい: 行く(VERB) + らしい(AUX/ADJ, lemma=らしい)
 * - 降るらしい: 降る(VERB) + らしい(AUX/ADJ, lemma=らしい)
 * - フランス人らしい: フランス人(NOUN) + らしい(AUX/ADJ, lemma=らしい)
 */
export default bunproLinguisticRule('らしい1', (r) => {
  r.either(
    // Branch 1: Verb (any form) + らしい
    // Example: 行くらしい, 降るらしい, できるらしい, 続けるらしい, 働いていたらしい
    (b) => {
      const verb = b.verb({}, 'verb');
      const rashii = b.tok({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(verb, rashii);
      b.captureSpan('らしい1', verb, rashii);
    },

    // Branch 2: I-adjective + らしい
    // Example: カッコいいらしい, 安いらしい
    (b) => {
      const adj = b.adj({}, 'adj');
      const rashii = b.tok({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(adj, rashii, 1);
      b.captureSpan('らしい1', adj, rashii);
    },

    // Branch 3: Na-adjective + らしい
    // Example: 本当らしい, 便利らしい
    // Na-adjectives can be ADJ or ADV depending on context
    (b) => {
      const adj = b.tok({
        tag: '形状詞-一般',
      }, 'adj');
      const rashii = b.tok({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(adj, rashii, 1);
      b.captureSpan('らしい1', adj, rashii);
    },

    // Branch 4: Noun + じゃない + らしい
    // Example: 姉のタイプじゃないらしい (Apparently not my sister's type)
    // じゃない is the casual negative form of the copula だ
    (b) => {
      const noun = b.noun({}, 'noun');
      const ja = b.particle('じゃ', 'ja');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      const rashii = b.tok({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(noun, ja);
      b.inOrder(ja, nai, 1);
      b.inOrder(nai, rashii, 1);
      b.captureSpan('らしい1', noun, rashii);
    },

    // Branch 5: Noun + らしい
    // Example: フランス人らしい, 社長らしい, 会社らしい
    // Note: This also matches らしい2 patterns, but that's expected -
    // the distinction between らしい1 (hearsay) and らしい2 (characteristic)
    // is purely semantic, not syntactic. Both are grammatically NOUN + らしい.
    (b) => {
      const noun = b.noun({}, 'noun');
      const rashii = b.tok({
        lemma: 'らしい',
      }, 'rashii');
      b.inOrder(noun, rashii, 1);
      b.captureSpan('らしい1', noun, rashii);
    }
  );
});
