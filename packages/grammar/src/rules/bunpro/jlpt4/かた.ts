import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: かた (way of doing)
 *
 * Verb-masu-stem + かた (方) = way of doing, how to do
 * Examples:
 * - 使いかた (way of using, how to use)
 * - 食べかた (way of eating, how to eat)
 * - しかた (way of doing, for する verbs only)
 *
 * Key patterns:
 * 1. Verb stem (masu form) + かた
 * 2. Noun + の + verb-stem + かた (e.g., この本のよみかた)
 * 3. Noun + の + 仕方 (special case for suru verbs)
 *
 * GiNZA parse structure:
 * - たべかた: たべ (NOUN, tag=動詞-一般, inflectionForm=連用形-一般) + かた (NOUN, tag=接尾辞-名詞的-一般)
 * - さきかた: さき (VERB, inflectionForm=連用形-一般) + かた (NOUN, tag=接尾辞-名詞的-一般)
 * - いいかた: いい (ADJ, tag=動詞-一般, inflectionForm=連用形-一般) + かた (NOUN, tag=接尾辞-名詞的-一般)
 * - 貯金のしかた: 貯金 (NOUN) + の (ADP) + しかた (NOUN, lemma=しかた)
 *
 * Note: GiNZA VERY inconsistently parses verb stems:
 * - Sometimes as NOUN with tag=動詞-一般 (たべ)
 * - Sometimes as VERB with inflectionForm=連用形-一般 (さき)
 * - Sometimes as ADJ with tag=動詞-一般 (いい from いう)
 * - Sometimes as NOUN proper name (さき from さく - GiNZA error)
 *
 * We handle this by creating separate pattern branches for each GiNZA parsing variant.
 */
export default bunproLinguisticRule('かた', (r) => {
  r.either(
    // Pattern 1a: Verb stem (NOUN/VERB/ADJ with tag=動詞-一般) + かた
    (b) => {
      const verb = b.tok({ tag: '動詞-一般' }, 'verb');
      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', verb, kata);
    },

    // Pattern 1b: Verb stem (VERB with 連用形-一般) + かた
    (b) => {
      const verb = b.tok({ pos: 'VERB', inflectionForm: '連用形-一般' }, 'verb');
      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', verb, kata);
    },

    // Pattern 1c: Verb stem (ADJ with 連用形-一般) + かた
    (b) => {
      const verb = b.tok({ pos: 'ADJ', inflectionForm: '連用形-一般' }, 'verb');
      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', verb, kata);
    },

    // Pattern 2a: Noun + の + verb-stem (NOUN/VERB/ADJ with tag=動詞-一般) + かた
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const verb = b.tok({ tag: '動詞-一般' }, 'verb');
      b.inOrder(no, verb, 1);

      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', noun, kata);
    },

    // Pattern 2b: Noun + の + verb-stem (VERB with 連用形-一般) + かた
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const verb = b.tok({ pos: 'VERB', inflectionForm: '連用形-一般' }, 'verb');
      b.inOrder(no, verb, 1);

      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', noun, kata);
    },

    // Pattern 2c: Noun + の + verb-stem (ADJ with 連用形-一般) + かた
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const verb = b.tok({ pos: 'ADJ', inflectionForm: '連用形-一般' }, 'verb');
      b.inOrder(no, verb, 1);

      const kata = b.noun({
        lemmaOneOf: ['かた', '方'],
        tag: '接尾辞-名詞的-一般',
      }, 'kata');
      b.inOrder(verb, kata, 1);
      b.captureSpan('かた', noun, kata);
    },

    // Pattern 3: Noun + の + 仕方 (for suru-verbs)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const shikata = b.noun({
        lemma: 'しかた',
      }, 'shikata');

      b.inOrder(no, shikata, 1);
      b.captureSpan('かた', noun, shikata);
    }
  );
});
