import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: past-tense-い-adjectives - い-Adjectives (Past)
 *
 * Matches い-adjectives in past tense form (～かった).
 * Pattern: remove い from adjective, add かった
 * Examples: さむい → さむかった, たのしい → たのしかった
 *
 * Exception: いい → よかった (not いかった)
 *
 * Both casual and polite forms:
 * - Casual: adj-stem + た (e.g., さむかった, たのしかった)
 * - Polite: adj-stem + た + です (e.g., さむかったです, たのしかったです)
 *
 * GiNZA parses this pattern as:
 * - Adjective stem: pos=VERB, tag=形容詞-一般, conjugationClass=形容詞, inflectionForm=連用形-促音便
 * - Past auxiliary: pos=AUX, lemma=た, tag=助動詞, conjugationClass=助動詞-タ, dep=aux
 *
 * This rule captures the entire past tense form including optional です.
 */
export default linguisticRule('past-tense-い-adjectives', (r) => {
  r.either(
    // Casual form: い-adjective stem + た
    (b) => {
      // GiNZA parses adjective past stem inconsistently:
      // - Most: pos=VERB (e.g., さむかっ, 寒かっ)
      // - Some: pos=ADJ (e.g., 甘かっ, おいしかっ)
      // All have: tag=形容詞-一般, conjugationClass=形容詞, inflectionForm=連用形-促音便
      const adjStem = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        tag: '形容詞-一般',
        conjugationClass: '形容詞',
        inflectionForm: '連用形-促音便',
      }, 'stem');

      // Past tense auxiliary た
      const pastAux = b.aux({
        lemma: 'た',
        tag: '助動詞',
        conjugationClass: '助動詞-タ',
      }, 'past');

      // Require auxiliary to attach to adjective stem
      b.auxOf(adjStem, pastAux);

      // Capture from stem to auxiliary (includes full past form)
      b.captureSpan('match', adjStem, pastAux);
    },
    // Polite form: い-adjective stem + た + です
    (b) => {
      // GiNZA parses adjective past stem inconsistently:
      // - Most: pos=VERB (e.g., さむかっ, 寒かっ)
      // - Some: pos=ADJ (e.g., 甘かっ, おいしかっ)
      // All have: tag=形容詞-一般, conjugationClass=形容詞, inflectionForm=連用形-促音便
      const adjStem = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        tag: '形容詞-一般',
        conjugationClass: '形容詞',
        inflectionForm: '連用形-促音便',
      }, 'stem');

      // Past tense auxiliary た
      const pastAux = b.aux({
        lemma: 'た',
        tag: '助動詞',
        conjugationClass: '助動詞-タ',
      }, 'past');

      // Polite auxiliary です
      const desu = b.aux({
        lemma: 'です',
      }, 'desu');

      // Require auxiliaries to attach in order
      b.auxOf(adjStem, pastAux);
      b.inOrder(pastAux, desu, 1);

      // Capture from stem to desu (includes full polite past form)
      b.captureSpan('match', adjStem, desu);
    }
  );
});
