import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: としては (toshite wa) - "as for, in the capacity of"
 *
 * A phrase used to judge something from the standpoint of a particular role,
 * position, or category. Indicates "as (A)" or "for a (A)" when (A) defines
 * a capacity or identity, often with contrastive or evaluative nuance.
 *
 * Structure: Noun + として + は
 *
 * Examples:
 * - 先輩としてはいいけど、友達になろうとは思えない。
 *   (As a senpai, he is a good person, but I don't think I would want to be friends with him.)
 * - あの会社は建築会社としてはホワイトな方だと思う。
 *   (I think that company is white as far as construction companies go.)
 * - アイデアとしてはいいんですが、実際にやると費用がかかる。
 *   (That is good as an idea, but it would cost a lot to actually do it.)
 * - 素質としては十分だが、まだまだ荒削りだ。
 *   (As for its qualities it is adequate, but it is still very much a work in progress.)
 * - 概論としては、まだ不十分です。
 *   (For an outline, it still needs work.)
 *
 * Key discriminators:
 * - Follows nouns (NOUN, PROPN, PRON) representing roles/positions/categories
 * - と is quotational particle (ADP with lemma=と)
 * - して is te-form of する (lemma=する, inflectionForm=連用形-一般)
 * - は is topic particle (ADP with lemma=は)
 * - Often used with contrastive or evaluative predicates (ga, kedo, etc.)
 *
 * GiNZA parse structure:
 * - NOUN + と(ADP, lemma=と) + して(VERB/AUX, lemma=する) + は(ADP, lemma=は)
 * - して can be single token or split into し + て
 *
 * Different from:
 * - として (without は) - "as" (JLPT3 grammar, less emphatic)
 * - にしては (nishite) - "considering, for" (emphasizes unexpectedness)
 * - には (niwa) - "for, in regard to" (emphasizes fundamental relation)
 * - にとって (nitotte) - "to, for" (emphasizes relevance/effect)
 * - としても (toshitemo) - "even as" (conditional)
 * - Simple topic は - "as for" (no role/capacity meaning)
 */
export default linguisticRule('としては', (r) => {
  const to = r.particle('と', 'to');
  const wa = r.particle('は', 'wa');

  r.either(
    // Pattern 1: して as single token (VERB or AUX)
    // 連用形-一般 is the te-form inflection
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const shite = b1.tok({
        text: 'して',
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'shite');

      b1.inOrder(noun, to, 3);
      b1.inOrder(to, shite, 1);
      b1.inOrder(shite, wa, 1);

      b1.captureSpan('としては', noun, wa);
    },

    // Pattern 2: して split into し + て (most common)
    // GiNZA often splits the te-form
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const shi = b2.tok({
        text: 'し',
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'shi');
      const te = b2.tok({
        text: 'て',
        lemma: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b2.inOrder(noun, to, 3);
      b2.inOrder(to, shi, 1);
      b2.inOrder(shi, te, 1);
      b2.inOrder(te, wa, 1);

      b2.captureSpan('としては', noun, wa);
    },

    // Pattern 3: VERB-tagged verbal nouns (GiNZA inconsistency)
    // Some verbal nouns like 売上 are tagged as VERB by GiNZA despite functioning as nouns
    // Distinguish from actual verbs by: tag field contains "名詞" or dependency structure
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'noun');
      const shi = b3.tok({
        text: 'し',
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'shi');
      const te = b3.tok({
        text: 'て',
        lemma: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b3.inOrder(noun, to, 3);
      b3.inOrder(to, shi, 1);
      b3.inOrder(shi, te, 1);
      b3.inOrder(te, wa, 1);

      b3.captureSpan('としては', noun, wa);
    },

    // Pattern 4: して as VERB without explicit inflection form check
    // Some parsings may not have inflectionForm populated
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const shite = b4.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
      }, 'shite');

      b4.inOrder(noun, to, 3);
      b4.inOrder(to, shite, 2);
      b4.inOrder(shite, wa, 2);

      b4.captureSpan('としては', noun, wa);
    },

    // Pattern 5: VERB-tagged verbal nouns without inflection check
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'noun');
      const shi = b5.aux({
        text: 'し',
        lemma: 'する',
      }, 'shi');
      const te = b5.tok({
        text: 'て',
        lemma: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b5.inOrder(noun, to, 3);
      b5.inOrder(to, shi, 1);
      b5.inOrder(shi, te, 1);
      b5.inOrder(te, wa, 1);

      b5.captureSpan('としては', noun, wa);
    },

    // Pattern 6: Loose matching for edge cases
    // This handles cases where there might be intervening tokens
    (b6) => {
      const noun = b6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'noun');
      const shite = b6.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX'],
      }, 'shite');

      b6.inOrder(noun, to, 5);
      b6.inOrder(to, shite, 3);
      b6.inOrder(shite, wa, 3);

      b6.captureSpan('としては', noun, wa);
    }
  );
});
