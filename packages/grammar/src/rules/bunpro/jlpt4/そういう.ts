import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: そういう (that kind of / such)
 *
 * そう/こう/ああ/どう + いう + noun = this/that/what kind of noun
 *
 * This is a pre-nominal adjectival expression (連体詞的) from the こそあど words.
 * It functions as a determiner modifying a following noun.
 *
 * Examples from Bunpro:
 * - そういう人は嫌い。 (I don't like people like that.)
 * - そういう人もいるよ。 (There are also people like that.)
 * - そういうところに行きたい。 (I want to go to a place like that.)
 * - そういう授業は難しいですね。 (That kind of class is difficult.)
 * - そういうプレゼントをあげます。 (I will give a present like that.)
 * - そういう服をお探しですか？ (Are you looking for clothes like that?)
 *
 * GiNZA parse structure:
 * - Multi-token (そういう, こういう, ああいう):
 *   - そういう人: そう (ADV, lemma=そう) + いう (VERB, lemma=いう, dep=acl, inflectionForm=連体形-一般) + 人 (NOUN)
 *   - The いう verb modifies the noun with dep=acl (adnominal clause)
 * - Single-token (どういう only):
 *   - どういう意味: どういう (DET, lemma=どういう, dep=acl) + 意味 (NOUN)
 *   - GiNZA tokenizes どういう as a single DET
 *
 * We match all variants (そういう, こういう, ああいう, どういう) since they
 * share the same grammatical structure and are taught together in Bunpro.
 */
export default linguisticRule('そういう', (r) => {
  r.either(
    // Pattern 1: Multi-token (そう/こう/ああ + いう + noun)
    // GiNZA parses as: ADV/INTJ + VERB(inflectionForm=連体形-一般, dep=acl) + NOUN
    // Note: ああ is parsed as INTJ, not ADV (GiNZA inconsistency)
    (b) => {
      const so = b.tok({
        lemmaOneOf: ['そう', 'こう', 'ああ'],
        posOneOf: ['ADV', 'INTJ'],
      }, 'so');

      const iu = b.verb({
        lemma: 'いう',
        inflectionForm: '連体形-一般',
      }, 'iu');

      b.inOrder(so, iu, 1);

      // Must be followed by a noun that いう modifies
      const noun = b.noun({}, 'noun');
      b.headChild(noun, iu, 'acl');
      b.inOrder(iu, noun, 1);

      // Capture the full expression (そういう)
      b.captureSpan('そういう', so, iu);
    },

    // Pattern 2: Single-token (どういう as DET)
    // GiNZA parses as: DET(dep=acl) + NOUN
    (b) => {
      const douiu = b.tok({
        lemma: 'どういう',
        pos: 'DET',
        dep: 'acl',
      }, 'douiu');

      // Must be followed by a noun
      const noun = b.noun({}, 'noun');
      b.headChild(noun, douiu, 'acl');
      b.inOrder(douiu, noun, 1);

      b.capture(douiu);
    }
  );
});
