import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: たところだ - Just finished doing / Was just doing
 *
 * Matches patterns where a verb in ta-form is followed by ところ(だ/です)
 * to express "just did" or "have just done" something.
 *
 * Structures:
 * - Verb［た］+ ところ + だ/です (just finished doing X)
 * - Verb［ていた］+ ところ + だ/です (was just doing X - past progressive)
 *
 * Examples:
 * - 今帰ったところです (I just got home.)
 * - 仕事は今終わったところ (I just finished work.)
 * - 食べたところでケーキを出された (Just when I had eaten ice cream, cake was brought out.)
 * - 食べたところ、同僚に誘われた (Just when I had eaten, a co-worker invited me.)
 *
 * Key discriminators:
 * - Must be verb ta-form + ところ (not other uses of ところ)
 * - GiNZA INCONSISTENTLY parses ところ as either SCONJ or NOUN depending on sentence
 * - GiNZA sometimes parses た as AUX, sometimes as SCONJ (inconsistency)
 * - Copula (だ/です) is optional in sentence-final position
 *
 * GiNZA parse structure (HIGHLY variable by sentence):
 * - 帰った場所 (noun): 帰っ(verb) + た(aux) + ところ(noun)
 * - 食べた場所、 (sconj): たべ(verb) + た(sconj) + ところ(sconj)
 * - のんだ場所で: のん(verb) + た(aux) + ところ(sconj) + で(sconj,dep=fixed)
 * - 楽しんでいた場所で: 楽しん(verb) + て(sconj) + い(verb) + た(aux) + ところ(sconj)
 */
export default linguisticRule('たところだ', (r) => {
  r.either(
    // Branch 1: Verb［た］+ ところ (standard pattern)
    // GiNZA parses た as either AUX or SCONJ depending on sentence
    // GiNZA parses ところ as either SCONJ or NOUN depending on sentence
    (b) => {
      const verb = b.verb({}, 'verb');
      const ta = b.tok({
        lemma: 'た',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'ta');

      // Handle two different parse patterns:
      // 1. ta as aux attached to verb (dep=aux)
      // 2. ta as marker attached to verb (dep=mark) - GiNZA quirk
      b.either(
        // Standard: ta is aux of verb
        (eb) => {
          eb.auxOf(verb, ta);
        },
        // GiNZA quirk: ta has dep=mark pointing to verb
        (eb) => {
          eb.headChild(verb, ta, 'mark');
        }
      );

      // Followed by ところ (GiNZA inconsistently parses as SCONJ or NOUN)
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(ta, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('たところだ', verb, tokoro);
    },
    // Branch 2: Verb［た］+ ところ where ところ has dep=mark pointing to verb
    // GiNZA quirk: sometimes ところ directly marks the verb, skipping over た
    (b) => {
      const verb = b.verb({}, 'verb');

      // Optional ta (can be た or だ)
      b.optional((ob) => {
        const ta = ob.tok({
          lemmaOneOf: ['た', 'だ'],
          pos: 'AUX',
        }, 'ta');
        ob.auxOf(verb, ta);
      });

      // ところ has dep=mark pointing directly to verb
      const tokoro = b.tok({
        lemma: 'ところ',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'tokoro');
      b.headChild(verb, tokoro, 'mark');

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('たところだ', verb, tokoro);
    },
    // Branch 3: Verb［ていた］+ ところ (past progressive: "was just doing")
    (b) => {
      const verb = b.verb({}, 'verb');

      // GiNZA sometimes parses て as SCONJ with dep=mark
      const te = b.tok({
        lemma: 'て',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'te');

      // Check if te is attached to verb (either aux or fixed relationship)
      b.either(
        // te as aux attached to verb
        (eb) => {
          eb.auxOf(verb, te);
        },
        // te as sconj with dep=mark or dep=fixed attached to verb
        (eb) => {
          eb.headChild(verb, te, 'mark');
        },
        // te as sconj with dep=fixed
        (eb) => {
          eb.headChild(te, verb, 'fixed');
        }
      );

      // Followed by いる (sometimes as い)
      const iru = b.tok({
        lemmaOneOf: ['いる', 'いるる', 'おる', 'いるた'],
        posOneOf: ['VERB', 'AUX'],
      }, 'iru');
      b.inOrder(te, iru, 2);

      // Followed by た
      const ta = b.tok({
        lemma: 'た',
        pos: 'AUX',
      }, 'ta');
      b.auxOf(iru, ta);

      // Followed by ところ
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(ta, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('たところだ', verb, tokoro);
    }
  );
});
