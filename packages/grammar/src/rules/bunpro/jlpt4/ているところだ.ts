import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ているところだ - In the middle of doing (right now)
 *
 * Matches patterns where a verb in te-form + いる is followed by ところ(だ/です)
 * to express "in the middle of doing" or "in the process of doing" something right now.
 *
 * Structures:
 * - Verb［ている］+ ところ + だ/です (in the middle of doing X)
 * - Verb［ていた］+ ところ + だ/です (was in the middle of doing X - past)
 *
 * Examples:
 * - 今、映画を見ているところです (I'm in the middle of watching a movie right now.)
 * - 仕事をしているところだから後で電話するね (I'm in the middle of work right now, so I'll call later.)
 * - 食べているところだった (I was in the middle of eating.)
 *
 * Key discriminators:
 * - Must be verb te-form + いる + ところ (not other uses of ところ)
 * - Different from たところだ (just finished) - this uses て-form + いる
 * - Different from ところだ (about to do) - this uses dictionary form + ところ
 * - GiNZA parses ている in various ways (see いる auxiliary patterns)
 * - GiNZA parses ところ as either SCONJ or NOUN depending on sentence
 * - Copula (だ/です) is optional in sentence-final position
 *
 * GiNZA parse structure (variable):
 * - 見ている場所: 見(verb) + て(sconj) + いる(verb) + ところ(noun)
 * - してる場所: し(verb) + てる(aux) + ところ(noun)
 * - のんでいる場所で: のん(verb) + て(sconj) + いる(verb) + ところ(sconj) + で(sconj)
 */
export default bunproLinguisticRule('ているところだ', (r) => {
  r.either(
    // Branch 1: Verb［ている］+ ところ (standard pattern)
    // e.g., 見ているところだ, 食べているところです
    (b) => {
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');

      // Te-form marker (GiNZA parses as SCONJ with dep=mark)
      const te = b.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, te, 1);

      // いる auxiliary verb
      const iru = b.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');
      b.inOrder(te, iru, 1);

      // Followed by ところ
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(iru, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('ているところだ', verb, tokoro);
    },

    // Branch 2: Verb［ていた］+ ところ (past progressive: "was in the middle of")
    // e.g., していたところだ, 見ていたところです
    (b) => {
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');

      // Te-form marker
      const te = b.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, te, 1);

      // いる in ren'youkei (connecting form) + た (past)
      const iru = b.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionForm: '連用形-一般',
      }, 'iru');
      b.inOrder(te, iru, 1);

      // た (past tense auxiliary)
      const ta = b.aux({
        lemma: 'た',
        dep: 'aux',
      }, 'ta');
      b.auxOf(iru, ta);
      b.inOrder(iru, ta, 1);

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

      b.captureSpan('ているところだ', verb, tokoro);
    },

    // Branch 3: Suru-verbs with ている + ところ
    // e.g., 勉強しているところだ, 結婚しているところです
    // GiNZA parses suru-verbs differently
    (b) => {
      const verb = b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');

      // Te-form marker
      const te = b.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');
      b.inOrder(verb, te, 1);

      // いる auxiliary verb
      const iru = b.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');
      b.inOrder(te, iru, 1);

      // Followed by ところ
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(iru, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('ているところだ', verb, tokoro);
    },

    // Branch 4: Casual contraction てる + ところ
    // e.g., 見てるところだ, 食べてるところです
    // GiNZA parses てる as a single AUX token
    (b) => {
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');

      // Casual contraction てる (or でる)
      const teru = b.aux({
        lemmaOneOf: ['てる', 'でる'],
        dep: 'aux',
      }, 'teru');
      b.auxOf(verb, teru);

      // Followed by ところ
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(teru, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('ているところだ', verb, tokoro);
    },

    // Branch 5: Casual contraction てた + ところ (past)
    // e.g., 見てたところだ, 食べてたところです
    // GiNZA parses てた as AUX + AUX pattern
    (b) => {
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');

      // Casual contraction てる (or でる) in past form
      const teru = b.aux({
        lemmaOneOf: ['てる', 'でる'],
        dep: 'aux',
      }, 'teru');
      b.auxOf(verb, teru);

      // Past tense marker
      const ta = b.aux({
        lemma: 'た',
        dep: 'aux',
      }, 'ta');
      b.inOrder(teru, ta, 1);

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

      b.captureSpan('ているところだ', verb, tokoro);
    },

    // Branch 6: Benefactive/causative auxiliary + ている + ところ
    // e.g., 直してもらっているところだ, せしてもらっているところです
    // Structure: Verb[te] + auxiliary verb (もらう/くれる/やる) + いる + ところ
    (b) => {
      const verb = b.verb({}, 'verb');

      // Te-form marker
      const te = b.tok({
        textOneOf: ['て', 'で'],
      }, 'te');
      b.inOrder(verb, te, 2);

      // Benefactive auxiliary (もらう/くれる/やる/あげる etc.) - in te-form (ren'youkei)
      const morau = b.tok({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-促音便',
          '連用形-イ音便',
          '連用形-撥音便',
        ],
      }, 'morau');
      b.inOrder(te, morau, 3);

      // Second te-form marker (connects morau to iru)
      const te2 = b.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
      }, 'te2');
      b.inOrder(morau, te2, 1);

      // いる auxiliary verb
      const iru = b.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');
      b.inOrder(te2, iru, 1);

      // Followed by ところ
      const tokoro = b.tok({
        lemma: 'ところ',
        posOneOf: ['SCONJ', 'NOUN'],
      }, 'tokoro');
      b.inOrder(iru, tokoro, 3);

      // Followed by optional copula (だ/です)
      b.optional((ob) => {
        const copula = ob.tok({
          lemmaOneOf: ['だ', 'です'],
          posOneOf: ['AUX', 'VERB'],
        }, 'copula');
        ob.inOrder(tokoro, copula, 2);
      });

      b.captureSpan('ているところだ', verb, tokoro);
    }
  );
});
