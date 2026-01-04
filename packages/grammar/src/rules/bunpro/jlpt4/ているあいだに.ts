import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ているあいだに - While doing / During the time that ~
 *
 * Matches patterns where Verb[ている] + 間に expresses that something
 * happens during the timeframe of an ongoing action or state.
 *
 * Structures:
 * - Verb［ている］+ 間に (while doing X)
 * - Verb［ていて］+ 間に (colloquial variant)
 * - Verb［で］+ いる + 間に (for verbs taking で-form)
 *
 * Examples:
 * - サッカーをしている間に電話がかかってきました
 * - 映画を見ている間に、お父さんが帰って来た
 * - 妻が寝ている間に、私は好きなことができる
 * - 荷物を運んでいる間に転んで怪我をした
 *
 * Key discriminators:
 * - 間 (aida) is a noun meaning "interval/period"
 * - に (ni) is a case marker particle (dep=case)
 * - いる (iru) is an auxiliary verb attached to the te-form
 *
 * GiNZA parse structure:
 * - している間に: する(verb) + て(sconj/aux) + いる(aux/verb) + 間(noun) + に(particle,dep=case)
 * - 見ている間に: 見る(verb) + て(sconj) + いる(aux/verb) + 間(noun) + に(particle,dep=case)
 * - 運んでいる間に: 運ぶ(verb) + で(sconj/aux) + いる(aux/verb) + 間(noun) + に(particle,dep=case)
 */
export default linguisticRule('ているあいだに', (r) => {
  r.either(
    // Branch 1: Verb[て] + いる + 間 + に (standard pattern)
    (b) => {
      // Any verb in te-form (various inflection types)
      const verb = b.verb({}, 'verb');

      // て (te) form - can be て or で depending on verb conjugation
      // GiNZA parses te-form as either SCONJ or AUX depending on sentence
      const te = b.tok({
        textOneOf: ['て', 'で'],
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      // Check attachment between verb and te
      b.either(
        // te as aux attached to verb
        (eb) => {
          eb.auxOf(verb, te);
        },
        // te as sconj with dep=mark attached to verb
        (eb) => {
          eb.headChild(verb, te, 'mark');
        }
      );

      // Followed by いる (iru) - progressive aspect auxiliary
      // Can be written as いる or 居る
      const iru = b.tok({
        lemmaOneOf: ['いる', 'いるる', 'おる'],
        posOneOf: ['AUX', 'VERB'],
      }, 'iru');
      b.inOrder(te, iru, 3);

      // Check that iru attaches to the verb phrase (te-form)
      // GiNZA: iru has head pointing to verb or te
      b.either(
        // iru as aux of te
        (eb) => {
          eb.auxOf(te, iru);
        },
        // iru as aux of the main verb
        (eb) => {
          eb.auxOf(verb, iru);
        }
      );

      // Followed by 間 (aida) - noun meaning "interval/period"
      // Can be written as 間 or 間
      const aida = b.noun({
        lemmaOneOf: ['あいだ', '間'],
      }, 'aida');
      b.inOrder(iru, aida, 5);

      // Followed by case marker に (ni)
      const ni = b.particle('に', 'ni', { dep: 'case' });
      b.inOrder(aida, ni, 1);

      // Capture from verb through に
      b.captureSpan('ているあいだに', verb, ni);
    },
    // Branch 2: Verb[て] + いてる + 間 + に (colloquial contracted form)
    // Some sentences use てる instead of ている
    (b) => {
      const verb = b.verb({}, 'verb');

      // て (te) form
      const te = b.tok({
        textOneOf: ['て', 'で'],
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b.either(
        (eb) => {
          eb.auxOf(verb, te);
        },
        (eb) => {
          eb.headChild(verb, te, 'mark');
        }
      );

      // Followed by contracted form てる or でる
      // GiNZA may parse this as a single token or split it
      const teru = b.tok({
        textOneOf: ['てる', 'でる'],
        posOneOf: ['AUX', 'VERB'],
      }, 'teru');
      b.inOrder(te, teru, 3);

      // Followed by 間
      const aida = b.noun({
        lemmaOneOf: ['あいだ', '間'],
      }, 'aida');
      b.inOrder(teru, aida, 5);

      // Followed by に
      const ni = b.particle('に', 'ni', { dep: 'case' });
      b.inOrder(aida, ni, 1);

      b.captureSpan('ているあいだに', verb, ni);
    }
  );
});
