import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ところだった2 - "was just about to", "was in the middle of"
 *
 * Matches verb dictionary form + ところ + だった/でした to express that
 * something was about to happen but didn't actually occur, or was in the
 * middle of happening.
 *
 * Structure:
 * - Verb［る］+ ところ + だった/でした (casual/polite)
 * - Verb［ている］+ ところ + だった/でした (in the middle of)
 *
 * This JLPT2 variant emphasizes:
 * 1. "Was just about to do X" - almost happened but didn't
 * 2. "Was in the middle of doing X" - ongoing action interrupted
 *
 * Similar to JLPT3 ところだった1 but with broader usage including progressive
 * forms (ている) and more emphasis on being "in the situation/position of".
 *
 * Examples:
 * - 今から関西弁を教わるところだったのに。
 *   (I was just about to be taught the Kansai dialect.)
 * - 危うく期限が切れてしまうところだった。
 *   (I was just about to narrowly miss the deadline.)
 * - 出掛けるところだったが、雨が降ってきたのでやめました。
 *   (I was just about to go out, but because it started raining, I stopped.)
 * - お風呂に入っているところだったから、電話に出られませんでした。
 *   (Since I was in the middle of taking a bath, I couldn't answer the phone.)
 *
 * GiNZA parse structure:
 * - Verb (VERB) - dictionary form or te+iru form
 * - ところ (NOUN or SCONJ) - nominal suffix meaning "place/situation"
 * - だった/でした (AUX) - past copula indicating past tense
 *
 * For だった:
 * - だっ (AUX, lemma=だ, inf=連用形-促音便) - copula stem
 * - た (AUX, lemma=た) - past tense auxiliary
 *
 * For でした (polite form):
 * - で (SCONJ/ADP, lemma=で) - te-form of copula
 * - し (AUX, lemma=する) - polite auxiliary
 * - た (AUX, lemma=た) - past tense auxiliary
 *
 * Key discriminators:
 * - Must follow verb dictionary form (る-form) or progressive form (ている)
 * - Different from たところだ (JLPT4) - uses dictionary form, not ta-form
 * - Different from るところだ (JLPT4) - uses past copula だった, not present だ
 * - Different from ところだ (JLPT4) - about to do NOW, not WAS about to
 */
export default linguisticRule('ところだった2', (r) => {
  r.either(
    // Branch 1: Verb dictionary form + ところ + だった (casual, most common)
    // e.g., 教わるところだった, 出掛けるところだった, するところだった
    (b1) => {
      const verb = b1.verb({}, 'verb');

      // ところ - nominal suffix meaning "place/situation"
      const tokoro = b1.noun({
        lemma: 'ところ',
      }, 'tokoro');

      // ところ must follow verb
      b1.inOrder(verb, tokoro, 3);

      // だった - past copula (casual form)
      // GiNZA parses as TWO tokens: だっ + た
      const dattsu = b1.aux({
        lemma: 'だ',
        inflectionForm: '連用形-促音便',
      }, 'dattsu');
      b1.copulaOf(tokoro, dattsu);

      const ta = b1.aux({
        lemma: 'た',
      }, 'ta');
      b1.auxOf(tokoro, ta);

      b1.captureSpan('ところだった2', verb, ta);
    },

    // Branch 2: Verb dictionary form + ところ + でした (polite form)
    // e.g., 教わるところでした, するところでした
    (b2) => {
      const verb = b2.verb({}, 'verb');

      const tokoro = b2.noun({
        lemma: 'ところ',
      }, 'tokoro');

      b2.inOrder(verb, tokoro, 3);

      // でした - past copula (polite form)
      // GiNZA parses as: で + し + た
      const de = b2.tok({
        lemma: 'で',
        posOneOf: ['SCONJ', 'ADP'],
      }, 'de');
      b2.copulaOf(tokoro, de);

      const shi = b2.aux({
        lemma: 'する',
      }, 'shi');
      b2.auxOf(tokoro, shi);

      const ta = b2.aux({
        lemma: 'た',
      }, 'ta');
      b2.auxOf(tokoro, ta);

      b2.captureSpan('ところだった2', verb, ta);
    },

    // Branch 3: Verb［ている］+ ところ + だった (progressive form, casual)
    // e.g., 入っているところだった, 調整しているところだった
    (b3) => {
      const verb = b3.verb({}, 'verb');

      // て (te-form)
      const te = b3.tok({
        lemma: 'て',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'te');

      b3.auxOf(verb, te);

      // いる (progressive auxiliary)
      const iru = b3.tok({
        lemmaOneOf: ['いる', 'おる'],
        posOneOf: ['VERB', 'AUX'],
      }, 'iru');

      b3.inOrder(te, iru, 2);

      const tokoro = b3.noun({
        lemma: 'ところ',
      }, 'tokoro');

      b3.inOrder(iru, tokoro, 3);

      // だった
      const dattsu = b3.aux({
        lemma: 'だ',
        inflectionForm: '連用形-促音便',
      }, 'dattsu');
      b3.copulaOf(tokoro, dattsu);

      const ta = b3.aux({
        lemma: 'た',
      }, 'ta');
      b3.auxOf(tokoro, ta);

      b3.captureSpan('ところだった2', verb, ta);
    },

    // Branch 4: Verb［ている］+ ところ + でした (progressive form, polite)
    // e.g., 入っているところでした
    (b4) => {
      const verb = b4.verb({}, 'verb');

      const te = b4.tok({
        lemma: 'て',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'te');

      b4.auxOf(verb, te);

      const iru = b4.tok({
        lemmaOneOf: ['いる', 'おる'],
        posOneOf: ['VERB', 'AUX'],
      }, 'iru');

      b4.inOrder(te, iru, 2);

      const tokoro = b4.noun({
        lemma: 'ところ',
      }, 'tokoro');

      b4.inOrder(iru, tokoro, 3);

      // でした
      const de = b4.tok({
        lemma: 'で',
        posOneOf: ['SCONJ', 'ADP'],
      }, 'de');
      b4.copulaOf(tokoro, de);

      const shi = b4.aux({
        lemma: 'する',
      }, 'shi');
      b4.auxOf(tokoro, shi);

      const ta = b4.aux({
        lemma: 'た',
      }, 'ta');
      b4.auxOf(tokoro, ta);

      b4.captureSpan('ところだった2', verb, ta);
    },

    // Branch 5: Loose catch-all for verb + ところ + だった/でした variations
    // Handles edge cases where GiNZA parsing is inconsistent
    (b5) => {
      const verb = b5.verb({}, 'verb');

      const tokoro = b5.tok({
        lemma: 'ところ',
        posOneOf: ['NOUN', 'SCONJ'],
      }, 'tokoro');

      b5.inOrder(verb, tokoro, 5);

      // Any copula + ta combination (だった or でした)
      b5.optional((ob) => {
        const copulaPart = ob.tok({
          lemmaOneOf: ['だ', 'で'],
        }, 'copulaPart');
        ob.inOrder(tokoro, copulaPart, 2);
      });

      const ta = b5.aux({
        lemma: 'た',
      }, 'ta');
      b5.inOrder(tokoro, ta, 5);

      b5.captureSpan('ところだった2', verb, ta);
    }
  );
});
