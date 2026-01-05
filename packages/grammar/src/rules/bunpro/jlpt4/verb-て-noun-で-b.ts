import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('verb-て-noun-で-b', (r) => {
  // Verb[て]・Noun[で] (B) - means/manner "by, with"
  // A casual, simple conjunctive form showing that (B) was done in the way of (A).
  // Covers both:
  // 1. Verb te-form (Verb in 連用形 + て)
  // 2. Noun + で particle (instrumental/means)
  //
  // Examples:
  //   泳いでトレーニングをしています (training BY swimming)
  //   車で仕事に行く (go to work BY car)
  //   フライパンを使って、料理をします (cooking USING a frying pan)
  //   水と石鹸で手を丁寧に洗いましょう (wash hands WITH soap and water)
  //   総理大臣は新幹線を利用して東京に戻りました (returned BY USING shinkansen)
  //   毎日ＮＨＫニュースを読んで日本語を勉強しています (study BY reading)
  //
  // Key discriminators:
  // - te-form: て must have pos=SCONJ and dep=mark (conjunctive particle)
  // - de particle: で must have pos=ADP and dep=case (case particle)
  // This prevents matching:
  // - Locative で (東京で働く) - still dep=case but context differs
  // - Conjunction では (東京では...) - different dep=dep
  // - Causative て-form (てしまう) - different auxiliary

  r.either(
    // Pattern 1: Verb te-form (連用形 + て)
    // Examples: 泳いで, 使って, 慌てて, 歩いて
    (b) => {
      const verb = b.tok({
        pos: 'VERB',
        inflectionFormOneOf: [
          '連用形-一般',      // 慌てて
          '連用形-イ音便',    // 泳いで, 歩いて
          '連用形-促音便',    // 使って, 触って
          '連用形-撥音便',    // (rare)
        ],
      }, 'verb');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      b.inOrder(verb, te, 1);
      b.captureSpan('verb-て', verb, te);
    },

    // Pattern 2: Suru-verb te-form (利用して)
    // Example: 総理大臣は新幹線を利用して東京に戻りました
    // GiNZA parses: 利用 (VERB, no inflectionForm) + し (AUX, 連用形-一般) + て (SCONJ)
    (b) => {
      const verb = b.tok({
        pos: 'VERB',
      }, 'verb');

      const shi = b.tok({
        lemma: 'する',
        pos: 'AUX',
        inflectionForm: '連用形-一般',
      }, 'shi');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      b.inOrder(verb, shi, 2);
      b.inOrder(shi, te, 1);
      b.auxOf(verb, shi);
      b.captureSpan('verb-て', verb, te);
    },

    // Pattern 3: Verb + で (te-form expressed as で)
    // Example: 毎日ＮＨＫニュースを読んで日本語を勉強しています
    // GiNZA parses: よん (VERB, 連用形-撥音便) + で (ADP, case)
    // This happens with verbs ending in む/ぶ/ぬ (mu/bu/nu sound changes)
    (b) => {
      const verb = b.tok({
        pos: 'VERB',
        inflectionFormOneOf: [
          '連用形-撥音便',    // よんで (from よむ)
        ],
      }, 'verb');

      const de = b.tok({
        text: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      b.inOrder(verb, de, 1);
      b.captureSpan('verb-で', verb, de);
    },

    // Pattern 4: Noun + で particle (instrumental/means)
    // Examples: 車で, ハサミで, 徒歩で, 石鹸で
    (b) => {
      const noun = b.noun({}, 'noun');
      const de = b.tok({
        text: 'で',
        pos: 'ADP',
        dep: 'case',
      }, 'de');

      b.caseMarker(noun, de);
      b.captureSpan('noun-で', noun, de);
    }
  );
});
