import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('transitive-intransitive-verbs', (r) => {
  // Transitive verbs use を (direct object marker)
  // Intransitive verbs use が (subject marker)
  //
  // Common transitive/intransitive pairs:
  // - 落とす (otosu - to drop) / 落ちる (ochiru - to fall)
  // - つける (tsukeru - to attach/turn on) / つく (tsuku - to be attached/turn on)
  // - 見つける (mitsukeru - to find) / 見つかる (mitsukaru - to be found)
  // - 下げる (sageru - to lower) / 下がる (sagaru - to be lowered/go down)
  // - 消す (kesu - to extinguish/turn off) / 消える (kieru - to disappear/go out)
  // - 開ける (akeru - to open) / 開く (aku - to be open)
  // - 集める (atsumeru - to gather) / 集まる (atsumaru - to gather)
  // - 始める (hajimeru - to start) / 始まる (hajimaru - to start)

  r.either(
    // Branch 1: Transitive verb pattern (Noun + を + Verb)
    (b) => {
      const wo = b.particle('を', 'wo');

      const transitiveVerb = b.verb({
        lemmaOneOf: [
          'おとす',      // 落とす - to drop
          'つける',      // to attach/turn on
          'みつける',    // 見つける - to find
          'さげる',      // 下げる - to lower
          'けす',        // 消す - to extinguish/turn off
          'あける',      // 開ける - to open
          'あつめる',    // 集める - to gather
          'はじめる',    // 始める - to start
          'する',        // to do (transitive partner of なる)
        ],
      }, 'transitiveVerb');

      const obj = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'obj');
      b.caseMarker(obj, wo);
      b.objectOf(transitiveVerb, obj);
      b.inOrder(obj, wo, 1);
      b.inOrder(wo, transitiveVerb, 3);
      b.capture(transitiveVerb);
    },

    // Branch 2: Intransitive verb pattern (Noun + が + Verb)
    (b) => {
      const ga = b.particle('が', 'ga');

      const intransitiveVerb = b.verb({
        lemmaOneOf: [
          'おちる',      // 落ちる - to fall
          'つく',        // to be attached/turn on
          'みつかる',    // 見つかる - to be found
          'さがる',      // 下がる - to be lowered/go down
          'きえる',      // 消える - to disappear/go out
          'あく',        // 開く - to be open
          'あつまる',    // 集まる - to gather
          'はじまる',    // 始まる - to start
          'なる',        // to become (intransitive partner of する)
        ],
      }, 'intransitiveVerb');

      const subj = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'subj');
      b.caseMarker(subj, ga);
      b.headChild(intransitiveVerb, subj, 'nsubj');
      b.inOrder(subj, ga, 1);
      b.inOrder(ga, intransitiveVerb, 3);
      b.capture(intransitiveVerb);
    }
  );
});
