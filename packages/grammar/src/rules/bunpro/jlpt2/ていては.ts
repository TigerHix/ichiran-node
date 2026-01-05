import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ていては', (r) => {
  // Pattern: Verb-te form + いては (from いる + は)
  // Example: していては, 食べていては, 遊んでいては, 太っていては, 見られていては
  //
  // GiNZA parses this as:
  //   Regular verbs: verb + て + い + て + は
  //   Geminate verbs: verb + で + い + て + は
  // where both te/de tokens have head=verb and dep=mark
  // The first て/で is the te-form, い is from いる (fixed to first て/で)
  // The second て + は form the conditional ては
  //
  // We distinguish the two て tokens by position:
  //   te1 comes before い
  //   te2 comes after い but before は

  r.either(
    // Pattern 1: Regular verbs with て (していては, 食べていては)
    (r1) => {
      const verb = r1.verb({}, 'verb');
      const te1 = r1.tok({ text: 'て', dep: 'mark' }, 'te1');
      const i = r1.tok({ text: 'い', dep: 'fixed' }, 'i');
      const te2 = r1.tok({ text: 'て', dep: 'mark' }, 'te2');
      const wa = r1.particle('は', 'wa');

      // Order constraints: verb + te1 + i + te2 + wa
      r1.inOrder(verb, te1, 5);
      r1.inOrder(te1, i, 1);
      r1.inOrder(i, te2, 1);
      r1.inOrder(te2, wa, 1);

      // Dependency constraints: te1 and i should be connected
      r1.headChild(te1, i, 'fixed');

      // All of te1, te2, wa should have the verb as head
      r1.headChild(verb, te1, 'mark');
      r1.headChild(verb, te2, 'mark');
      r1.headChild(verb, wa, 'case');

      // Capture the full span from verb to は
      r1.captureSpan('ていては', verb, wa);
    },

    // Pattern 2: Geminate verbs with で (遊んでいては)
    (r2) => {
      const verb = r2.verb({}, 'verb');
      const de1 = r2.tok({ text: 'で', dep: 'mark' }, 'de1');
      const i = r2.tok({ text: 'い', dep: 'fixed' }, 'i');
      const te2 = r2.tok({ text: 'て', dep: 'mark' }, 'te2');
      const wa = r2.particle('は', 'wa');

      // Order constraints: verb + de1 + i + te2 + wa
      r2.inOrder(verb, de1, 5);
      r2.inOrder(de1, i, 1);
      r2.inOrder(i, te2, 1);
      r2.inOrder(te2, wa, 1);

      // Dependency constraints: de1 and i should be connected
      r2.headChild(de1, i, 'fixed');

      // All of de1, te2, wa should have the verb as head
      r2.headChild(verb, de1, 'mark');
      r2.headChild(verb, te2, 'mark');
      r2.headChild(verb, wa, 'case');

      // Capture the full span from verb to は
      r2.captureSpan('ていては', verb, wa);
    },
  );
});
