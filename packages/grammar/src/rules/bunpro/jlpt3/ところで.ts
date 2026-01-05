import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ところで', (r) => {
  // ところで (tokorode) - "by the way", "incidentally"
  // A conjunction/discourse marker used to change topics or start a new topic
  //
  // This is the sentence-initial discourse marker usage.
  // Related but DIFFERENT grammar points:
  // - たところで (even if/even though) - advanced grammar after verb past tense
  // - Verb-たところで (just did/just then) - JLPT4 grammar
  // - Noun + ところで (at that place/situation) - literal meaning
  //
  // GiNZA tokenizes as either:
  // - Single token (text=ところで)
  // - Two tokens: ところ + で
  //
  // Based on metadata: "conjunction particle, case-marking particle"
  // Similar to さて, すると, そこで which are sentence starters
  //
  // This rule matches all instances of ところで. The specific usage
  // (discourse marker vs other forms) is determined by context.

  r.either(
    // Pattern 1: Single token ところで
    (b) => {
      const tokorode = b.tok({ text: 'ところで' }, 'tokorode');
      b.capture(tokorode);
    },
    // Pattern 2: Two tokens - ところ + で
    (b) => {
      const tokoro = b.tok({ text: 'ところ' }, 'tokoro');
      const de = b.tok({ text: 'で' }, 'de');
      b.inOrder(tokoro, de, 1);
      b.captureSpan('ところで', tokoro, de);
    },
    // Pattern 3: Kanji variant 所で (single token)
    (b) => {
      const tokorode = b.tok({ text: '所で' }, 'tokorode');
      b.capture(tokorode);
    },
    // Pattern 4: Kanji variant 所 + で (two tokens)
    (b) => {
      const tokoro = b.tok({ text: '所' }, 'tokoro');
      const de = b.tok({ text: 'で' }, 'de');
      b.inOrder(tokoro, de, 1);
      b.captureSpan('所で', tokoro, de);
    }
  );
});
