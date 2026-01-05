import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('まで', (r) => {
  // JLPT4 まで - "even" or "to the extent of" (surprising degree)
  // Unlike JLPT5 noun-まで (simple "until"), this emphasizes surprising/extreme extent
  //
  // Key discriminators:
  // - までも (optional も adds emphasis) - "even to the extent that"
  // - Used with various nouns to indicate surprising inclusion
  // - Context: "X [までも] Y" = "Y happens even to the extent of X (surprisingly)"
  //
  // Examples from test data:
  // - バイク[までも]持っているの？ "You even have a bike?"
  // - 頭[までも]痛くなってきた "Even my head is starting to hurt"
  // - こっち[までも]イライラしてきた "Even I am getting annoyed"
  // - 漢字[までも]書ける "Can even write kanji"
  // - 夜中[までも]勉強している "Studying even until midnight"
  // - 夜遅く[までも]勉強する "Study even until late at night"
  //
  // POS: ADP (adposition/particle)
  // dep: case (case marker) - marks the preceding noun as an extent
  //
  // Note: This rule will also match までに (by + deadline) since they're syntactically
  // identical. The distinction is semantic/pragmatic, not syntactic.
  //
  // Special case: For phrases like "夜遅くまで" where "夜遅く" is parsed as
  // two tokens (夜 NOUN + 遅く ADV), the case marker attaches to the compound.

  r.either(
    // Pattern 1: まで alone (without emphasis particle も)
    (b) => {
      const made = b.particle('まで', 'made', { dep: 'case' });
      // Include ADV for time phrases like 夜遅く (late at night), 夜中 (midnight)
      // Also allow ADJ for adjectival nouns
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM', 'ADV', 'ADJ'] }, 'noun');
      b.caseMarker(noun, made);
      b.capture(made);
    },
    // Pattern 2: までも (with emphatic も)
    (b) => {
      const made = b.particle('まで', 'made', { dep: 'case' });
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'NUM', 'ADV', 'ADJ'] }, 'noun');
      const mo = b.particle('も', 'mo', { dep: 'case' });
      b.caseMarker(noun, made);
      b.inOrder(made, mo, 1);
      b.captureSpan('までも', made, mo);
    }
  );
});
