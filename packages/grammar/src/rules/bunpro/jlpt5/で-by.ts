import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('で-by', (r) => {
  // Instrumental で particle (by means of/with/using)
  // Indicates the means, method, or tool used to perform an action
  // Examples: バスで行く (go by bus), 鉛筆で書く (write with pencil)
  //
  // NOTE: GiNZA parses instrumental で identically to locative で (at/in).
  // Both have pos=ADP, dep=case. The distinction is semantic/contextual.
  //
  // This rule matches the same structural pattern as the "で" (location) rule,
  // which is linguistically accurate since they're the same particle used in
  // different contexts. Learners must distinguish meaning from context.
  const de = r.particle('で', 'de', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'noun');
  r.caseMarker(noun, de);
  r.capture(de);
});
