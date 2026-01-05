import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なさる', (r) => {
  // なさる is an honorific verb that replaces する (to do)
  // GiNZA parses it as lemma=なさる, but POS varies:
  // - Standalone: pos=VERB
  // - Attached to verb-te form: pos=AUX
  // - After nouns/suru-verbs: pos=VERB or AUX
  //
  // IMPORTANT: Exclude imperative form (命令形) which is the なさい grammar point
  // - Honorific: なさい (inflectionForm=連用形-イ音便) ✓ should match
  // - Imperative: なさい (inflectionForm=命令形) ✗ should NOT match

  r.either(
    // Pattern 1: 連体形-一般 (plain form: なさる)
    (b) => {
      const nasaru = b.tok({
        lemma: 'なさる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連体形-一般',
      }, 'nasaru');
      b.capture(nasaru);
    },

    // Pattern 2: 未然形-一般 (negative stem: なさら)
    (b) => {
      const nasaru = b.tok({
        lemma: 'なさる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '未然形-一般',
      }, 'nasaru');
      b.capture(nasaru);
    },

    // Pattern 3: 連用形-イ音便 (polite stem: なさい)
    (b) => {
      const nasaru = b.tok({
        lemma: 'なさる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-イ音便',
      }, 'nasaru');
      b.capture(nasaru);
    },

    // Pattern 4: 連用形-促音便 (te/past stem: なさっ)
    (b) => {
      const nasaru = b.tok({
        lemma: 'なさる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-促音便',
      }, 'nasaru');
      b.capture(nasaru);
    },

    // Pattern 5: 意志推量形 (volitional: なさろう)
    (b) => {
      const nasaru = b.tok({
        lemma: 'なさる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '意志推量形',
      }, 'nasaru');
      b.capture(nasaru);
    },

    // Pattern 6: なさる in compound patterns (verb-te form + nasaru)
    // Example: 勉強なさっている, 出席なさいます
    // GiNZA makes the base verb the root, with なさる as aux child
    (b) => {
      const baseVerb = b.verb({}, 'baseVerb');
      const te = b.tok({ text: 'て' }, 'te');
      const nasaru = b.tok({
        lemma: 'なさる',
        inflectionForm: '連用形-促音便',
      }, 'nasaru');

      // Te-verb structure with なさる attached
      b.auxOf(baseVerb, te);

      // Check if nasaru comes after te
      b.inOrder(te, nasaru, 3);

      // Capture from base verb through なさる
      b.captureSpan('て-なさる', baseVerb, nasaru);
    }
  );
});
