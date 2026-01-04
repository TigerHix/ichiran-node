import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ても-なくても', (r) => {
  // ～ても～なくても - "whether ~ or not"
  // Pattern: Verb[て-form] + も + same Verb + なくても
  // e.g., 言っても言わなくても, してもしなくても, あってもなくても
  //
  // This rule matches when the same verb appears in both positive and negative
  // conditional forms.
  //
  // Due to GiNZA parsing inconsistencies, we use multiple patterns:
  // - Standard: verb + て + も + verb + なく + て + も
  // - Special cases for ある, irregular verbs, etc.

  r.either(
    // Pattern 1: Standard case with なく
    // verb1 + て/で + も + verb2 + なく + て/で + も
    (b) => {
      const verb1 = b.verb({}, 'verb1');
      const te1 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te1');
      const mo1 = b.tok({ text: 'も', pos: 'ADP' }, 'mo1');
      b.inOrder(verb1, te1, 3);
      b.inOrder(verb1, mo1, 4);

      const verb2 = b.verb({}, 'verb2');
      const naku = b.tok({ text: 'なく', posOneOf: ['AUX', 'SCONJ', 'SYM'] }, 'naku');
      b.inOrder(verb2, naku, 3);

      const te2 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te2');
      const mo2 = b.tok({ text: 'も', pos: 'ADP' }, 'mo2');
      b.inOrder(naku, te2, 2);
      b.inOrder(te2, mo2, 1);

      b.inOrder(mo1, verb2, 10);
      b.captureSpan('ても-なくても', verb1, mo2);
    },

    // Pattern 2: Special case for ある → ない + く
    // ある + て + も + ない + く + て + も
    (b) => {
      const aru = b.tok({ text: 'ある', pos: 'VERB' }, 'aru');
      const te1 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te1');
      const mo1 = b.tok({ text: 'も', pos: 'ADP' }, 'mo1');
      b.inOrder(aru, te1, 2);
      b.inOrder(te1, mo1, 1);

      const nai = b.aux({ lemma: 'ない' }, 'nai');
      const ku = b.tok({ text: 'く' }, 'ku');
      const te2 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te2');
      const mo2 = b.tok({ text: 'も', pos: 'ADP' }, 'mo2');

      b.inOrder(nai, ku, 1);
      b.inOrder(ku, te2, 1);
      b.inOrder(te2, mo2, 1);

      b.inOrder(mo1, nai, 10);
      b.captureSpan('ても-なくても', aru, mo2);
    },

    // Pattern 3: Direct nai auxiliary (without く)
    // verb1 + て + も + verb2 + ない + て + も
    (b) => {
      const verb1 = b.verb({}, 'verb1');
      const te1 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te1');
      const mo1 = b.tok({ text: 'も', pos: 'ADP' }, 'mo1');
      b.inOrder(verb1, te1, 3);
      b.inOrder(verb1, mo1, 4);

      const verb2 = b.verb({}, 'verb2');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(verb2, nai);

      const te2 = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te2');
      const mo2 = b.tok({ text: 'も', pos: 'ADP' }, 'mo2');
      b.inOrder(nai, te2, 2);
      b.inOrder(te2, mo2, 1);

      b.inOrder(mo1, verb2, 10);
      b.captureSpan('ても-なくても', verb1, mo2);
    },
  );
});
