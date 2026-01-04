import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('Verb[passive]', (r) => {
  // Passive voice: verb + passive auxiliary (れる/られる/われる/etc.)
  //
  // The passive auxiliary れる/られる has multiple uses:
  // - Passive (ukemi): 受け身 - action done to subject
  // - Potential (kanou): 可能 - possibility
  // - Spontaneous (jihatsu): 自発 - happens spontaneously
  // - Respectful (sonkei): 尊敬 - respectful form
  //
  // This rule matches the grammatical form (verb + passive auxiliary).
  // Causative-passive forms will also match since they contain the passive auxiliary.
  // A separate rule handles causative-passive specifically.

  // Passive auxiliaries (all forms that can attach to verbs)
  const passiveAuxLemmas = [
    'れる',    // Base passive auxiliary
    'られる',  // Ichidan passive / kuru passive
  ];

  r.either(
    // Branch 1: Casual present (れる/られる/われる/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'passive');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.inOrder(verb, passive, 5);
      b.captureSpan('Verb[passive]', verb, passive);
    },

    // Branch 2: Casual past (れた/られた/われた/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionForm: '連用形-一般',
      }, 'passive');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.auxOf(verb, ta);
      b.inOrder(verb, passive, 5);
      b.inOrder(passive, ta, 1);
      b.captureSpan('Verb[passive]', verb, ta);
    },

    // Branch 3: Te-form (れて/られて/われて/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionForm: '連用形-一般',
      }, 'passive');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, passive, 5);
      b.inOrder(passive, te, 1);
      b.captureSpan('Verb[passive]', verb, te);
    },

    // Branch 4: Polite present (れます/られます/われます/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionForm: '連用形-一般',
      }, 'passive');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.auxOf(verb, masu);
      b.inOrder(verb, passive, 5);
      b.inOrder(passive, masu, 1);
      b.captureSpan('Verb[passive]', verb, masu);
    },

    // Branch 5: Polite past (れました/られました/われました/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionForm: '連用形-一般',
      }, 'passive');
      const mashita = b.tok({ lemma: 'ました' }, 'mashita');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.auxOf(verb, mashita);
      b.inOrder(verb, passive, 5);
      b.inOrder(passive, mashita, 1);
      b.captureSpan('Verb[passive]', verb, mashita);
    },

    // Branch 6: Negative (れない/られない/われない/etc.)
    (b) => {
      const passive = b.aux({
        lemmaOneOf: passiveAuxLemmas,
        inflectionForm: '未然形-一般',
      }, 'passive');
      const nai = b.aux({ lemma: 'ない' }, 'nai');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, passive);
      b.auxOf(verb, nai);
      b.inOrder(verb, passive, 5);
      b.inOrder(passive, nai, 1);
      b.captureSpan('Verb[passive]', verb, nai);
    }
  );
});
