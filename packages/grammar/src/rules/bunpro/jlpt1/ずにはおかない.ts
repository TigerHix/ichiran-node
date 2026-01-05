import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ずにはおかない', (r) => {
  // Pattern: Verb (ない-stem) + ずには + おかない (or ないでは + おかない)
  // Strong determination or inevitability: "will definitely X", "bound to X"
  //
  // おかない can be parsed as:
  // 1. Single token: おかない
  // 2. Two tokens: おか + ない (verb stem + auxiliary)

  r.either(
    // Pattern 1: Verb + ず + には + おかない (combined おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const zu = b.tok({ text: 'ず' }, 'zu');
      const niwa = b.tok({ textOneOf: ['には', 'にわ'] }, 'niwa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません', 'おかん']
      }, 'okanai');
      b.inOrder(verb, zu, 8).inOrder(zu, niwa, 2).inOrder(niwa, okanai, 3);
      b.captureSpan('ずにはおかない', verb, okanai);
    },

    // Pattern 1b: Verb + ず + に + は + おかない (separate particles, combined おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const zu = b.tok({ text: 'ず' }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません', 'おかん']
      }, 'okanai');
      b.inOrder(verb, zu, 8).inOrder(zu, ni, 2).inOrder(ni, wa, 1).inOrder(wa, okanai, 3);
      b.captureSpan('ずにはおかない', verb, okanai);
    },

    // Pattern 1c: Verb + ず + には + おか + ない (split おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const zu = b.tok({ text: 'ず' }, 'zu');
      const niwa = b.tok({ textOneOf: ['には', 'にわ'] }, 'niwa');
      const oka = b.tok({ textOneOf: ['おか', '置か'] }, 'oka');
      const nai = b.tok({ textOneOf: ['ない', 'ません'] }, 'nai');
      b.inOrder(verb, zu, 8).inOrder(zu, niwa, 2).inOrder(niwa, oka, 2).inOrder(oka, nai, 1);
      b.captureSpan('ずにはおかない', verb, nai);
    },

    // Pattern 1d: Verb + ず + に + は + おか + ない (separate particles, split おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const zu = b.tok({ text: 'ず' }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const oka = b.tok({ textOneOf: ['おか', '置か'] }, 'oka');
      const nai = b.tok({ textOneOf: ['ない', 'ません'] }, 'nai');
      b.inOrder(verb, zu, 8).inOrder(zu, ni, 2).inOrder(ni, wa, 1).inOrder(wa, oka, 2).inOrder(oka, nai, 1);
      b.captureSpan('ずにはおかない', verb, nai);
    },

    // Pattern 2: Verb + ない + では + おかない (combined おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const nai = b.tok({ text: 'ない' }, 'nai');
      const dewa = b.tok({ textOneOf: ['では', 'でわ'] }, 'dewa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません', 'おかん']
      }, 'okanai');
      b.inOrder(verb, nai, 5).inOrder(nai, dewa, 2).inOrder(dewa, okanai, 3);
      b.captureSpan('ずにはおかない', verb, okanai);
    },

    // Pattern 2b: Verb + ない + で + は + おかない (separate particles, combined おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const nai = b.tok({ text: 'ない' }, 'nai');
      const de = b.tok({ text: 'で' }, 'de');
      const wa = b.particle('は', 'wa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません', 'おかん']
      }, 'okanai');
      b.inOrder(verb, nai, 5).inOrder(nai, de, 1).inOrder(de, wa, 1).inOrder(wa, okanai, 3);
      b.captureSpan('ずにはおかない', verb, okanai);
    },

    // Pattern 2c: Verb + ない + では + おか + ない (split おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const nai = b.tok({ text: 'ない' }, 'nai');
      const dewa = b.tok({ textOneOf: ['では', 'でわ'] }, 'dewa');
      const oka = b.tok({ textOneOf: ['おか', '置か'] }, 'oka');
      const nai2 = b.tok({ textOneOf: ['ない', 'ません'] }, 'nai2');
      b.inOrder(verb, nai, 5).inOrder(nai, dewa, 2).inOrder(dewa, oka, 2).inOrder(oka, nai2, 1);
      b.captureSpan('ずにはおかない', verb, nai2);
    },

    // Pattern 2d: Verb + ない + で + は + おか + ない (separate particles, split おかない)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX', 'NOUN'] }, 'verb');
      const nai = b.tok({ text: 'ない' }, 'nai');
      const de = b.tok({ text: 'で' }, 'de');
      const wa = b.particle('は', 'wa');
      const oka = b.tok({ textOneOf: ['おか', '置か'] }, 'oka');
      const nai2 = b.tok({ textOneOf: ['ない', 'ません'] }, 'nai2');
      b.inOrder(verb, nai, 5).inOrder(nai, de, 1).inOrder(de, wa, 1).inOrder(wa, oka, 2).inOrder(oka, nai2, 1);
      b.captureSpan('ずにはおかない', verb, nai2);
    }
  );
});
