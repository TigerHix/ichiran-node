import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずにはおかない', (r) => {
  // Pattern: Verb (ない-stem) + ずには + おかない (or ないでは + おかない)
  // Strong determination or inevitability: "will definitely X", "bound to X"
  //
  // Examples:
  // - 感動させずにはおかない (won't fail to move [people])
  // - 言わせずにはおかない (will definitely make [him] say)
  // - 与えずにはおかない (will not fail to give)
  // - しないではおかない (will definitely do)
  //
  // The pattern has two forms:
  // 1. Classical form: Verb-ずにはおかない (zu ni wa okanai)
  // 2. Modern form: Verb-ないではおかない (nai dewa okanai)
  //
  // GiNZA parses "ず" as a separate token (pos=ADV or AUX) attached to the verb stem
  // GiNZA parses "ないでは" as separate tokens (nai AUX + dewa ADP)
  //
  // Key constraint: "おかない" is the negative of "置く" (to place/set aside)
  // Also accept polite form "おきません" (okimasen)

  r.either(
    // Pattern 1: Verb + ず + には + おかない (classical form, combined particles)
    (b) => {
      const zu = b.tok({ text: 'ず' }, 'zu');
      const niwa = b.tok({ text: 'には' }, 'niwa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません']
      }, 'okanai');
      b.inOrder(zu, niwa, 1).inOrder(niwa, okanai, 2);
      b.captureSpan('ずにはおかない', zu, okanai);
    },

    // Pattern 1b: Verb + ず + に + は + おかない (classical form, separate particles)
    (b) => {
      const zu = b.tok({ text: 'ず' }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません']
      }, 'okanai');
      b.inOrder(zu, ni, 2).inOrder(ni, wa, 1).inOrder(wa, okanai, 3);
      b.captureSpan('ずにはおかない', zu, okanai);
    },

    // Pattern 2: Verb + ない + では + おかない (modern form, combined particles)
    (b) => {
      const nai = b.tok({ text: 'ない' }, 'nai');
      const dewa = b.tok({ text: 'では' }, 'dewa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません']
      }, 'okanai');
      b.inOrder(nai, dewa, 1).inOrder(dewa, okanai, 2);
      b.captureSpan('ずにはおかない', nai, okanai);
    },

    // Pattern 2b: Verb + ない + で + は + おかない (modern form, separate tokens)
    (b) => {
      const nai = b.tok({ text: 'ない' }, 'nai');
      const de = b.tok({ text: 'で' }, 'de');
      const wa = b.particle('は', 'wa');
      const okanai = b.tok({
        textOneOf: ['おかない', '置かない', 'おきません', '置きません']
      }, 'okanai');
      b.inOrder(nai, de, 1).inOrder(de, wa, 1).inOrder(wa, okanai, 3);
      b.captureSpan('ずにはおかない', nai, okanai);
    }
  );
});
