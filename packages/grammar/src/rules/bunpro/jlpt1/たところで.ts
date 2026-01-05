import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たところで', (r) => {
  // Pattern: Verb ta-form + ところ + で
  // e.g., 怒ったところで, 行ったところで, 続けたところで, 稼いだところで
  r.either(
    // Pattern 1: Combined tokenization (たところで as one token)
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const tokorode = b1.tok({ text: 'たところで' }, 'tokorode');
      b1.inOrder(verb, tokorode, 5);
      b1.captureSpan('たところで', verb, tokorode);
    },

    // Pattern 2: Split tokenization (た + ところで)
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const ta = b2.tok({ text: 'た' }, 'ta');
      const tokorode = b2.tok({ textOneOf: ['ところで', '所で'] }, 'tokorode');
      b2.inOrder(verb, ta, 5);
      b2.inOrder(ta, tokorode, 2);
      b2.captureSpan('たところで', verb, tokorode);
    },

    // Pattern 3: Split tokenization (たところ + で)
    (b3) => {
      const verb = b3.verb({}, 'verb');
      const ta = b3.tok({ text: 'た' }, 'ta');
      const tokoro = b3.tok({ textOneOf: ['ところ', '所'] }, 'tokoro');
      const de = b3.tok({ text: 'で' }, 'de');
      b3.inOrder(verb, ta, 5);
      b3.inOrder(ta, tokoro, 2);
      b3.inOrder(tokoro, de, 1);
      b3.captureSpan('たところで', verb, de);
    },

    // Pattern 4: All separate (た + ところ + で)
    (b4) => {
      const verb = b4.verb({}, 'verb');
      const ta = b4.tok({ text: 'た', pos: 'AUX' }, 'ta');
      const tokoro = b4.tok({ textOneOf: ['ところ', '所'], pos: 'NOUN' }, 'tokoro');
      const de = b4.tok({ text: 'で', pos: 'ADP' }, 'de');
      b4.inOrder(verb, ta, 5);
      b4.inOrder(ta, tokoro, 2);
      b4.inOrder(tokoro, de, 1);
      b4.captureSpan('たところで', verb, de);
    },

    // Pattern 5: だ (copula ta-form) + ところで (e.g., 稼いだところで)
    // Some verbs ending in だ may have the だ tokenized as AUX
    (b5) => {
      const da = b5.tok({ text: 'だ', posOneOf: ['AUX', 'VERB'] }, 'da');
      const tokoro = b5.tok({ textOneOf: ['ところ', '所'] }, 'tokoro');
      const de = b5.tok({ text: 'で' }, 'de');
      b5.inOrder(da, tokoro, 2);
      b5.inOrder(tokoro, de, 1);
      b5.captureSpan('たところで', da, de);
    },

    // Pattern 6: Any token ending in た + ところ + で
    (b6) => {
      const ta = b6.tok({ text: 'た' }, 'ta');
      const tokoro = b6.tok({ textOneOf: ['ところ', '所'] }, 'tokoro');
      const de = b6.tok({ text: 'で' }, 'de');
      b6.inOrder(ta, tokoro, 2);
      b6.inOrder(tokoro, de, 1);
      b6.captureSpan('たところで', ta, de);
    },
  );
});
