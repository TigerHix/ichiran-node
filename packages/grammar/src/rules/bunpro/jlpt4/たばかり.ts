import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たばかり', (r) => {
  // Verb ta-form (た) + ばかり (just did / just finished)
  // Matches: 買ったばかり, 食べたばかり, 行ったばかり, etc.

  // The た (ta) auxiliary - marks past tense completion
  const ta = r.aux({ text: 'た', lemma: 'た' }, 'ta');

  // The ばかり (bakari) particle/adverbial noun - indicates "just/only"
  const bakari = r.tok({ text: 'ばかり', lemma: 'ばかり' }, 'bakari');

  // Require immediate sequence: verb-ta + ばかり
  r.inOrder(ta, bakari, 1);

  // Capture the full pattern
  r.captureSpan('たばかり', ta, bakari);
});
