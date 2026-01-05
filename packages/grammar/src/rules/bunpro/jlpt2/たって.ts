import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たって', (r) => {
  r.either(
    // Pattern 1: Verb + たって (e.g., 聞いたって, 謝ったって, 行ったって, したって, 勝ったって)
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const tatte = b1.tok({ text: 'たって' }, 'tatte');
      b1.inOrder(verb, tatte, 5);
      b1.captureSpan('たって', verb, tatte);
    },

    // Pattern 1b: Verb + た + って (split tokenization)
    (b1b) => {
      const verb = b1b.verb({}, 'verb');
      const ta = b1b.tok({ text: 'た' }, 'ta');
      const tte = b1b.tok({ text: 'って' }, 'tte');
      b1b.inOrder(verb, ta, 5);
      b1b.inOrder(ta, tte, 1);
      b1b.captureSpan('たって', verb, tte);
    },

    // Pattern 2: I-adjective + たって (e.g., よくたって, 欲しくたって)
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const tatte = b2.tok({ text: 'たって' }, 'tatte');
      b2.inOrder(adj, tatte, 5);
      b2.captureSpan('たって', adj, tatte);
    },

    // Pattern 2b: Adverb + たって (e.g., よくたって)
    (b2b) => {
      const adv = b2b.adv({}, 'adv');
      const tatte = b2b.tok({ text: 'たって' }, 'tatte');
      b2b.inOrder(adv, tatte, 5);
      b2b.captureSpan('たって', adv, tatte);
    },

    // Pattern 2b-specific: よく + たって (also check split form)
    (b2bspec) => {
      const yoku = b2bspec.tok({ text: 'よく' }, 'yoku');
      const tatte = b2bspec.tok({ textOneOf: ['たって', 'た', 'って'] }, 'tatte');
      b2bspec.inOrder(yoku, tatte, 5);
      b2bspec.captureSpan('たって', yoku, tatte);
    },

    // Pattern 2b-combined: よくたって as single token
    (b2bcomb) => {
      const yukutatte = b2bcomb.tok({ text: 'よくたって' }, 'yukutatte');
      b2bcomb.captureSpan('たって', yukutatte, yukutatte);
    },

    // Pattern 2c: I-adjective + た + って (split)
    (b2c) => {
      const adj = b2c.adj({}, 'adj');
      const ta = b2c.tok({ text: 'た' }, 'ta');
      const tte = b2c.tok({ text: 'って' }, 'tte');
      b2c.inOrder(adj, ta, 5);
      b2c.inOrder(ta, tte, 1);
      b2c.captureSpan('たって', adj, tte);
    },

    // Pattern 2d: Adverb + た + って (split)
    (b2d) => {
      const adv = b2d.adv({}, 'adv');
      const ta = b2d.tok({ text: 'た' }, 'ta');
      const tte = b2d.tok({ text: 'って' }, 'tte');
      b2d.inOrder(adv, ta, 5);
      b2d.inOrder(ta, tte, 1);
      b2d.captureSpan('たって', adv, tte);
    },

    // Pattern 3: なく + たって (e.g., できなくたって)
    (b3) => {
      const naku = b3.tok({ text: 'なく', posOneOf: ['AUX', 'PART'] }, 'naku');
      const tatte = b3.tok({ text: 'たって' }, 'tatte');
      b3.inOrder(naku, tatte, 3);
      b3.captureSpan('たって', naku, tatte);
    },

    // Pattern 4: Noun + だって (e.g., 馬鹿だって, 友達だって)
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const datte = b4.tok({ text: 'だって' }, 'datte');
      b4.inOrder(noun, datte, 3);
      b4.captureSpan('たって', noun, datte);
    },

    // Pattern 4b: Noun + だ + って (split)
    (b4b) => {
      const noun = b4b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const da = b4b.tok({ text: 'だ', posOneOf: ['AUX'] }, 'da');
      const tte = b4b.tok({ text: 'って' }, 'tte');
      b4b.inOrder(noun, da, 3);
      b4b.inOrder(da, tte, 1);
      b4b.captureSpan('たって', noun, tte);
    },

    // Pattern 5: じゃなく + たって (e.g., ピザじゃなくたって)
    (b5) => {
      const janaku = b5.tok({ textOneOf: ['じゃなく', 'ではなく'] }, 'janaku');
      const tatte = b5.tok({ text: 'たって' }, 'tatte');
      b5.inOrder(janaku, tatte, 3);
      b5.captureSpan('たって', janaku, tatte);
    },
  );
});
