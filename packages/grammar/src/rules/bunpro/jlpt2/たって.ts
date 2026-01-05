import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たって', (r) => {
  r.either(
    // Pattern 1: Verb + たって (e.g., 聞いたって, 謝ったって, 行ったって, したって, 勝ったって)
    // "たって" is a casual form of "てform + も" meaning "even if"
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const tatte = b1.tok({ text: 'たって' }, 'tatte');
      b1.inOrder(verb, tatte, 5);
      b1.captureSpan('たって', verb, tatte);
    },

    // Pattern 2: I-adjective (ku-form or negative) + たって (e.g., よくたって, 欲しくたって, 楽しくなくたって)
    // "楽しくなくたって" = 楽しく + なく + たって
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const tatte = b2.tok({ text: 'たって' }, 'tatte');
      b2.inOrder(adj, tatte, 5);
      b2.captureSpan('たって', adj, tatte);
    },

    // Pattern 3: なく + たって (negative auxiliary, e.g., できなくたって, 楽しくなくたって)
    (b3) => {
      const naku = b3.tok({ text: 'なく', posOneOf: ['AUX', 'PART'] }, 'naku');
      const tatte = b3.tok({ text: 'たって' }, 'tatte');
      b3.inOrder(naku, tatte, 3);
      b3.captureSpan('たって', naku, tatte);
    },

    // Pattern 4: Noun/Na-adjective + だって (e.g., 馬鹿だって, 友達だって)
    // Note: "だって" can also mean "because" or "even", but in this grammar it means "even if"
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const datte = b4.tok({ text: 'だって' }, 'datte');
      b4.inOrder(noun, datte, 3);
      b4.captureSpan('たって', noun, datte);
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
