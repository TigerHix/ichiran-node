import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ので', (r) => {
  r.either(
    // Pattern 1: Noun + な + ので (e.g., 日曜日なので, 先生が休みなので)
    // Note: な has dep=cop when following nouns
    (r1) => {
      const noun = r1.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const na = r1.tok({ text: 'な', pos: 'AUX', depOneOf: ['cop', 'aux'] }, 'na');
      const no = r1.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de = r1.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de');

      r1.inOrder(noun, na, 1);
      r1.inOrder(na, no, 1);
      r1.inOrder(no, de, 1);
      r1.headChild(noun, na);
      r1.headChild(noun, no);
      r1.headChild(no, de);

      r1.captureSpan('なので', noun, de);
    },
    // Pattern 2: な-Adjective + な + ので (e.g., 綺麗なので, 嫌いなので)
    // Note: 形容詞 that act as na-adj have tag starting with 名詞- or 形状詞-
    (r2) => {
      const naAdj = r2.tok({ pos: 'ADJ' }, 'naAdj');
      const na = r2.tok({ text: 'な', pos: 'AUX', depOneOf: ['cop', 'aux'] }, 'na');
      const no = r2.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de = r2.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de');

      r2.inOrder(naAdj, na, 1);
      r2.inOrder(na, no, 1);
      r2.inOrder(no, de, 1);
      r2.auxOf(naAdj, na);
      r2.headChild(naAdj, no);
      r2.headChild(no, de);

      r2.captureSpan('なので', naAdj, de);
    },
    // Pattern 2b: な-Adjective + では + ない + ので (e.g., 好きではないので)
    // Negative form of na-adjectives
    (r2b) => {
      const naAdj = r2b.tok({ pos: 'ADJ' }, 'naAdj');
      const de = r2b.tok({ text: 'で', pos: 'AUX', lemma: 'だ', dep: 'aux' }, 'de');
      const wa = r2b.tok({ text: 'は', pos: 'ADP', dep: 'fixed' }, 'wa');
      const nai = r2b.tok({ text: 'ない', pos: 'AUX', dep: 'fixed' }, 'nai');
      const no = r2b.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de2 = r2b.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de2');

      r2b.inOrder(naAdj, de, 1);
      r2b.inOrder(de, wa, 1);
      r2b.inOrder(wa, nai, 1);
      r2b.inOrder(nai, no, 1);
      r2b.inOrder(no, de2, 1);
      r2b.auxOf(naAdj, de);
      r2b.headChild(naAdj, no);
      r2b.headChild(no, de2);

      r2b.captureSpan('ではないので', naAdj, de2);
    },
    // Pattern 3: い-Adjective + ので (e.g., 寒いので, 強いので, 汚いので)
    // I-adjectives have conjugationClass='形容詞' and tag='形容詞-一般'
    (r3) => {
      const iAdj = r3.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const no = r3.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de = r3.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de');

      r3.inOrder(iAdj, no, 1);
      r3.inOrder(no, de, 1);
      r3.headChild(iAdj, no);
      r3.headChild(no, de);

      r3.captureSpan('ので', iAdj, de);
    },
    // Pattern 4: Verb + (た)? + ので (e.g., あるので, 来るので)
    // Auxiliary た can appear between verb and ので (optional)
    (r4) => {
      const verb = r4.verb({}, 'verb');
      const aux = r4.tok({ lemma: 'た', pos: 'AUX' }, 'aux');
      const no = r4.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de = r4.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de');

      r4.inOrder(verb, aux, 2);
      r4.inOrder(aux, no, 1);
      r4.inOrder(no, de, 1);
      r4.auxOf(verb, aux);
      r4.headChild(verb, no);
      r4.headChild(no, de);

      r4.captureSpan('ので', verb, de);
    },
    // Pattern 5: Verb + ので (without aux, e.g., 来るので, 行くので)
    (r5) => {
      const verb = r5.verb({}, 'verb');
      const no = r5.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const de = r5.tok({ text: 'で', pos: 'AUX', dep: 'fixed' }, 'de');

      r5.inOrder(verb, no, 1);
      r5.inOrder(no, de, 1);
      r5.headChild(verb, no);
      r5.headChild(no, de);

      r5.captureSpan('ので', verb, de);
    }
  );
});
