import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('では-それでは-じゃあ', (r) => {
  // GiNZA tokenizes these differently:
  // - じゃあ → single token
  // - では → two tokens: で + は (both ADP)
  // - それでは → three tokens: それ (CCONJ) + で (fixed) + は (fixed)
  r.either(
    // Pattern 1: じゃあ as single token
    (b) => {
      const jaa = b.tok({ text: 'じゃあ' }, 'jaa');
      b.capture(jaa);
    },
    // Pattern 2: では as conjunction with dep=dep (both pointing to same head)
    (b) => {
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'dep' }, 'de');
      const wa = b.tok({ text: 'は', pos: 'ADP', dep: 'dep' }, 'wa');
      b.inOrder(de, wa, 1);
      b.captureSpan('では', de, wa);
    },
    // Pattern 2b: では with dep=dep + fixed (は attaches to で)
    (b) => {
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'dep' }, 'de');
      const wa = b.tok({ text: 'は', pos: 'ADP', dep: 'fixed' }, 'wa');
      b.inOrder(de, wa, 1);
      b.captureSpan('では', de, wa);
    },
    // Pattern 2c: では with dep=cc + fixed (で as coordinating conjunction)
    (b) => {
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'cc' }, 'de');
      const wa = b.tok({ text: 'は', pos: 'ADP', dep: 'fixed' }, 'wa');
      b.inOrder(de, wa, 1);
      b.captureSpan('では', de, wa);
    },
    // Pattern 3: それでは - それ is CCONJ with で/は as fixed
    (b) => {
      const sore = b.tok({ text: 'それ', pos: 'CCONJ' }, 'sore');
      const de = b.tok({ text: 'で', dep: 'fixed' }, 'de');
      const wa = b.tok({ text: 'は', dep: 'fixed' }, 'wa');
      b.inOrder(sore, de, 1);
      b.inOrder(de, wa, 1);
      b.captureSpan('それでは', sore, wa);
    }
  );
});

