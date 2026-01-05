import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('でございます', (r) => {
  // でございます is the very polite form of the copula です (is/am/are)
  // It's equivalent to です but in humble/polite register (敬語)
  // Structure: Noun + でございます (copula form of ござる)
  //
  // GiNZA tokenizes "ございます" as ["ござい", "ます"] so we need to match both

  r.either(
    // Pattern 1: でございます (present tense)
    // GiNZA tokenizes as: で + ござい + ます
    (b1) => {
      const de = b1.tok({ text: 'で' }, 'de');
      const gozai = b1.tok({ text: 'ござい' }, 'gozai');
      const masu = b1.tok({ text: 'ます', lemma: 'ます' }, 'masu');
      b1.inOrder(de, gozai, 3);
      b1.inOrder(gozai, masu, 1);
      // Ensure this is "ございます" not "ございました"
      b1.not((nr) => {
        const mashita = nr.tok({ textOneOf: ['ました', 'ませんでした'] }, 'mashita');
        nr.inOrder(masu, mashita, 0);
      });
      b1.captureSpan('でございます', de, masu);
    },

    // Pattern 2: でございました (past tense)
    // GiNZA tokenizes as: で + ござい + ました
    (b2) => {
      const de = b2.tok({ text: 'で' }, 'de');
      const gozai = b2.tok({ text: 'ござい' }, 'gozai');
      const mashita = b2.tok({ textOneOf: ['ました', 'ませんでした'] }, 'mashita');
      b2.inOrder(de, gozai, 3);
      b2.inOrder(gozai, mashita, 1);
      b2.captureSpan('ございました', de, mashita);
    },

    // Pattern 3: でござる (archaic/casual form)
    (b3) => {
      const de = b3.tok({ text: 'で' }, 'de');
      const gozaru = b3.tok({ text: 'ござる' }, 'gozaru');
      b3.inOrder(de, gozaru, 3);
      b3.captureSpan('でござる', de, gozaru);
    }
  );
});
