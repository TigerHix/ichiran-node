import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ございます', (r) => {
  // ございます is the very polite form of ある (to be/exist for inanimate things)
  // GiNZA parses it with lemma=ござる, pos=VERB
  //
  // Special cases:
  // - おはようございます: GiNZA parses as single INTJ token with lemma="おはようございます"
  // - ありがとうございます: GiNZA parses as ありがとう (INTJ) + ございます (VERB)

  r.either(
    // Pattern 1: Standard ございます (very polite form of ある)
    // Examples: 時間がございます, 出口は左にございます
    (b) => {
      const gozaimasu = b.verb({ lemma: 'ござる' }, 'gozaimasu');
      b.capture(gozaimasu);
    },

    // Pattern 2: Fixed expression おはようございます (good morning)
    // GiNZA parses this as a single INTJ token
    (b) => {
      const ohayou = b.tok({ lemma: 'おはようございます' }, 'gozaimasu');
      b.capture(ohayou);
    },

    // Pattern 3: ありがとうございます (thank you very much)
    // GiNZA parses as ありがとう (INTJ) + ございます (VERB)
    (b) => {
      const arigatou = b.tok({ lemma: 'ありがとう' }, 'arigatou');
      const gozaimasu = b.verb({ lemma: 'ござる' }, 'gozaimasu');
      b.inOrder(arigatou, gozaimasu, 1);
      b.captureSpan('ありがとうございます', arigatou, gozaimasu);
    }
  );
});
