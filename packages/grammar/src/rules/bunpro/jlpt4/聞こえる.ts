import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('聞こえる', (r) => {
  // 聞こえる is an intransitive verb meaning "to be audible" or "can hear"
  // It's a spontaneous/potential form verb - sound naturally reaches the ears
  // Uses が to mark what is being heard
  //
  // GiNZA parses:
  // - Dictionary form (きこえる) as ADJ, lemma=きこえる
  // - Conjugated forms (きこえない, etc.) as VERB, lemma=きこえる

  r.either(
    // Pattern 1: Dictionary form (GiNZA tags as ADJ)
    (b) => {
      const kikoeru = b.tok({ pos: 'ADJ', lemma: 'きこえる' }, 'kikoeru');
      b.capture(kikoeru);
    },
    // Pattern 2: Conjugated forms (GiNZA tags as VERB)
    (b) => {
      const kikoeru = b.verb({ lemma: 'きこえる' }, 'kikoeru');
      b.capture(kikoeru);
    }
  );
});
