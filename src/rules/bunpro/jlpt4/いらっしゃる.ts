import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いらっしゃる', (r) => {
  // いらっしゃる is an honorific verb that replaces いる/くる/いく
  // GiNZA parses it as lemma=いらっしゃる, but POS varies:
  // - Plain forms: pos=VERB
  // - Negative + です: pos=NOUN (GiNZA quirk)

  r.either(
    // Pattern 1: いらっしゃる as VERB (standard)
    (b) => {
      const irassharu = b.verb({ lemma: 'いらっしゃる' }, 'irassharu');
      b.capture(irassharu);
    },

    // Pattern 2: いらっしゃる as NOUN in negative+desu (GiNZA quirk)
    // Example: いらっしゃらないですか parses いらっしゃら as NOUN, ない as ADJ
    (b) => {
      const irassharu = b.tok({ lemma: 'いらっしゃる', pos: 'NOUN' }, 'irassharu');
      b.capture(irassharu);
    },

    // Pattern 3: いらっしゃる attached to verb-te form (verbていらっしゃる)
    // In this case, GiNZA makes いらっしゃる the root, with the te-verb as advcl child
    (b) => {
      const irassharu = b.verb({ lemma: 'いらっしゃる' }, 'irassharu');
      const teVerb = b.verb({}, 'teVerb');
      const te = b.tok({ text: 'て' }, 'te');
      b.headChild(irassharu, teVerb, 'advcl');
      b.auxOf(teVerb, te);
      b.captureSpan('ていらっしゃる', teVerb, irassharu);
    }
  );
});
