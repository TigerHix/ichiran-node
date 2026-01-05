import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('みたいに-みたいな', (r) => {
  // みたいに・みたいな (like/similar to - casual)
  // Casual version of ように/ような
  // Pattern: Noun + みたいに/みたいな = "like X"
  // - みたいに (mitai ni) - adverbial form, modifies verbs/adj
  // - みたいな (mitai na) - attributive form, modifies nouns
  //
  // みたい is a na-adjective, so it conjugates like:
  // - だ (copula) -> に (adverbial/連用形-ニ) for verbs
  // - だ (copula) -> な (attributive/連体形-一般) for nouns
  //
  // Example sentences:
  // - サンダルみたいに履きやすい (easy to put on like sandals)
  // - お金持ちの人みたいな生活 (life like a rich person's)
  // - それは城みたいな家です (that's a castle-like house)

  r.either(
    // Pattern 1: みたいに (adverbial - modifies verbs/adjectives)
    (b) => {
      const noun = b.tok({}, 'noun');
      const mitai = b.aux({ lemma: 'みたい', dep: 'aux' }, 'mitai');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');

      // mitai attaches to noun as aux
      b.auxOf(noun, mitai);
      // ni attaches to noun as copula (not aux!)
      b.copulaOf(noun, ni);

      // Order: noun -> mitai -> ni (contiguous)
      b.inOrder(noun, mitai, 1);
      b.inOrder(mitai, ni, 1);

      b.captureSpan('みたいに-みたいな', noun, ni);
    },
    // Pattern 2: みたいな (attributive - modifies nouns)
    (b) => {
      const noun = b.tok({}, 'noun');
      const mitai = b.aux({ lemma: 'みたい', dep: 'aux' }, 'mitai');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');

      // mitai attaches to noun as aux
      b.auxOf(noun, mitai);
      // na attaches to noun as copula (not aux!)
      b.copulaOf(noun, na);

      // Order: noun -> mitai -> na (contiguous)
      b.inOrder(noun, mitai, 1);
      b.inOrder(mitai, na, 1);

      b.captureSpan('みたいに-みたいな', noun, na);
    }
  );
});
