import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ちゃんと-きちんと', (r) => {
  // ちゃんと・きちんと - adverbs meaning "properly, neatly, correctly"
  // These two adverbs have very similar meanings and usage patterns.
  // They modify verbs to indicate doing something properly, correctly, neatly.
  //
  // Patterns:
  // - ちゃんと/きちんと + verb: ちゃんと宿題した, きちんと書きましょう
  // - ちゃんと/きちんと + した + noun: ちゃんとした車, きちんとした服装
  // - ちゃんと/きちんと + している: ちゃんとしてる

  const adv = r.adv({
    textOneOf: ['ちゃんと', 'きちんと'],
  }, 'adv');

  r.either(
    // Pattern 1: ちゃんと/きちんと + verb
    // ちゃんと宿題した, きちんと片付けしなさい, ちゃんと謝る, ちゃんと食べた
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(adv, verb, 5);
      b.captureSpan('ちゃんと-きちんと', adv, verb);
    },

    // Pattern 2: ちゃんと/きちんと + した + noun
    // ちゃんとした車, きちんとした服装, ちゃんとした仕事
    (b) => {
      const shita = b.verb({
        lemma: 'する',
        inflectionForm: '連体形-一般',
      }, 'shita');
      const noun = b.noun({}, 'noun');
      b.inOrder(adv, shita, 2);
      b.inOrder(shita, noun, 2);
      b.captureSpan('ちゃんと-きちんと', adv, noun);
    },

    // Pattern 3: ちゃんと/きちんと + して + auxiliary
    // ちゃんとしている, きちんとしておいて
    (b) => {
      const shite = b.verb({
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'shite');
      const aux = b.aux({}, 'aux');
      b.inOrder(adv, shite, 2);
      b.auxOf(shite, aux);
      b.captureSpan('ちゃんと-きちんと', adv, aux);
    }
  );
});
