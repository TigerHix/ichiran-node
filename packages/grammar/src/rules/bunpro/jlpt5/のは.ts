import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('のは', (r) => {
  // Verb + の(は/が/も) - nominalizer followed by particle
  // Matches: verb/verb-phrase + の + (は/が/も)
  // The grammar point "の" (slug: のは) covers the nominalizer の followed by various particles
  //
  // Examples with は (topic marker):
  //   食べるのは彼です - The one who eats is him
  //   勉強するのは私です - The one who studies is me
  //   登っているのは今田さんだ - The one who is climbing is Imada-san
  //
  // Examples with が (subject marker):
  //   走るのが好き - I like running
  //   乗るのが嫌い - I hate riding
  //
  // Examples with も (also marker):
  //   読むのもいい - Reading is also good

  const verb = r.verb({}, 'verb');

  const no = r.tok({
    text: 'の',
    tag: '助詞-準体助詞',
    pos: 'SCONJ',
    dep: 'mark',
  }, 'no');

  r.either(
    // Branch 1: Verb + の + は (topic marker)
    (b) => {
      const wa = b.particle('は', 'particle');
      b.inOrder(verb, no, 2);
      b.inOrder(no, wa, 1);
      b.captureSpan('のは', verb, wa);
    },
    // Branch 2: Verb + の + が (subject marker)
    (b) => {
      const ga = b.particle('が', 'particle', {
        tag: '助詞-格助詞',
        dep: 'case',
      });
      b.inOrder(verb, no, 2);
      b.inOrder(no, ga, 1);
      b.captureSpan('のは', verb, ga);
    },
    // Branch 3: Verb + の + も (also marker)
    (b) => {
      const mo = b.particle('も', 'particle');
      b.inOrder(verb, no, 2);
      b.inOrder(no, mo, 1);
      b.captureSpan('のは', verb, mo);
    }
  );
});
