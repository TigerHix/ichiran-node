import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('お-になる', (r) => {
  // Case particle に
  const ni = r.particle('に', 'ni');

  // Verb なる (in various inflection forms)
  // 連用形-一般 (ren'youkei) - before ます: お帰りになります
  // 連用形-促音便 (ren'youkei-sokuonbin) - past: お帰りになった
  // 連体形-一般 (rentaikei) - before の: お見えになるのは
  // 仮定形-一般 (kateikei) - conditional: ご参考になれば
  const naru = r.verb({
    lemma: 'なる',
    inflectionFormOneOf: [
      '連用形-一般',
      '連用形-促音便',
      '連体形-一般',
      '仮定形-一般',
    ],
  }, 'naru');

  // Optional polite auxiliary (ます, ました, etc.) or other auxiliaries
  r.optional((b) => {
    const aux = b.aux({}, 'polite');
    b.auxOf(naru, aux);
  });

  // Two pattern variants:
  r.either(
    // Pattern 1: Separate honorific prefix + stem
    // e.g., お + 帰り + に + なる
    (b) => {
      const honorificPrefix = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
      }, 'prefix');

      const verbStem = b.tok({
        posOneOf: ['NOUN', 'VERB'],
      }, 'stem');

      b.inOrder(honorificPrefix, verbStem, 1);
      b.inOrder(verbStem, ni, 1);
      b.inOrder(ni, naru, 1);
      b.captureSpan('お-になる', honorificPrefix, naru);
    },
    // Pattern 2: Compound honorific nouns (includes お/ご)
    // e.g., おでかけ, おとり, おいで, ご覧
    (b) => {
      const compoundStem = b.tok({
        pos: 'NOUN',
        textOneOf: ['おでかけ', 'おとり', 'おいで', 'ご覧', 'おめしあがり'],
      }, 'stem');

      b.inOrder(compoundStem, ni, 1);
      b.inOrder(ni, naru, 1);
      b.captureSpan('お-になる', compoundStem, naru);
    }
  );
});
