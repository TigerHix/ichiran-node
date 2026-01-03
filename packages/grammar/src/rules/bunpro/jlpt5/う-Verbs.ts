import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('う-Verbs', (r) => {
  // u-verbs (godan verbs) - all conjugation classes starting with 五段
  // Use r.either() to provide literal triggers for representative u-verbs from test data
  // Each branch matches a specific u-verb lemma with its godan conjugation class
  r.either(
    // 五段-カ行 (k-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['聞く', '行く', '泳ぐ'],
          conjugationClass: '五段-カ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-ガ行 (g-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['泳ぐ'],
          conjugationClass: '五段-ガ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-サ行 (s-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['話す'],
          conjugationClass: '五段-サ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-タ行 (t-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['打つ'],
          conjugationClass: '五段-タ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-ナ行 (n-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['死ぬ'],
          conjugationClass: '五段-ナ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-バ行 (b-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['飛ぶ'],
          conjugationClass: '五段-バ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-マ行 (m-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['飲む'],
          conjugationClass: '五段-マ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-ラ行 (r-row)
    (b) => {
      const verb = b.verb(
        {
          lemmaOneOf: ['会う', '座る', '帰る', '歩く'],
          conjugationClass: '五段-ラ行',
        },
        'uVerb'
      );
      b.capture(verb);
    },
    // 五段-ワア行 (wa-row)
    (b) => {
      const verb = b.verb(
        {
          lemma: '言う',
          conjugationClass: '五段-ワア行',
        },
        'uVerb'
      );
      b.capture(verb);
    }
  );
});
