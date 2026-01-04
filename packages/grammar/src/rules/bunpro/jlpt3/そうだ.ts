import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: そうだ - hearsay (I heard that / they say that)
 *
 * This matches the HEARSAY usage of そうだ, which reports information heard from others.
 * This is different from the APPEARANCE usage (样態), which uses stem forms.
 *
 * Structure for HEARSAY (this rule):
 * - Verb［dictionary form］+ そうだ/そうです (e.g., 降るそうだ - I heard it will rain)
 * - い-adj［dictionary form］+ そうだ/そうです (e.g., 美味しいそうだ - I heard it's delicious)
 * - Noun/な-adj + だ + そうだ/そうです (e.g., 女優だそうだ - I heard she's an actress)
 *
 * Structure for APPEARANCE (different rule - JLPT4):
 * - Verb［stem form］+ そうだ (e.g., 降りそうだ - looks like it will rain)
 * - い-adj［drop い］+ そうだ (e.g., 寒そうだ - looks cold)
 * - Noun/な-adj + そうだ (e.g., 綺麗そうだ - looks beautiful)
 *
 * The key discriminator:
 * - Hearsay: tag='名詞-助動詞語幹' (noun auxiliary stem) + plain form attachment
 * - Appearance: tag='形状詞-助動詞語幹' (adjectival auxiliary stem) + stem attachment
 *
 * Examples from test data:
 * - 来週から気温がさがるそうだ (I heard the temperature will drop next week)
 * - 今晩雨がふるそうだ (I heard it will rain tonight)
 * - 美味しいそうだから、たくさん買った (I heard they're good, so I bought a lot)
 * - やさしいそうだ (I heard he/she is kind)
 * - 女優だそうだ (I heard she's an actress)
 * - 親切だそうです (I heard that person is kind - polite)
 * - 綺麗だそうだ (I heard the sunset is beautiful)
 *
 * GiNZA parse structure:
 * - さがるそうだ: さがる(verb) + そう(aux, tag=名詞-助動詞語幹) + だ(aux)
 * - 美味しいそうだ: 美味しい(adj) + そう(aux, tag=名詞-助動詞語幹) + だ(aux)
 * - 女優だそうだ: 女優(noun) + だ(aux/copula) + そう(aux, tag=名詞-助動詞語幹) + だ(aux)
 * - 親切だそうです: 親切(adj/na) + だ(aux/copula) + そう(aux, tag=名詞-助動詞語幹) + です(aux/polite)
 *
 * Discriminator to avoid matching appearance form:
 * - Must have tag='名詞-助動詞語幹' (hearsay), NOT '形状詞-助動詞語幹' (appearance)
 */
export default linguisticRule('そうだ', (r) => {
  r.either(
    // Branch 1: Verb/adj + そう + だ (with tag constraint - primary)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
        tagOneOf: ['名詞-助動詞語幹', '助動詞-一般'],
      }, 'sou');
      b.inOrder(pred, sou, 1);

      const da = b.aux({
        lemma: 'だ',
      }, 'copula');
      b.inOrder(sou, da, 1);

      b.captureSpan('そうだ', pred, da);
    },
    // Branch 2: Verb/adj + そう + です (with tag constraint)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
        tagOneOf: ['名詞-助動詞語幹', '助動詞-一般'],
      }, 'sou');
      b.inOrder(pred, sou, 1);

      const desu = b.aux({
        lemma: 'です',
      }, 'copula');
      b.inOrder(sou, desu, 1);

      b.captureSpan('そうだ', pred, desu);
    },
    // Branch 3: Verb/adj + そう + だ (without tag - for sentences where GiNZA doesn't tag)
    // Require pred to be in dictionary form to avoid matching appearance (stem form)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '終止形-一般',  // Dictionary form
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
      }, 'sou');
      b.inOrder(pred, sou, 1);

      const da = b.aux({
        lemma: 'だ',
      }, 'copula');
      b.inOrder(sou, da, 1);

      b.captureSpan('そうだ', pred, da);
    },
    // Branch 4: Verb/adj + そう + です (without tag)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '終止形-一般',
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
      }, 'sou');
      b.inOrder(pred, sou, 1);

      const desu = b.aux({
        lemma: 'です',
      }, 'copula');
      b.inOrder(sou, desu, 1);

      b.captureSpan('そうだ', pred, desu);
    },
    // Branch 5: Noun + だ + そう + だ
    (b) => {
      const pred = b.noun({}, 'pred');
      const da1 = b.aux({
        lemma: 'だ',
      }, 'da1');
      b.copulaOf(pred, da1);
      b.inOrder(pred, da1, 1);

      const sou = b.tok({
        text: 'そう',
        tagOneOf: ['名詞-助動詞語幹', '助動詞-一般'],
      }, 'sou');
      b.inOrder(da1, sou, 1);

      const da2 = b.aux({
        lemma: 'だ',
      }, 'copula');
      b.inOrder(sou, da2, 1);

      b.captureSpan('そうだ', pred, da2);
    },
    // Branch 6: Noun + だ + そう + です
    (b) => {
      const pred = b.noun({}, 'pred');
      const da1 = b.aux({
        lemma: 'だ',
      }, 'da1');
      b.copulaOf(pred, da1);
      b.inOrder(pred, da1, 1);

      const sou = b.tok({
        text: 'そう',
        tagOneOf: ['名詞-助動詞語幹', '助動詞-一般'],
      }, 'sou');
      b.inOrder(da1, sou, 1);

      const desu = b.aux({
        lemma: 'です',
      }, 'copula');
      b.inOrder(sou, desu, 1);

      b.captureSpan('そうだ', pred, desu);
    }
  );
});
