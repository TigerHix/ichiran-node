import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('そうだ', (r) => {
  r.either(
    // Branch 1: Verb/adj (+ aux) + そう + だ (with tag=名詞-助動詞語幹 - definite hearsay)
    // Allows one auxiliary between pred and sou (e.g., あったそうだ, 到達するそうだ)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
        tag: '名詞-助動詞語幹',
      }, 'sou');
      b.inOrder(pred, sou, 2);  // Allow up to 2 tokens distance (pred + aux + sou)

      const da = b.aux({
        lemma: 'だ',
      }, 'copula');
      b.inOrder(sou, da, 1);

      b.captureSpan('そうだ', pred, da);
    },
    // Branch 2: Verb/adj (+ aux) + そう + です (with tag=名詞-助動詞語幹 - definite hearsay)
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'ADJ'],
      }, 'pred');
      const sou = b.tok({
        text: 'そう',
        tag: '名詞-助動詞語幹',
      }, 'sou');
      b.inOrder(pred, sou, 2);  // Allow up to 2 tokens distance

      const desu = b.aux({
        lemma: 'です',
      }, 'copula');
      b.inOrder(sou, desu, 1);

      b.captureSpan('そうだ', pred, desu);
    },
    // Branch 3: Noun (+ aux) + そう + だ
    // For compound verbs where GiNZA parses as NOUN + AUX + そう (e.g., さむくなるそうだ)
    (b) => {
      const pred = b.noun({}, 'pred');
      const sou = b.tok({
        text: 'そう',
        tag: '名詞-助動詞語幹',
      }, 'sou');
      b.inOrder(pred, sou, 2);  // Allow up to 2 tokens distance (noun + aux + sou)

      const da = b.aux({
        lemma: 'だ',
      }, 'copula');
      b.inOrder(sou, da, 1);

      b.captureSpan('そうだ', pred, da);
    },
    // Branch 4: Noun (+ aux) + そう + です
    (b) => {
      const pred = b.noun({}, 'pred');
      const sou = b.tok({
        text: 'そう',
        tag: '名詞-助動詞語幹',
      }, 'sou');
      b.inOrder(pred, sou, 2);  // Allow up to 2 tokens distance

      const desu = b.aux({
        lemma: 'です',
      }, 'copula');
      b.inOrder(sou, desu, 1);

      b.captureSpan('そうだ', pred, desu);
    },
    // Branch 5: Verb (dictionary form) + そう + だ
    // For cases where GiNZA tags そう as 形状詞-助動詞語幹 (usually for appearance)
    // Discriminator: pred must be verb in dictionary form (text === lemma), not stem form
    // - Hearsay: 鳴くそうだ (text=鳴く, lemma=鳴く, tag=動詞-一般) ✓
    // - Appearance: 降りそうだ (text=降り, lemma=降る) ✗
    // - Appearance: 元気そうだ (ADJ, not VERB) ✗
    // - Appearance: 人気そうだ (VERB but tag=名詞-普通名詞-一般) ✗
    (b) => {
      const pred = b.verb({
        textEqualsLemma: true,  // Dictionary form (text === lemma)
        tag: '動詞-一般',  // Must be actual verb, not noun mis-tagged as verb
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
    // Branch 6: Verb (dictionary form) + そう + です
    (b) => {
      const pred = b.verb({
        textEqualsLemma: true,  // Dictionary form (text === lemma)
        tag: '動詞-一般',  // Must be actual verb, not noun mis-tagged as verb
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
    // Branch 7: Noun/na-adj + だ + そう + だ
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
    // Branch 8: Noun/na-adj + だ + そう + です
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
