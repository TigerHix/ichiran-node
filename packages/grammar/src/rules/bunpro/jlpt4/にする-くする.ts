import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: にする・くする - Making something X state
 *
 * Matches adjective + する construction meaning "to make something (more) X".
 *
 * Structures:
 * - ［い］Adjective［く］ + する (i-adj + ku + suru)
 * - ［な］Adjective + に + する (na-adj + ni + suru)
 * - Noun + に + する (noun + ni + suru)
 *
 * Examples:
 * - 部屋を大きくする (make the room bigger)
 * - コーヒーを甘くする (make coffee sweeter)
 * - 部屋を綺麗にする (make the room clean)
 * - 元気にする (make healthy/cheer up)
 *
 * Key discriminators:
 * - I-adjectives: ADJ/VERB with tag=形容詞-一般, inflectionForm=連用形-一般
 * - Na-adjectives: ADJ/NOUN with tag=形状詞-一般 or 名詞-普通名詞-形状詞可能
 * - に is AUX/ADP with lemma=だ, inflectionForm=連用形-ニ
 *
 * GiNZA parse structure variations:
 * - Standard: 大きく (ADJ, 連用形-一般) + dep=advcl -> する
 * - Te-form: 大きく (ADJ, 連用形-一般) + し (AUX, 連用形-一般, dep=aux) -> 大きく
 * - Na-adj: 綺麗 (ADJ, 形状詞-一般) + に (AUX, 連用形-ニ) + dep=advcl -> する
 * - Te-form na-adj: 綺麗 (NOUN) + に (ADP, dep=case) + し (AUX, dep=aux)
 *
 * Note: In te-form (して, した), GiNZA analyzes し as an AUX attached TO the
 * adjective (reversed from standard form). Must handle both directions.
 */
export default bunproLinguisticRule('にする-くする', (r) => {
  r.either(
    // Branch 1: I-adjective + くする (standard form)
    // Example: 大きくする, 甘くする, 新しくする, 熱くする
    // The adjective is in adverbial form (連用形-一般) and modifies する
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'VERB'], // GiNZA sometimes uses VERB for i-adj
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
      }, 'adj');
      const suru = b.tok({
        lemma: 'する',
        pos: 'VERB', // Main verb form
      }, 'suru');
      b.headChild(suru, adj, 'advcl');
      b.inOrder(adj, suru, 1);
      b.captureSpan('にする・くする', adj, suru);
    },

    // Branch 2: I-adjective + くして (te-form)
    // Example: 大きくして, 新しくして, 甘くして
    // GiNZA parses: adj (ADJ) + し (AUX, dep=aux -> adj OR dep=advcl)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'VERB'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
      }, 'adj');
      const shi = b.tok({
        lemma: 'する',
        pos: 'AUX',
        inflectionForm: '連用形-一般',
      }, 'shi');
      // shi is immediately after adj, with either aux or advcl dependency
      b.inOrder(adj, shi, 1);
      b.captureSpan('にする・くする', adj, shi);
    },

    // Branch 2b: I-adjective + くする (AUX form with dep=aux)
    // Example: おもしろくする (modified noun before verb)
    // GiNZA parses: adj (ADJ) + する (AUX, dep=aux -> adj)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'VERB'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
      }, 'adj');
      const suru = b.tok({
        lemma: 'する',
        pos: 'AUX',
        inflectionFormOneOf: ['連体形-一般', '連用形-一般'],
      }, 'suru');
      b.auxOf(adj, suru);
      b.captureSpan('にする・くする', adj, suru);
    },

    // Branch 3: Na-adjective + にする (standard form)
    // Example: 綺麗にする, 丈夫にする, 元気にする
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'NOUN'],
        tagOneOf: ['形状詞-一般', '名詞-普通名詞-形状詞可能'],
      }, 'adj');
      const ni = b.tok({
        lemma: 'だ',
        posOneOf: ['AUX', 'ADP'], // GiNZA varies
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.inOrder(adj, ni, 1);
      const suru = b.tok({
        lemma: 'する',
        pos: 'VERB',
      }, 'suru');
      b.headChild(suru, adj, 'advcl');
      b.inOrder(ni, suru, 1);
      b.captureSpan('にする・くする', adj, suru);
    },

    // Branch 4: Na-adjective + にして (te-form)
    // Example: 綺麗にして, 親切にして
    // GiNZA parses: adj (NOUN/ADJ) + に (ADP, dep=case) + し (AUX, dep=aux -> adj)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'NOUN'],
        tagOneOf: ['形状詞-一般', '名詞-普通名詞-形状詞可能'],
      }, 'adj');
      const ni = b.tok({
        lemma: 'だ',
        posOneOf: ['AUX', 'ADP'],
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.inOrder(adj, ni, 1);
      const shi = b.tok({
        lemma: 'する',
        pos: 'AUX',
        inflectionForm: '連用形-一般',
      }, 'shi');
      b.auxOf(adj, shi);
      b.inOrder(ni, shi, 1);
      b.captureSpan('にする・くする', adj, shi);
    }
  );
});
