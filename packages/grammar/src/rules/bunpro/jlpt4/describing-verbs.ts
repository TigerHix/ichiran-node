import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: describing-verbs - Adverbial forms (く・に)
 *
 * Matches i-adjective or na-adjective used adverbially before a verb.
 *
 * Structures:
 * - ［い］Adjective［く］ + Verb (i-adj + ku)
 * - ［な］Adjective + に + Verb (na-adj + ni)
 *
 * Examples:
 * - 強く引く (pull strongly)
 * - きつく閉める (close tightly)
 * - 新しく買う (buy new)
 * - 丁寧に書く (write neatly)
 * - 上手に歌う (sing well)
 * - 適当に話す (speak haphazardly)
 *
 * Key discriminators:
 * - I-adjectives: ADJ with tag=形容詞-一般, inflectionForm=連用形-一般
 * - Na-adjectives: ADJ with tag=形状詞-一般 + AUX with lemma=だ, inflectionForm=連用形-ニ
 * - The adverbial form modifies the verb with dep=advcl
 *
 * GiNZA parse structure:
 * - 強く: ADJ(形容詞-一般, 連用形-一般) + dep=advcl -> verb
 * - 丁寧に: ADJ(形状詞-一般) + AUX(lemma=だ, 連用形-ニ, dep=aux) + dep=advcl -> verb
 *
 * Note: This is for productive adverbial forms, not lexical adverbs.
 * True adverbs (e.g., もっと, とても) are not matched by this rule.
 */
export default linguisticRule('describing-verbs', (r) => {
  r.either(
    // Branch 1: I-adjective + く (連用形-一般)
    // Example: 強く, きつく, 新しく, かるく, あまく, ひろく, はげしく, あつく, やさしく
    // Must be ADJ with tag=形容詞-一般 and inflectionForm=連用形-一般
    // Note: GiNZA stores lemmas variably - kanji (強い) or hiragana (つよい) depending on surface form
    // We include all observed forms to handle this inconsistency
    (b) => {
      const adj = b.adj({
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: [
          '強い', 'つよい',  // strong
          'きつい',  // tight
          '新しい', 'あたらしい',  // new
          '軽い', 'かるい',  // light
          '甘い', 'あまい',  // sweet
          '広い', 'ひろい',  // wide
          '激しい', 'はげしい',  // fierce
          '熱い', 'あつい',  // hot
          '優しい', 'やさしい',  // gentle
        ],
      }, 'adj');
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'advcl');
      b.captureSpan('く・に', adj, adj);
    },

    // Branch 2: Na-adjective + に
    // Example: 丁寧に, 上手に, 適当に, まえむきに, きれいに, ゆうがに, はやめに, しずかに, たいせつに, しんせつに
    // Na-adjective is ADJ with tag=形状詞-一般
    // に is AUX with lemma=だ, inflectionForm=連用形-ニ, dep=aux
    // Note: GiNZA stores lemmas variably - kanji (丁寧) or hiragana (ていねい) depending on surface form
    // We include all observed forms to handle this inconsistency
    (b) => {
      const adj = b.adj({
        tag: '形状詞-一般',
        lemmaOneOf: [
          '丁寧', 'ていねい',  // polite
          '上手', 'じょうず',  // skilled
          '適当', 'てきとう',  // appropriate/haphazard
          '前向き', 'まえむき',  // positive
          '綺麗', 'きれい',  // clean/beautiful
          '優雅', 'ゆうが',  // elegant
          '早め', 'はやめ',  // early
          '静か', 'しずか',  // quiet
          '大切', 'たいせつ',  // important
          '親切', 'しんせつ',  // kind
        ],
      }, 'adj');
      const ni = b.aux({
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.auxOf(adj, ni);
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'advcl');
      b.captureSpan('く・に', adj, ni);
    }
  );
});
