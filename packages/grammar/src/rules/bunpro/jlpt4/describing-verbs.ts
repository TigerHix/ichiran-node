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
    // Branch 1a: I-adjective + く (連用形-一般) with dep=advcl
    // Example: 強く, きつく, 新しく, etc. (kanji forms)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'ADV', 'VERB'],
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

    // Branch 1b: I-adjective + く with dep=advmod (hiragana forms like あまく)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'ADV', 'VERB'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: [
          '甘い', 'あまい',  // sweet (shows as advmod in hiragana)
        ],
      }, 'adj');
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'advmod');
      b.captureSpan('く・に', adj, adj);
    },

    // Branch 1c: I-adjective + く with adj as root (e.g., あつく)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'ADV', 'VERB'],
        tag: '形容詞-一般',
        inflectionForm: '連用形-一般',
        lemmaOneOf: [
          '熱い', 'あつい',  // hot/passionate (shows as root)
        ],
      }, 'adj');
      const verb = b.verb({}, 'verb');
      b.headChild(adj, verb, 'advcl');  // verb depends on adj
      b.captureSpan('く・に', adj, adj);
    },

    // Branch 2a: Na-adjective + に with dep=aux (standard kanji forms)
    // Example: 丁寧に, 上手に, etc.
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'NOUN'],
        tagOneOf: ['形状詞-一般'],
        lemmaOneOf: [
          '丁寧', 'ていねい',  // polite
          '上手', 'じょうず',  // skilled
          '適当', 'てきとう',  // appropriate/haphazard
          '前向き', 'まえむき',  // positive
          '綺麗', 'きれい',  // clean/beautiful
          '早め', 'はやめ',  // early
          '静か', 'しずか',  // quiet
          '大切', 'たいせつ',  // important
        ],
      }, 'adj');
      const ni = b.tok({
        posOneOf: ['AUX', 'ADP'],
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.auxOf(adj, ni);
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'advcl');
      b.captureSpan('く・に', adj, ni);
    },

    // Branch 2b: Na-adjective + に with hiragana forms (名詞-普通名詞-形状詞可能) + dep=case + obl
    // Example: ゆうがに
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'NOUN'],
        tagOneOf: ['名詞-普通名詞-形状詞可能'],
        lemmaOneOf: [
          '優雅', 'ゆうが',  // elegant
        ],
      }, 'adj');
      const ni = b.tok({
        posOneOf: ['ADP'],
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.headChild(adj, ni, 'case');
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'obl');
      b.captureSpan('く・に', adj, ni);
    },

    // Branch 2c: Na-adjective + に with hiragana forms + dep=aux + obl (しんせつに)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ', 'NOUN'],
        tagOneOf: ['名詞-普通名詞-形状詞可能'],
        lemmaOneOf: [
          '親切', 'しんせつ',  // kind
        ],
      }, 'adj');
      const ni = b.tok({
        posOneOf: ['AUX', 'ADP'],
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.auxOf(adj, ni);
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'obl');
      b.captureSpan('く・に', adj, ni);
    },

    // Branch 2d: Na-adjective + に with dep=case (しずかに)
    (b) => {
      const adj = b.tok({
        posOneOf: ['ADJ'],
        tag: '形状詞-一般',
        lemmaOneOf: [
          '静か', 'しずか',  // quiet
        ],
      }, 'adj');
      const ni = b.tok({
        posOneOf: ['ADP'],
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
      }, 'ni');
      b.headChild(adj, ni, 'case');
      const verb = b.verb({}, 'verb');
      b.headChild(verb, adj, 'advcl');
      b.captureSpan('く・に', adj, ni);
    }
  );
});
