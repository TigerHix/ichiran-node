import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ないことはない - not impossible / it doesn't mean that it isn't
 *
 * Double negative construction meaning "(A) is not impossible" or "it doesn't mean that (A) isn't true"
 *
 * Patterns:
 * 1. Verb［ない］+ ことはない/ことはありません (casual/polite)
 *    - 食べられないことはない (It's not that I can't eat it)
 *    - 行けないことはない (It's not that I can't go)
 *    - できないことはない (It's not that I can't do it)
 *
 * 2. ［い］Adj［ない］+ ことはない/ことはありません
 *    - あぶなくないことはない (It's not that it isn't dangerous)
 *
 * 3. ［な］Adj + ではない/じゃない + ことはない/ことはありません
 *    - 元気ではないことはない (It doesn't mean that I'm not healthy)
 *    - 安全じゃないことはない (It's not that it isn't safe)
 *
 * Structure:
 * The first "ない" makes the verb/adj negative, and "ことはない" adds "it's not the case that",
 * creating a double negative: "it's not that I don't/it isn't (A)" = "it is possible that (A)"
 *
 * GiNZA parse structure:
 * - 食べられないことはない: 食べる(verb) + られる(aux) + ない(aux) + こと(noun) + は(particle) + ない(aux)
 * - あぶなくないことはない: あぶない(adj) + ない(aux) + こと(noun) + は(particle) + ない(aux)
 * - 元気ではないことはない: 元気(adj) + ではない(copula) + こと(noun) + は(particle) + ない(aux)
 */
export default linguisticRule('ないことはない', (r) => {
  r.either(
    // Branch 1: Verb negative + ことはない (casual)
    // 食べられないことはない, 行けないことはない, できないことはない
    // GiNZA: verb(る) + aux(られる) + aux(ない) + noun(こと) + particle(は) + aux(ない)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(verb, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない' }, 'nai2');
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', nai1, nai2);
    },

    // Branch 2: Verb negative + ことはありません (polite)
    // 行けないことはありません, 食べられないことはありません
    // GiNZA parses ありません as ある(lemma,連用形) + ます
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(verb, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ないことはない', nai1, masu);
    },

    // Branch 3: Verb negative + こと は + ありません (text match - alternate parse)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(verb, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const arimasen = b.tok({ textOneOf: ['ありませ', 'ありません'], lemma: 'ある' }, 'aru');
      b.inOrder(wa, arimasen, 1);

      b.captureSpan('ないことはない', nai1, arimasen);
    },

    // Branch 4: Verb negative + ことはないです (polite variant with desu)
    // できないことはないです
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(verb, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai2');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai2, desu);
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', nai1, desu);
    },

    // Branch 5: ［い］Adj negative + ことはない (casual)
    // あぶなくないことはない (It's not that it isn't dangerous)
    // GiNZA: adj + aux(ない) + noun(こと) + particle(は) + aux(ない)
    (b) => {
      const adj = b.adj({}, 'adj');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(adj, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない' }, 'nai2');
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', nai1, nai2);
    },

    // Branch 6: ［い］Adj negative + ことはありません (polite)
    (b) => {
      const adj = b.adj({}, 'adj');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(adj, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ないことはない', nai1, masu);
    },

    // Branch 7: ［い］Adj negative + ことはないです (polite variant with desu)
    (b) => {
      const adj = b.adj({}, 'adj');
      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.auxOf(adj, nai1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai2');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai2, desu);
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', nai1, desu);
    },

    // Branch 8: ［な］Adj + ではない + ことはない (casual)
    // 元気ではないことはない (It doesn't mean that I'm not healthy)
    // 安全ではないことはない (It's not that it isn't safe)
    // GiNZA: adj + aux(で,lemma=だ) + particle(は) + aux(ない) + noun(こと) + particle(は) + aux(ない)
    // Note: "では" is parsed as two tokens: で + は
    (b) => {
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', text: 'で' }, 'de');
      b.inOrder(adj, de, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(de, wa, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(wa, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa2 = b.particle('は', 'wa2');
      b.inOrder(koto, wa2, 1);

      const nai2 = b.tok({ lemma: 'ない' }, 'nai2');
      b.inOrder(wa2, nai2, 1);

      b.captureSpan('ないことはない', nai1, nai2);
    },

    // Branch 9: ［な］Adj + ではない + ことはありません (polite)
    (b) => {
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', text: 'で' }, 'de');
      b.inOrder(adj, de, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(de, wa, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(wa, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa2 = b.particle('は', 'wa2');
      b.inOrder(koto, wa2, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa2, aru, 1);

      b.captureSpan('ないことはない', nai1, masu);
    },

    // Branch 10: ［な］Adj + ではない + ことはないです (polite variant with desu)
    (b) => {
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', text: 'で' }, 'de');
      b.inOrder(adj, de, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(de, wa, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(wa, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa2 = b.particle('は', 'wa2');
      b.inOrder(koto, wa2, 1);

      const nai2 = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai2');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai2, desu);
      b.inOrder(wa2, nai2, 1);

      b.captureSpan('ないことはない', nai1, desu);
    },

    // Branch 11: ［な］Adj + じゃない + ことはない (casual, spoken)
    // 元気じゃないことはない (casual form of ではない)
    // GiNZA: adj + copula(じゃ) + aux(ない) + noun(こと) + particle(は) + aux(ない)
    // Note: "じゃない" may be parsed as one or two tokens depending on GiNZA
    (b) => {
      const adj = b.adj({}, 'adj');
      const ja = b.aux({ lemma: 'じゃ', text: 'じゃ' }, 'ja');
      b.inOrder(adj, ja, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(ja, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない' }, 'nai2');
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', ja, nai2);
    },

    // Branch 12: ［な］Adj + じゃない + ことはありません (polite, spoken)
    (b) => {
      const adj = b.adj({}, 'adj');
      const ja = b.aux({ lemma: 'じゃ', text: 'じゃ' }, 'ja');
      b.inOrder(adj, ja, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(ja, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ないことはない', ja, masu);
    },

    // Branch 13: ［な］Adj + じゃない + ことはないです (polite variant with desu, spoken)
    (b) => {
      const adj = b.adj({}, 'adj');
      const ja = b.aux({ lemma: 'じゃ', text: 'じゃ' }, 'ja');
      b.inOrder(adj, ja, 1);

      const nai1 = b.aux({ lemma: 'ない' }, 'nai1');
      b.inOrder(ja, nai1, 1);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai1, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai2 = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai2');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai2, desu);
      b.inOrder(wa, nai2, 1);

      b.captureSpan('ないことはない', ja, desu);
    }
  );
});
