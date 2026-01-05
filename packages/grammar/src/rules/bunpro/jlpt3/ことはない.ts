import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことはない - no need to / never happens
 *
 * Matches verb/adj in dictionary form + こと + は + ない/ありません/ないです
 *
 * This is DIFFERENT from たことがない (JLPT5 - past experience):
 * - JLPT5: Verb-た + ことがない = "have never done"
 * - JLPT3: Verb-dict form + ことはない = "there is no need to" or "never happens"
 *
 * Also DIFFERENT from ことがある (JLPT3 - "sometimes"):
 * - JLPT3: Verb-dict form + ことがある = "sometimes do"
 * - JLPT3: Verb-dict form + ことはない = "no need to" or "never happens"
 *
 * Structure:
 * - Verb［る］+ こと + は + ない/ありません/ないです (casual/polite)
 * - ［い］Adj + こと + は + ない/ありません/ないです
 * - ［な］Adj + な + こと + は + ない/ありません/ないです
 *
 * Note: ありません is the polite form of ない in this grammar pattern.
 * GiNZA may parse it as ある(lemma,連用形) + ます or as ありません(lemma).
 *
 * Examples:
 * - 心配することはない (There is no need to worry)
 * - 彼と話すことはない (I never talk to him)
 * - 沖縄に住んだら、雪を見ることはない (If you live in Okinawa, you never see snow)
 * - そんな急ぐことはない (There's no need to rush)
 * - もう彼に教えることはありません (There is no need to teach him anymore)
 *
 * GiNZA parse structure:
 * - 心配することはない: 心配(verb) + する(aux) + こと(noun) + は(particle) + ない(aux)
 * - 話すことはない: 話す(verb) + こと(noun) + は(particle) + ない(aux)
 * - 教えることはありません: 教える(verb) + こと(noun) + は(particle) + あります(verb)
 *
 * To exclude たことがない (past experience "have never done"), we ensure the verb
 * is NOT in past tense. The た-form patterns (食べたこと, 行ったこと) won't match
 * because we don't include た auxiliaries in our pattern.
 */
export default bunproLinguisticRule('ことはない', (r) => {
  r.either(
    // Branch 1: Verb + ことはない (casual)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 10);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない' }, 'nai');
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', verb, nai);
    },
    // Branch 2: Verb + ことはありません (polite - GiNZA parses as aru+masu)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 10);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ことはない', verb, masu);
    },
    // Branch 3: Verb + こと は + ありません (text match - alternate parse)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 10);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const arimasen = b.tok({ textOneOf: ['ありませ', 'ありません'], lemma: 'ある' }, 'aru');
      b.inOrder(wa, arimasen, 1);

      b.captureSpan('ことはない', verb, arimasen);
    },
    // Branch 4: Verb + ことはないです (polite variant with desu)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 10);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai, desu);
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', verb, desu);
    },
    // Branch 5: ［い］Adj + ことはない (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない' }, 'nai');
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', adj, nai);
    },
    // Branch 6: ［い］Adj + ことはありません (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ことはない', adj, masu);
    },
    // Branch 7: ［い］Adj + ことはないです (polite variant with desu)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai, desu);
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', adj, desu);
    },
    // Branch 8: ［な］Adj + な + ことはない (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない' }, 'nai');
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', adj, nai);
    },
    // Branch 9: ［な］Adj + な + ことはありません (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(wa, aru, 1);

      b.captureSpan('ことはない', adj, masu);
    },
    // Branch 10: ［な］Adj + な + ことはないです (polite variant with desu)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const wa = b.particle('は', 'wa');
      b.inOrder(koto, wa, 1);

      const nai = b.tok({ lemma: 'ない', inflectionForm: '連体形-一般' }, 'nai');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(nai, desu);
      b.inOrder(wa, nai, 1);

      b.captureSpan('ことはない', adj, desu);
    }
  );
});
