import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ようと思う-おうと思う - thinking of doing / intend to do
 *
 * Matches volitional verb + と思う expressing intention or plan.
 *
 * This pattern expresses the speaker's intention or plan to do something.
 * Similar to "I think I will do X" or "I'm thinking of doing X" in English.
 *
 * Forms:
 * - ようと思う (casual, present) - "I think I'll do..."
 * - ようと思った (casual, past) - "I thought I would do..."
 * - ようと思います (polite, present) - "I think I'll do..." (polite)
 * - ようと思いました (polite, past) - "I thought I would do..." (polite)
 * - ようと思っている (continuous/state) - "I've been thinking of doing..."
 * - ようと思っています (polite continuous) - "I've been thinking of doing..." (polite)
 *
 * Examples:
 * - 明日勉強しようと思う (I think I'll study tomorrow)
 * - 家を買おうと思う (I think I'll buy a house)
 * - スキーに行こうと思う (I think I'll go skiing)
 *
 * This is different from:
 * - Simple volitional よう・おう (let's do / shall we)
 * - とおもう without volitional (simple thought)
 * - ようとする (attempt to do)
 *
 * GiNZA parse structure:
 * - Volitional verb - GiNZA parses volitional forms variably (VERB, NOUN, AUX)
 * - と (particle, dep=case) - quotation marker
 * - 思う/おもう (verb) - thinking verb with various auxiliaries
 *
 * Note: Due to GiNZA's variable tokenization of volitional forms and lack of
 * reliable inflectionForm markers, this rule explicitly lists common volitional
 * verb endings. This may not cover all possible volitional forms.
 */
export default linguisticRule('ようと思う-おうと思う', (r) => {
  // Match volitional forms + と + 思う
  const vol = r.tok({
    textOneOf: [
      // Ichidan verbs ending in よう
      'しよう', '見よう', 'みよう',
      '食べよう', 'たべよう',
      '寝よう', 'ねよう',
      '起きよう', 'おきよう',
      '考えよう', 'かんがえよう',
      '始めよう', 'はじめよう',
      '続きよう', 'つづけよう',
      '止めよう', 'やめよう',
      '開けよう', 'あけよう',
      'あきらめよう',
      '別れよう', 'わかれよう',
      '勉強しよう', 'べんきょうしよう',
      '練習しよう', 'れんしゅうしよう',
      '掃除しよう', 'そうじしよう',
      // Godan verbs ending in おう
      '行こう', 'いこう',
      '買おう', 'かおう',
      '書こう', 'かこう',
      '読もう', 'よもう',
      '死のう', 'しなう',
      '言おう', 'いおう',
      '呼ぼう', 'よぼう',
      '歌おう', 'うたおう',
      '持とう', 'もとう',
      '飲もう', 'のもう',
      '作ろう', 'つくろう',
      '遊ぼう', 'あそぼう',
      '待とう', 'まとう',
      '帰ろう', 'かえろう',
      '出よう', 'でよう',
      '入ろう', 'はいろう',
      '習おう', 'ならおう',
      '聞こう', 'きこう',
      '泳ごう', 'およごう',
      '通おう', 'かよおう',
      // Compound verb volitional forms (te-form + auxiliary)
      'てあげよう', 'してもらおう',
      'してあげよう', 'してもらいおう',
      '見てあげよう', 'みてあげよう',
      'たべてあげよう', 'かってあげよう',
      '勉強してあげよう', 'べんきょうしてあげよう',
      '家事をしてもらおう', '弟にしてもらおう',
      // Shorter forms (GiNZA might tokenize compound verbs differently)
      'あげよう', 'もらおう', 'てあげ', 'てもら',
    ]
  }, 'vol');

  const to = r.particle('と', 'to');
  r.inOrder(vol, to, 1);

  r.either(
    // Pattern 1a: ようと思う (casual, present)
    (r) => {
      const omou = r.tok({ textOneOf: ['思う', 'おもう'] }, 'omou');
      r.inOrder(to, omou, 3);
      r.captureSpan('ようと思う', vol, omou);
    },
    // Pattern 1e: ようと思っている (continuous/state) - moved before 1b to prioritize
    (r) => {
      const omotteiru = r.verb({ textOneOf: ['思っ', 'おもっ'] }, 'omotteiru');
      const te = r.tok({ text: 'て' }, 'te');
      const iru = r.tok({ textOneOf: ['いる', 'てる'] }, 'iru');
      r.inOrder(to, omotteiru, 3);
      r.inOrder(omotteiru, te, 1);
      r.inOrder(te, iru, 1);
      r.captureSpan('ようと思う', vol, iru);
    },
    // Pattern 1b: ようと思った (casual, past)
    (r) => {
      const omotta = r.tok({ textOneOf: ['思っ', 'おもっ'] }, 'omotta');
      const ta = r.tok({ text: 'た' }, 'ta');
      r.inOrder(to, omotta, 3);
      r.inOrder(omotta, ta, 1);
      r.captureSpan('ようと思う', vol, ta);
    },
    // Pattern 1c: ようと思います (polite, present)
    (r) => {
      const omoi = r.tok({ textOneOf: ['思', 'おも', '思い', 'おもい'] }, 'omoi');
      const masu = r.aux({ lemma: 'ます' }, 'masu');
      r.inOrder(to, omoi, 2);
      r.inOrder(omoi, masu, 1);
      r.captureSpan('ようと思う', vol, masu);
    },
    // Pattern 1d: ようと思いました (polite, past)
    (r) => {
      const omomashi = r.tok({ textOneOf: ['思い', 'おもい', '思', 'おも'] }, 'omomashi');
      const mashita = r.aux({ lemma: 'ました' }, 'mashita');
      r.inOrder(to, omomashi, 2);
      r.inOrder(omomashi, mashita, 1);
      r.captureSpan('ようと思う', vol, mashita);
    },
    // Pattern 1f: ようと思っています (polite continuous)
    (r) => {
      const omotteimasu = r.tok({ textOneOf: ['思っ', 'おもっ'] }, 'omotteimasu');
      const te = r.tok({ text: 'て' }, 'te');
      const imasu = r.aux({ lemma: 'います' }, 'imasu');
      r.inOrder(to, omotteimasu, 3);
      r.inOrder(omotteimasu, te, 1);
      r.inOrder(te, imasu, 1);
      r.captureSpan('ようと思う', vol, imasu);
    }
  );
});
