import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: Verb[potential] - Potential form (can/be able to)
 *
 * Matches verbs conjugated to potential form:
 * - る-verbs: 見られる, 食べられる, 着られる
 * - う-verbs: 書ける, 読める, 待てる, 死ねる, 遊べる
 * - irregular: できる, 来られる/こられる
 * - ら抜き言葉: 見れる, 食べれる (casual/informal)
 *
 * Polite forms: られます, けます, べます, etc.
 * Negative forms: られない, けません, etc.
 *
 * GiNZA parsing:
 * - る-verbs and irregulars are typically parsed as verb + auxiliary (見られる = 見る + られる)
 * - う-verbs in potential form are often single tokens (書ける), but their stem
 *   attaches to ます/ない/ません in the expected way
 * - Inflection form indicates conjugation (終止形-一般 for present, etc.)
 *
 * Note: This rule will also match passive and respectful forms that use
 * the same auxiliary (れる/られる), which is acceptable since they share
 * the same form. Context determines the actual meaning.
 */
export default bunproLinguisticRule('Verb[potential]', (r) => {
  r.either(
    // Pattern 1: Dictionary form potential verbs (書ける, 読める, いける, かける)
    // These are single tokens with potential form ending
    (b) => {
      const verb = b.verb({
        textOneOf: ['かける', 'いける', 'よめる', 'かえる', 'あそべる', 'まてる', 'もてる', 'とれる', 'われる'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'verb');
      b.capture(verb);
    },

    // Pattern 2: Potential form with auxiliary - る-verbs (見られる, 食べられる)
    (b) => {
      const aux = b.aux({
        lemmaOneOf: ['れる', 'られる'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'potential');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, aux);
      b.inOrder(verb, aux, 5);
      b.captureSpan('Verb[potential]', verb, aux);
    },

    // Pattern 3: Potential auxiliary followed by other auxiliaries (見られたら, みつけられなくて)
    (b) => {
      const aux = b.aux({
        lemmaOneOf: ['れる', 'られる'],
      }, 'potential');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, aux);
      b.inOrder(verb, aux, 5);

      // Capture from verb through the auxiliary (not including following auxiliaries)
      b.captureSpan('Verb[potential]', verb, aux);
    },

    // Pattern 4: Polite potential form (見られます, 食べられます, 書けます)
    (b) => {
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      // The stem (見られ, 書け) attaches to ます
      // Potential verb stems:
      // - Godan potential: 書け, 読め, 行け, 待て, 持て, 死ね, etc. (end in 'e' column kana)
      // - Ichidan potential: 見られ, 食べられ, etc. (end in 'られ' with at least 1 char before)
      // Exclude standard polite forms: 行き, 読み, 話し, etc. (end in 'i' column kana), and simple ichidan stems (食べ, 見)
      const stem = b.tok({
        // Match ending in 'e' column kana (えけげせぜてでねへべめれ) or ending in 'られ'/'れ' with 1+ chars before
        textRe: /[えけげせぜてでねへべめれ]$|^.{2,}れ$/,
      }, 'stem');

      b.auxOf(stem, masu);
      b.captureSpan('Verb[potential]', stem, masu);
    },

    // Pattern 5: Negative potential form (見られない, 食べられない, 書けない)
    (b) => {
      const nai = b.aux({ lemma: 'ない' }, 'nai');

      const stem = b.tok({
        textRe: /[えけげせぜてでねへべめれ]$|^.{2,}れ$/,
      }, 'stem');

      b.auxOf(stem, nai);
      b.captureSpan('Verb[potential]', stem, nai);
    },

    // Pattern 6: Negative polite potential (見られません, 食べられません, 書けません)
    (b) => {
      const masen = b.aux({ lemma: 'ません' }, 'masen');

      const stem = b.tok({
        textRe: /[えけげせぜてでねへべめれ]$|^.{2,}れ$/,
      }, 'stem');

      b.auxOf(stem, masen);
      b.captureSpan('Verb[potential]', stem, masen);
    },

    // Pattern 7: できる as standalone potential verb (provides literal trigger)
    (b) => {
      const dekiru = b.verb({
        lemma: 'できる',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'dekiru');
      b.capture(dekiru);
    },

    // Pattern 8: できます (polite form of できる)
    (b) => {
      const deki = b.verb({
        lemma: 'できる',
        inflectionForm: '連用形-一般',
      }, 'deki');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(deki, masu);
      b.captureSpan('Verb[potential]', deki, masu);
    },

    // Pattern 9: できない (negative できる)
    (b) => {
      const deki = b.verb({
        lemma: 'できる',
        inflectionForm: '未然形-一般',
      }, 'deki');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(deki, nai);
      b.captureSpan('Verb[potential]', deki, nai);
    },

    // Pattern 10: できません (negative polite できる)
    (b) => {
      const deki = b.verb({
        lemma: 'できる',
        inflectionForm: '未然形-一般',
      }, 'deki');
      const masen = b.aux({ lemma: 'ません' }, 'masen');
      b.auxOf(deki, masen);
      b.captureSpan('Verb[potential]', deki, masen);
    }
  );
});
