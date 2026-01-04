import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: たらいい・といい - "it would be good if", "should do", "I hope"
 *
 * Matches two conditional patterns expressing wishes or suggestions:
 *
 * Structure:
 * - Verb［たら］+ いい - "it would be good if [verb happened]" (medium certainty)
 * - Verb + と + いい - "it would be good if [verb happens]" (high certainty)
 *
 * Note: ばいい is a separate grammar point (JLPT3) with lower certainty
 *
 * This expresses the speaker's wish or hope for something to happen.
 * Unlike たほうがいい (suggestion/advice), this can express hope for
 * things outside one's control (e.g., weather, luck).
 *
 * Examples:
 * - しゃべったらいい (it would be good if you speak / you should speak)
 * - はれるといい (I hope it's sunny)
 * - したらいい (what should I do?)
 * - 聞いてみるといい (it would be good to try asking)
 * - あえたらいいな (I hope we can meet)
 *
 * GiNZA parse structure:
 * - たら: AUX with lemma=たら, dep=mark (conditional marker)
 * - と: SCONJ/ADP with lemma=と (quotational/conditional particle)
 * - いい: ADJ with lemma=いい (i-adjective "good")
 *
 * Key discriminators:
 * - たら follows verb ta-form or adjective/auxiliary ta-form
 * - と follows verb dictionary form or potential form
 * - Both must be immediately followed by いい (or いいです for polite)
 */
export default linguisticRule('たらいい-といい', (r) => {
  r.either(
    // Pattern 1a: Verb/Adj + たら (single token) + いい
    // GiNZA sometimes parses たら as a single AUX token
    // E.g., したらいい, しゃべったらいい where たら is one token
    (b) => {
      const verbOrAdj = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
      }, 'verbOrAdj');

      // たら as a single token (text match is sufficient trigger)
      const tara = b.aux({
        textOneOf: ['たら', 'ッタラ'],
      }, 'tara');

      // たら attaches to the preceding verb/adj
      b.auxOf(verbOrAdj, tara);

      // いい follows たら (allow small distance for particles/aux)
      const ii = b.tok({
        text: 'いい',
      }, 'ii');

      b.inOrder(tara, ii, 3);
      b.captureSpan('たらいい-といい', verbOrAdj, ii);
    },

    // Pattern 1b: Verb/Adj ending in た + ら + いい
    // GiNZA parses as: ...た (VERB/AUX) + ら (AUX) + いい
    // E.g., いったらいい, およげたらいい, しゃべったらいい
    (b) => {
      // Match any verb/aux that ends in た
      const verbEndingInTa = b.tok({
        posOneOf: ['VERB', 'AUX', 'ADJ'],
        text: 'た',
      }, 'verbEndingInTa');

      // ら is the conditional marker
      const ra = b.aux({
        text: 'ら',
        lemma: 'ら',
      }, 'ra');

      // ら attaches to the verb ending in た
      b.auxOf(verbEndingInTa, ra);

      // いい follows ら (allow small distance)
      const ii = b.tok({
        text: 'いい',
      }, 'ii');

      b.inOrder(ra, ii, 3);
      b.captureSpan('たらいい-といい', verbEndingInTa, ii);
    },

    // Pattern 2: Verb + と + いい
    // はれるといい, しゃべるといい, あえるといい
    // やってみるといい, 聞いてみるといい, やっているといい
    (b) => {
      const verb = b.verb({}, 'verb');

      // と is the conditional/quotational particle
      const to = b.tok({
        text: 'と',
        lemma: 'と',
      }, 'to');

      // Verb comes before と
      b.inOrder(verb, to, 3);

      // いい immediately follows と (this is our trigger)
      const ii = b.tok({
        text: 'いい',
      }, 'ii');

      b.inOrder(to, ii, 1);
      b.captureSpan('たらいい-といい', verb, ii);
    }
  );
});
