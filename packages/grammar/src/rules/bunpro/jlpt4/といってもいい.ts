import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: といってもいい (だといってもいい) - you could say that... / you might say that...
 *
 * Matches a quoted phrase + といってもいい / だといってもいい
 *
 * This pattern expresses that the speaker considers something to be logically true and
 * can be stated. It's used to qualify a statement with "you could say" or "it's fair to say."
 *
 * Structure:
 * - Verb/Phrase + といってもいい
 * - [い]Adjective + といってもいい (no だ)
 * - [な]Adjective/Noun + だ + といってもいい
 * - Casual variant: っていってもいい
 *
 * Examples:
 * - 彼は全然日本語が出来ないといってもいい。
 *   (You could say that he can't speak a word of Japanese.)
 * - このワインは飲みやすいといってもいい。
 *   (One might say that this is a palatable wine.)
 * - 彼はとてもいい人だといってもいいです。
 *   (You might say that he is a very good person.)
 *
 * Key discriminators from similar patterns:
 * - Simple quotation と (just marking quoted content)
 * - という (called/named) - e.g., 田中という人
 * - ということだ (it means that / I hear that) - more definitive hearsay
 * - といえる (can be said) - stronger assertion
 *
 * GiNZA parse structure:
 * - と (particle/ADP with case dep) - quotation particle
 * - いう (verb) - "to say" in 促音便 form (いっ)
 * - ても (te-form + emphasis) - "even if"
 * - いい (adj/AUX) - "good"
 *
 * The pattern is: と + いう(conjugated to いっ) + ても + いい
 * where "いっても" is the て-form of いう + も
 */
export default linguisticRule('といってもいい', (r) => {
  // Quotation particle と
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: といってもいい (after verbs, i-adjectives)
    // e.g., 出来ないといってもいい、飲みやすいといってもいい
    (b) => {
      // 言う in 促音便 form (連用形-促音便) - appears as "いっ"
      const iu = b.verb({
        lemma: 'いう',
        inflectionForm: '連用形-促音便',
      }, 'iu');

      // て-form connector
      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      // Emphasis particle も
      const mo = b.tok({
        text: 'も',
        pos: 'ADP',
        dep: 'fixed',
      }, 'mo');

      // いい (good/okay) - sentence-final adjective
      const ii = b.tok({
        lemmaOneOf: ['いい', 'よい'],
        posOneOf: ['AUX', 'ADJ'],
        dep: 'fixed',
      }, 'ii');

      // Structural constraints
      b.headChild(iu, te, 'mark');
      b.headChild(te, mo, 'fixed');
      b.headChild(te, ii, 'fixed');

      // Order constraints
      b.inOrder(to, iu, 3);
      b.inOrder(iu, te, 1);
      b.inOrder(te, mo, 1);
      b.inOrder(mo, ii, 1);

      b.captureSpan('といってもいい', to, ii);
    },
    // Pattern 2: だといってもいい (after nouns, na-adjectives)
    // e.g., いい人だといってもいい、丁寧だといってもいい
    (b) => {
      const da = b.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      const iu = b.verb({
        lemma: 'いう',
        inflectionForm: '連用形-促音便',
      }, 'iu');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const mo = b.tok({
        text: 'も',
        pos: 'ADP',
        dep: 'fixed',
      }, 'mo');

      const ii = b.tok({
        lemmaOneOf: ['いい', 'よい'],
        posOneOf: ['AUX', 'ADJ'],
        dep: 'fixed',
      }, 'ii');

      b.headChild(iu, te, 'mark');
      b.headChild(te, mo, 'fixed');
      b.headChild(te, ii, 'fixed');

      b.inOrder(da, to, 1);
      b.inOrder(to, iu, 3);
      b.inOrder(iu, te, 1);
      b.inOrder(te, mo, 1);
      b.inOrder(mo, ii, 1);

      b.captureSpan('といってもいい', da, ii);
    },
    // Pattern 3: っていってもいい (casual variant)
    // e.g., 全然日本語が出来ないっていってもいい
    (b) => {
      const tte = b.particle('って', 'tte');

      const iu = b.verb({
        lemma: 'いう',
        inflectionForm: '連用形-促音便',
      }, 'iu');

      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const mo = b.tok({
        text: 'も',
        pos: 'ADP',
        dep: 'fixed',
      }, 'mo');

      const ii = b.tok({
        lemmaOneOf: ['いい', 'よい'],
        posOneOf: ['AUX', 'ADJ'],
        dep: 'fixed',
      }, 'ii');

      b.headChild(iu, te, 'mark');
      b.headChild(te, mo, 'fixed');
      b.headChild(te, ii, 'fixed');

      b.inOrder(tte, iu, 3);
      b.inOrder(iu, te, 1);
      b.inOrder(te, mo, 1);
      b.inOrder(mo, ii, 1);

      b.captureSpan('といってもいい', tte, ii);
    }
  );
});
