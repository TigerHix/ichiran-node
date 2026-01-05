import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てくれる (te-kureru) - someone does something for me
 *
 * Verb[te-form] + くれる expresses someone doing an action as a favor for the speaker.
 * This is the opposite of てあげる - the subject does something FOR the speaker.
 *
 * Examples:
 * - 買ってくれた (bought for me)
 * - 作ってくれる (will make for me)
 * - してくれました (kindly did for me)
 *
 * The direction is: someone else → speaker/beneficiary
 *
 * GiNZA parsing notes:
 * - Verb-te-forms are parsed as: verb stem + て (SCONJ)
 * - て has dep=mark, attaching to the verb stem
 * - くれる is the main verb (pos=VERB, lemma=くれる)
 *
 * Forms handled:
 * - Present: てくれる
 * - Past: てくれた
 * - Te-form: てくれて
 * - Polite: てくれます, てくれました
 * - Volitional: てくれましょう
 */
export default bunproLinguisticRule('てくれる', (r) => {
  r.either(
    // Pattern 1: Present/past forms (てくれる, てくれた, でくれる, でくれた)
    // Example: 作ってくれる, 買ってくれた, してくれた, 急いでくれた
    // GiNZA: verb + て/で (SCONJ) + くれる (VERB)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const kureru = b.verb({ lemma: 'くれる' }, 'kureru');

      b.inOrder(te, kureru, 1);
      b.captureSpan('てくれる', te, kureru);
    },

    // Pattern 1b: With で (e.g., 急いでくれる)
    (b) => {
      const de = b.tok({ lemma: 'て', text: 'で', pos: 'SCONJ' }, 'de');
      const kureru = b.verb({ lemma: 'くれる' }, 'kureru');

      b.inOrder(de, kureru, 1);
      b.captureSpan('てくれる', de, kureru);
    },

    // Pattern 1c: Imperative form (てくれ, でくれ)
    // Example: 急いでくれ, 買ってくれ, してくれ
    // GiNZA: verb + て/で (SCONJ/SYM) + くれ (VERB, lemma=くれる)
    (b) => {
      const te = b.tok({ posOneOf: ['SCONJ', 'SYM'] }, 'te');
      const kure = b.verb({ lemma: 'くれる' }, 'kure');

      b.inOrder(te, kure, 1);
      b.captureSpan('てくれる', te, kure);
    },

    // Pattern 2: Te-form (てくれて)
    // Example: 貸してくれて, 教えてくれて
    // GiNZA: verb + て (SCONJ) + くれ (lemma) + て (SCONJ)
    (b) => {
      const te1 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te1');
      const kure = b.tok({ lemma: 'くれる' }, 'kure');
      const te2 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te2');

      b.inOrder(te1, kure, 1);
      b.inOrder(kure, te2, 1);
      b.captureSpan('てくれる', te1, te2);
    },

    // Pattern 3: Polite forms (てくれます, てくれました)
    // Example: してくれます, してくれました
    // GiNZA: verb + て (SCONJ) + くれます (VERB, lemma=くれる)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const kureru = b.verb({
        lemma: 'くれる',
        inflectionForm: '連用形-一般',
      }, 'kureru');

      b.inOrder(te, kureru, 2);  // Allow distance for polite auxiliaries
      b.captureSpan('てくれる', te, kureru);
    },

    // Pattern 4: Volitional (てくれましょう)
    // Example: してくれましょう, 聞いてくれましょう
    // GiNZA: verb + て (SCONJ) + くれましょう (VERB, lemma=くれる)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const kureru = b.verb({
        lemma: 'くれる',
        inflectionForm: '意志推量形',
      }, 'kureru');

      b.inOrder(te, kureru, 2);
      b.captureSpan('てくれる', te, kureru);
    }
  );
});
