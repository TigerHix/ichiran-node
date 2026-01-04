import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てあげる (te-ageru) - to do something for someone
 *
 * Verb[te-form] + あげる expresses doing an action as a favor for someone else.
 *
 * Examples:
 * - 買ってあげた (bought for someone)
 * - 作ってあげる (will make for someone)
 * - してあげましょう (let's do for someone)
 *
 * The receiver is marked with に, the doer with が.
 *
 * GiNZA parsing notes:
 * - Verb-te-forms are parsed as: verb stem + て (SCONJ)
 * - て has dep=mark, attaching to the verb stem
 * - あげる is the main verb (pos=VERB, lemma=あげる)
 *
 * Forms handled:
 * - Present: てあげる
 * - Past: てあげた
 * - Te-form: てあげて
 * - Polite: てあげます, てあげました
 * - Volitional: てあげましょう
 */
export default linguisticRule('てあげる', (r) => {
  r.either(
    // Pattern 1: Present/past forms (てあげる, てあげた)
    // Example: 作ってあげる, 買ってあげた, してあげた
    // GiNZA: verb + て (SCONJ) + あげる (VERB)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const ageru = b.verb({ lemma: 'あげる' }, 'ageru');

      b.inOrder(te, ageru, 1);
      b.captureSpan('てあげる', te, ageru);
    },

    // Pattern 2: Te-form (てあげて)
    // Example: 貸してあげて, 教えてあげて
    // GiNZA: verb + て (SCONJ) + あげ (lemma) + て (SCONJ)
    (b) => {
      const te1 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te1');
      const age = b.tok({ lemma: 'あげる' }, 'age');
      const te2 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te2');

      b.inOrder(te1, age, 1);
      b.inOrder(age, te2, 1);
      b.captureSpan('てあげる', te1, te2);
    },

    // Pattern 3: Polite forms (てあげます, てあげました)
    // Example: してあげます, してあげました
    // GiNZA: verb + て (SCONJ) + あげます (VERB, lemma=あげる)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const ageru = b.verb({
        lemma: 'あげる',
        inflectionForm: '連用形-一般',
      }, 'ageru');

      b.inOrder(te, ageru, 2);  // Allow distance for polite auxiliaries
      b.captureSpan('てあげる', te, ageru);
    },

    // Pattern 4: Volitional (てあげましょう)
    // Example: してあげましょう, 聞いてあげましょう
    // GiNZA: verb + て (SCONJ) + あげましょう (VERB, lemma=あげる)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const ageru = b.verb({
        lemma: 'あげる',
        inflectionForm: '意志推量形',
      }, 'ageru');

      b.inOrder(te, ageru, 2);
      b.captureSpan('てあげる', te, ageru);
    },

    // Pattern 5: Request (てあげてください)
    // Example: 貸してあげてください, 渡してあげてください
    // GiNZA: verb + て + あげ + て + ください
    (b) => {
      const te1 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te1');
      const age = b.tok({ lemma: 'あげる' }, 'age');
      const te2 = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te2');
      const kudasai = b.verb({ lemma: 'くださる' }, 'kudasai');

      b.inOrder(te1, age, 1);
      b.inOrder(age, te2, 1);
      b.inOrder(te2, kudasai, 1);
      b.captureSpan('てあげる', te1, kudasai);
    }
  );
});
