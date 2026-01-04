import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ついでに (tsui de ni) - "while I'm at it", "take the opportunity to"
 *
 * Matches expressions indicating doing something extra while doing something else.
 *
 * Structure:
 * - Verb (any form) + ついでに
 * - Noun + の + ついでに
 * - ついでに at the beginning of a sentence (connective)
 *
 * This grammar point expresses taking advantage of an opportunity to do something else.
 * It has a casual nuance of "while I'm at it" or "on the occasion of".
 *
 * Examples:
 * - 買い物に行くついでに、郵便局に行ってこの荷物を出しておいてくれる？
 * - 散歩のついでに寄って行ってください。
 * - 給油のついでに、車を点検してもらいました。
 * - 注文のついでに、お水のおかわりもお願いします。
 * - 東京に行ったついでに、ディズニーランドへ行った。
 * - 話のついでに言っておきたいことがあります。
 * - ここまで来たならついでにうちにも寄ってよ。
 * - 友達を家に送ったついでに、床屋に行った。
 * - ついでに君の分も払ってあげるよ。
 * - ついでに君の車も洗ってあげようか？
 *
 * GiNZA parse structure:
 * - ついで may be tokenized as a single adverb (ADV) or noun (NOUN)
 * - に may be a separate particle (ADP) or part of ついでに
 * - The pattern needs to handle both: tokenized as one word or split into two
 */
export default linguisticRule('ついでに', (r) => {
  r.either(
    // Pattern 1: Verb + ついでに (as single token)
    // 行くついでに, 行ったついでに, 来るついでに, 送ったついでに
    (b) => {
      const verb = b.verb({}, 'verb');
      const tsuide = b.adv({ text: 'ついでに' }, 'tsuide');
      b.inOrder(verb, tsuide, 5);
      b.captureSpan('ついでに', verb, tsuide);
    },
    // Pattern 2: Verb + ついで + に (split tokens)
    (b) => {
      const verb = b.verb({}, 'verb');
      const tsuide = b.noun({ lemma: 'ついで' }, 'tsuide');
      const ni = b.particle('に', 'ni');
      b.inOrder(verb, tsuide, 5);
      b.inOrder(tsuide, ni, 1);
      b.captureSpan('ついでに', verb, ni);
    },
    // Pattern 3: Noun + の + ついでに (single token)
    // 散歩のついでに, 給油のついでに, 話のついでに, 注文のついでに
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);
      b.caseMarker(noun, no);
      const tsuide = b.adv({ text: 'ついでに' }, 'tsuide');
      b.inOrder(no, tsuide, 3);
      b.captureSpan('のついでに', noun, tsuide);
    },
    // Pattern 4: Noun + の + ついで + に (split tokens)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);
      b.caseMarker(noun, no);
      const tsuide = b.noun({ lemma: 'ついで' }, 'tsuide');
      const ni = b.particle('に', 'ni');
      b.inOrder(no, tsuide, 3);
      b.inOrder(tsuide, ni, 1);
      b.captureSpan('のついでに', noun, ni);
    },
    // Pattern 5: ついでに at beginning (single token)
    // ついでに君の分も, ついでに君の車も
    (b) => {
      const tsuide = b.adv({ text: 'ついでに' }, 'tsuide');
      b.capture(tsuide);
    },
    // Pattern 6: ついで + に at beginning (split tokens)
    (b) => {
      const tsuide = b.noun({ lemma: 'ついで' }, 'tsuide');
      const ni = b.particle('に', 'ni');
      b.inOrder(tsuide, ni, 1);
      b.captureSpan('ついでに', tsuide, ni);
    }
  );
});
