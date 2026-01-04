import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ために (tame ni) - "for the sake of", "in order to", "because of"
 *
 * Matches purpose and cause expressions using ために.
 *
 * Structure:
 * - Noun + の + ために (for the sake of / for the purpose of)
 * - Verb (dictionary form) + ために (in order to)
 * - Noun + の + ため (without に - used as noun ending)
 * - Verb + た + ため (past tense + cause)
 *
 * This grammar point has two main uses:
 * 1. Purpose: "for the sake of (A)" or "in order to (A)"
 * 2. Cause: "because of (A)" or "due to (A)"
 *
 * Examples from test data:
 * - 君のためなら何でもするよ (I would do anything for you)
 * - 勉強をするために、親に机を買ってもらった (In order to study, I got my parents to buy me a desk)
 * - 健康のために、嫌いでも少しは野菜を食べたほうがいい (For your health, you'd better eat vegetables even if you don't like them)
 * - 家を買うためにローンを組んだ (I took a loan in order to buy a house)
 * - 骨折したため、明日からしばらく仕事を休みます (Since I broke a bone, I'll take time off work starting tomorrow)
 *
 * GiNZA parse structure:
 * - ため after verb: SCONJ with lemma=ため, dep=mark
 * - ため after noun+の: NOUN with lemma=ため, dep=obl/advcl/root
 * - に after ため(SCONJ): SCONJ with lemma=に, dep=fixed, head=ため
 * - に after ため(NOUN): ADP with lemma=に, dep=case, head=ため
 */
export default linguisticRule('ため-に', (r) => {
  r.either(
    // Pattern 1: Verb + ために (purpose: "in order to")
    // 勉強をするために、家を買うために、明日遊びに行くために
    (b) => {
      const tame = b.tok({ lemma: 'ため', dep: 'mark' }, 'tame');
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(tame, ni, 1);
      b.headChild(tame, ni, 'fixed');
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, tame, 10);
      b.captureSpan('ために', verb, ni);
    },
    // Pattern 2: Noun + の + ために (purpose: "for the sake of")
    // 健康のために、私のために
    (b) => {
      const no = b.particle('の', 'no');
      const tame = b.noun({ lemma: 'ため', dep: 'obl' }, 'tame');
      b.inOrder(no, tame, 10);
      const ni = b.tok({ lemma: 'に', dep: 'case' }, 'ni');
      b.inOrder(tame, ni, 1);
      b.caseMarker(tame, ni);
      b.captureSpan('ために', no, ni);
    },
    // Pattern 3: Noun + の + ため (without に - emphasis on cause)
    // 君のためだよ、お金のためだけに仕事をする
    (b) => {
      const no = b.particle('の', 'no');
      const tame = b.noun({ lemmaOneOf: ['ため', '為'] }, 'tame');
      b.inOrder(no, tame, 10);
      b.captureSpan('のため', no, tame);
    },
    // Pattern 4: Verb + た + ため (past tense cause)
    // 骨折したため、２年前車を買うため
    (b) => {
      const verb = b.verb({}, 'verb');
      const tame = b.noun({ lemma: 'ため', dep: 'obl' }, 'tame');
      b.inOrder(verb, tame, 10);
      b.captureSpan('たため', verb, tame);
    }
  );
});
