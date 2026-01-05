import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ないではいられない (nai dewa irarenai) - "cannot help but do, irresistibly"
 *
 * A double negative expression meaning "can't help but do something" or "can't resist doing".
 * This expresses that the speaker cannot control the urge to do something - the action
 * happens uncontrollably or irresistibly. It's the more colloquial version of ずにはいられない.
 *
 * Structure:
 * - Verb［ない］(negative form) + では + いられない (casual)
 * - Verb［ない］(negative form) + じゃ + いられない (casual contraction)
 * - Verb［ない］(negative form) + では + いられません (polite)
 * - Verb［ない］(negative form) + じゃ + いられません (polite contraction)
 * - Past tense: いられなかった, いられませんでした
 *
 * Examples:
 * - セール品を見ると、買わないではいられなくなる。
 *   (When I see something on sale, I can't help but buy it.)
 * - 私は辛い物を食べると、牛乳を飲まないではいられないの。
 *   (When I eat something spicy, I can't help but drink milk.)
 * - ペットのワンちゃんが自分の尻尾を追いかけているのを見るとおかしくて、笑わないではいられない。
 *   (When I see my pet dog chasing his tail, it's so funny I can't help but laugh.)
 * - 大親友が事故でなくなったときは、泣かないではいられなかった。
 *   (When my best friend died, I couldn't help but cry.)
 * - おばあさんが困っているのを見ると助けないではいられない。
 *   (When I see an old lady in need, I can't resist helping them.)
 *
 * Key discriminators:
 * - Uses negative verb form (ない) instead of classical negative stem (ず)
 * - More colloquial/spoken than ずにはいられない
 * - では/じゃ is a conjunction particle (not locative)
 * - Different from てはいられない (can't afford to) - that uses te-form
 * - Different from simple negation with いられない (can't stay in a state)
 *
 * GiNZA parse structure:
 * - Verb[negative] + ない (AUX, lemma=ない, dep=aux)
 * - では/じゃ can be:
 *   - Single token (SCONJ) with text='では' or 'じゃ'
 *   - Two tokens: で/じゃ (ADP/SCONJ) + は (ADP/PART)
 * - い (VERB, lemma=いる)
 * - られ (AUX, lemma=られる, dep=aux)
 * - ない (AUX, lemma=ない, dep=aux) for casual form
 * - ませ (AUX, lemma=ます) + ん (AUX, lemma=ぬ) for polite form
 *
 * Related patterns:
 * - ずにはいられない (JLPT3) - same meaning, more formal
 * - てはいられない (JLPT2) - "can't afford to" (different meaning)
 * - ざるを得ない (JLPT2) - "have no choice but to" (more objective)
 */
export default bunproLinguisticRule('ないではいられない', (r) => {
  r.either(
    // Branch 1: では/じゃ as single SCONJ token + casual form
    // verb[nai] + では + い + られ + ない
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const dewa = b.tok({
        textOneOf: ['では', 'じゃ'],
        pos: 'SCONJ',
      }, 'dewa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
        dep: 'aux',
      }, 'rare');
      const nai2 = b.aux({
        lemma: 'ない',
        dep: 'aux',
      }, 'nai2');

      b.auxOf(verb, nai);
      b.inOrder(nai, dewa, 5);
      b.inOrder(dewa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, nai2);

      b.captureSpan('ないではいられない', nai, nai2);
    },

    // Branch 2: で/じゃ + は as separate tokens + casual form
    // verb[nai] + で/じゃ + は + い + られ + ない
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const de = b.tok({
        textOneOf: ['で', 'じゃ'],
        posOneOf: ['ADP', 'SCONJ'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
        dep: 'aux',
      }, 'rare');
      const nai2 = b.aux({
        lemma: 'ない',
        dep: 'aux',
      }, 'nai2');

      b.auxOf(verb, nai);
      b.inOrder(nai, de, 5);
      b.inOrder(de, wa, 2);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, nai2);

      b.captureSpan('ないではいられない', nai, nai2);
    },

    // Branch 3: では/じゃ as single SCONJ token + polite form
    // verb[nai] + では + い + られ + ませ + ん
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const dewa = b.tok({
        textOneOf: ['では', 'じゃ'],
        pos: 'SCONJ',
      }, 'dewa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const mase = b.aux({
        lemma: 'ます',
      }, 'mase');
      const n = b.aux({
        lemma: 'ぬ',
        text: 'ん',
      }, 'n');

      b.auxOf(verb, nai);
      b.inOrder(nai, dewa, 5);
      b.inOrder(dewa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, mase);
      b.auxOf(iru, n);

      b.captureSpan('ないではいられない', nai, n);
    },

    // Branch 4: で/じゃ + は as separate tokens + polite form
    // verb[nai] + で/じゃ + は + い + られ + ませ + ん
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const de = b.tok({
        textOneOf: ['で', 'じゃ'],
        posOneOf: ['ADP', 'SCONJ'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const mase = b.aux({
        lemma: 'ます',
      }, 'mase');
      const n = b.aux({
        lemma: 'ぬ',
        text: 'ん',
      }, 'n');

      b.auxOf(verb, nai);
      b.inOrder(nai, de, 5);
      b.inOrder(de, wa, 2);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, mase);
      b.auxOf(iru, n);

      b.captureSpan('ないではいられない', nai, n);
    },

    // Branch 5: Casual with られない parsed as single auxiliary + では/じゃ single token
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const dewa = b.tok({
        textOneOf: ['では', 'じゃ'],
        pos: 'SCONJ',
      }, 'dewa');
      const irarenai = b.aux({
        lemma: 'いられる',
      }, 'irarenai');
      const nai2 = b.aux({
        lemma: 'ない',
      }, 'nai2');

      b.auxOf(verb, nai);
      b.inOrder(nai, dewa, 5);
      b.inOrder(dewa, irarenai, 5);
      b.auxOf(irarenai, nai2);

      b.captureSpan('ないではいられない', nai, nai2);
    },

    // Branch 6: Casual with られない parsed as single auxiliary + で/じゃ + は separate
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const de = b.tok({
        textOneOf: ['で', 'じゃ'],
        posOneOf: ['ADP', 'SCONJ'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const irarenai = b.aux({
        lemma: 'いられる',
      }, 'irarenai');
      const nai2 = b.aux({
        lemma: 'ない',
      }, 'nai2');

      b.auxOf(verb, nai);
      b.inOrder(nai, de, 5);
      b.inOrder(de, wa, 2);
      b.inOrder(wa, irarenai, 5);
      b.auxOf(irarenai, nai2);

      b.captureSpan('ないではいられない', nai, nai2);
    },

    // Branch 7: More flexible parsing for casual form
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const de = b.tok({
        textOneOf: ['で', 'じゃ'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const nai2 = b.aux({
        lemma: 'ない',
      }, 'nai2');

      b.inOrder(verb, nai, 3);
      b.inOrder(nai, de, 5);
      b.inOrder(de, wa, 2);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, nai2);

      b.captureSpan('ないではいられない', nai, nai2);
    },

    // Branch 8: More flexible parsing for polite form
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        text: 'ない',
      }, 'nai');
      const de = b.tok({
        textOneOf: ['で', 'じゃ'],
      }, 'de');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const mase = b.aux({
        lemma: 'ます',
      }, 'mase');
      const n = b.aux({
        lemmaOneOf: ['ぬ', 'ない'],
        textOneOf: ['ん', 'ない'],
      }, 'n');

      b.inOrder(verb, nai, 3);
      b.inOrder(nai, de, 5);
      b.inOrder(de, wa, 2);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, mase);
      b.auxOf(iru, n);

      b.captureSpan('ないではいられない', nai, n);
    }
  );
});
