import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ように～てほしい (want someone to do like/in the way of ~)
 *
 * Expresses desire for someone else to do something in a certain manner or way.
 * Combines ように (like/in the way of) with てほしい (want someone to do).
 *
 * Patterns:
 * 1. Verb/Adj + ように + Verb[て] + ほしい
 *    - 頭がよくなるように勉強してほしい (want you to study so you become smart)
 *    - 日本語を話せるようになってほしい (want you to become able to speak Japanese)
 *
 * 2. Noun + のように + Verb[て] + ほしい
 *    - ウサイン・ボルトのように走ってほしい (want you to run like Usain Bolt)
 *    - 大人のように喋ってほしい (want you to talk like an adult)
 *
 * 3. Question word + ように + Verb[て] + ほしい
 *    - どのように書いてほしい (in what way do you want me to write)
 *
 * Key characteristics:
 * - ように (or のように after nouns) indicates manner/similarity
 * - Followed by verb in te-form + ほしい (want someone to do)
 * - Can include various tenses: ほしい, ほしくない, ほしいです, etc.
 *
 * Grammar structure:
 * - (Verb/Adj/Noun + の) + ように (manner/similarity marker)
 * - Verb in te-form (連用形 + て/で)
 * - ほしい auxiliary (desire for someone's action)
 *
 * GiNZA parses this as:
 * - ように as SCONJ or separate tokens (よう + に particle)
 * - Verb in te-form (VERB/AUX連用形 + SCONJて/で)
 * - ほしい as AUX (lemma=ほしい)
 */
export default linguisticRule('ように-てほしい', (r) => {
  r.either(
    // Pattern 1a: Verb/Adj dictionary form + ように + Verb[て] + ほしい (positive present)
    // e.g., よくなるように勉強してほしい
    // GiNZA: よく(ADV/ADJ) + なる(V) + ように(SCONJ) + 勉強する(NOUN+ AUX) + て(SCONJ) + ほしい(AUX)
    (b1a) => {
      const yoni = b1a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b1a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b1a.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b1a.inOrder(yoni, verb, 10);
      b1a.inOrder(verb, te, 1);
      b1a.inOrder(te, hoshii, 1);
      b1a.captureSpan('ように-てほしい', yoni, hoshii);
    },

    // Pattern 1b: Verb/Adj + よう + に (separate) + Verb[て] + ほしい
    // e.g., same as above but よう and に are separate tokens
    (b1b) => {
      const you = b1b.tok({ text: 'よう' }, 'you');
      const ni = b1b.particle('に', 'ni');
      const verb = b1b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b1b.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b1b.inOrder(you, ni, 1);
      b1b.inOrder(ni, verb, 10);
      b1b.inOrder(verb, te, 1);
      b1b.inOrder(te, hoshii, 1);
      b1b.captureSpan('ように-てほしい', you, hoshii);
    },

    // Pattern 2a: Noun + のように (single token) + Verb[て] + ほしい
    // e.g., ウサイン・ボルトのように走ってほしい
    (b2a) => {
      const no = b2a.particle('の', 'no');
      const yoni = b2a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b2a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b2a.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b2a.inOrder(no, yoni, 1);
      b2a.inOrder(yoni, verb, 10);
      b2a.inOrder(verb, te, 1);
      b2a.inOrder(te, hoshii, 1);
      b2a.captureSpan('ように-てほしい', no, hoshii);
    },

    // Pattern 2b: Noun + の + よう + に (separate) + Verb[て] + ほしい
    // e.g., same as above but よう and に are separate
    (b2b) => {
      const no = b2b.particle('の', 'no');
      const you = b2b.tok({ text: 'よう' }, 'you');
      const ni = b2b.particle('に', 'ni');
      const verb = b2b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b2b.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b2b.inOrder(no, you, 1);
      b2b.inOrder(you, ni, 1);
      b2b.inOrder(ni, verb, 10);
      b2b.inOrder(verb, te, 1);
      b2b.inOrder(te, hoshii, 1);
      b2b.captureSpan('ように-てほしい', no, hoshii);
    },

    // Pattern 3a: ように + suru-verbs (noun+する) + て + ほしい
    // e.g., ように勉強してほしい
    (b3a) => {
      const yoni = b3a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b3a.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b3a.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b3a.inOrder(yoni, verb, 10);
      b3a.inOrder(verb, te, 1);
      b3a.inOrder(te, hoshii, 1);
      b3a.captureSpan('ように-てほしい', yoni, hoshii);
    },

    // Pattern 3b: よう + に (separate) + suru-verbs + て + ほしい
    (b3b) => {
      const you = b3b.tok({ text: 'よう' }, 'you');
      const ni = b3b.particle('に', 'ni');
      const verb = b3b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b3b.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b3b.inOrder(you, ni, 1);
      b3b.inOrder(ni, verb, 10);
      b3b.inOrder(verb, te, 1);
      b3b.inOrder(te, hoshii, 1);
      b3b.captureSpan('ように-てほしい', you, hoshii);
    },

    // Pattern 4a: Noun + のように + suru-verbs + て + ほしい
    // e.g., 大人のように勉強してほしい
    (b4a) => {
      const no = b4a.particle('の', 'no');
      const yoni = b4a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b4a.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b4a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b4a.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b4a.inOrder(no, yoni, 1);
      b4a.inOrder(yoni, verb, 10);
      b4a.inOrder(verb, te, 1);
      b4a.inOrder(te, hoshii, 1);
      b4a.captureSpan('ように-てほしい', no, hoshii);
    },

    // Pattern 4b: Noun + の + よう + に + suru-verbs + て + ほしい
    (b4b) => {
      const no = b4b.particle('の', 'no');
      const you = b4b.tok({ text: 'よう' }, 'you');
      const ni = b4b.particle('に', 'ni');
      const verb = b4b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b4b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b4b.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b4b.inOrder(no, you, 1);
      b4b.inOrder(you, ni, 1);
      b4b.inOrder(ni, verb, 10);
      b4b.inOrder(verb, te, 1);
      b4b.inOrder(te, hoshii, 1);
      b4b.captureSpan('ように-てほしい', no, hoshii);
    },

    // Pattern 5a: ように + Verb[て] + ほしい + です (polite)
    // e.g., ように勉強してほしいです
    (b5a) => {
      const yoni = b5a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b5a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b5a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b5a.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b5a.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b5a.inOrder(yoni, verb, 10);
      b5a.inOrder(verb, te, 1);
      b5a.inOrder(te, hoshii, 1);
      b5a.inOrder(hoshii, desu, 1);
      b5a.captureSpan('ように-てほしい', yoni, desu);
    },

    // Pattern 5b: よう + に + Verb[て] + ほしい + です (polite)
    (b5b) => {
      const you = b5b.tok({ text: 'よう' }, 'you');
      const ni = b5b.particle('に', 'ni');
      const verb = b5b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b5b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b5b.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b5b.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b5b.inOrder(you, ni, 1);
      b5b.inOrder(ni, verb, 10);
      b5b.inOrder(verb, te, 1);
      b5b.inOrder(te, hoshii, 1);
      b5b.inOrder(hoshii, desu, 1);
      b5b.captureSpan('ように-てほしい', you, desu);
    },

    // Pattern 6a: Noun + のように + Verb[て] + ほしい + です (polite)
    // e.g., 大人のように走ってほしいです
    (b6a) => {
      const no = b6a.particle('の', 'no');
      const yoni = b6a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b6a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b6a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b6a.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b6a.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b6a.inOrder(no, yoni, 1);
      b6a.inOrder(yoni, verb, 10);
      b6a.inOrder(verb, te, 1);
      b6a.inOrder(te, hoshii, 1);
      b6a.inOrder(hoshii, desu, 1);
      b6a.captureSpan('ように-てほしい', no, desu);
    },

    // Pattern 6b: Noun + の + よう + に + Verb[て] + ほしい + です (polite)
    (b6b) => {
      const no = b6b.particle('の', 'no');
      const you = b6b.tok({ text: 'よう' }, 'you');
      const ni = b6b.particle('に', 'ni');
      const verb = b6b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b6b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b6b.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b6b.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b6b.inOrder(no, you, 1);
      b6b.inOrder(you, ni, 1);
      b6b.inOrder(ni, verb, 10);
      b6b.inOrder(verb, te, 1);
      b6b.inOrder(te, hoshii, 1);
      b6b.inOrder(hoshii, desu, 1);
      b6b.captureSpan('ように-てほしい', no, desu);
    },

    // Pattern 7a: ように + Verb[て] + ほしくない (negative)
    // e.g., ように負けてほしくない
    (b7a) => {
      const yoni = b7a.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b7a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b7a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b7a.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b7a.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b7a.inOrder(yoni, verb, 10);
      b7a.inOrder(verb, te, 1);
      b7a.inOrder(te, hoshiku, 1);
      b7a.inOrder(hoshiku, nai, 1);
      b7a.captureSpan('ように-てほしい', yoni, nai);
    },

    // Pattern 7b: Noun + のように + Verb[て] + ほしくない (negative)
    // e.g., ブラジルのように負けてほしくない
    (b7b) => {
      const no = b7b.particle('の', 'no');
      const yoni = b7b.tok({ textOneOf: ['ように', '樣に'], pos: 'SCONJ' }, 'yoni');
      const verb = b7b.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b7b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b7b.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b7b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b7b.inOrder(no, yoni, 1);
      b7b.inOrder(yoni, verb, 10);
      b7b.inOrder(verb, te, 1);
      b7b.inOrder(te, hoshiku, 1);
      b7b.inOrder(hoshiku, nai, 1);
      b7b.captureSpan('ように-てほしい', no, nai);
    },

    // Pattern 7c: Noun + の + よう + に (separate) + Verb[て] + ほしくない (negative)
    // e.g., ブラジルのように負けてほしくない (when parsed as separate tokens)
    (b7c) => {
      const no = b7c.particle('の', 'no');
      const you = b7c.tok({ text: 'よう' }, 'you');
      const ni = b7c.particle('に', 'ni');
      const verb = b7c.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b7c.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'PART'] }, 'te');
      const hoshiku = b7c.tok({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b7c.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b7c.inOrder(no, you, 1);
      b7c.inOrder(you, ni, 1);
      b7c.inOrder(ni, verb, 10);
      b7c.inOrder(verb, te, 1);
      b7c.inOrder(te, hoshiku, 1);
      b7c.inOrder(hoshiku, nai, 1);
      b7c.captureSpan('ように-てほしい', no, nai);
    }
  );
});
