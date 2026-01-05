import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: というわけではない (to iu wake dewa nai) - It is not that, It doesn't mean that
 *
 * A grammar pattern used to express partial negation or clarification. It means "it is not
 * necessarily the case that (A)" or "it doesn't mean that (A)". The speaker is denying that
 * something is entirely true or is the reason for something.
 *
 * Structure:
 * - Verb (any form) + という + わけ + では/じゃ + ない
 * - I-adjective + という + わけ + では/じゃ + ない
 * - Na-adjective/Noun + (だ) + という + わけ + では/じゃ + ない
 *
 * Variants:
 * - というわけではない (formal, standard)
 * - というわけじゃない (casual)
 * - というわけではありません (polite)
 * - だというわけではない (after noun/na-adj)
 *
 * Examples:
 * - 日本に2年間住んでいたからと言って、日本語を話せるというわけではない。
 *   (Just because I lived in Japan for two years doesn't mean I speak Japanese.)
 * - 嫌いというわけではないけど、単に今は食べたくない。
 *   (It's not that I hate it, I'm simply not hungry at the moment.)
 * - 全てのキノコが毒を持っているというわけではない。
 *   (It is not the case that all mushrooms are poisonous.)
 *
 * Key discriminators:
 * - Follows verbs, adjectives, or nouns
 * - という is the quotative particle combination (often parsed as single token)
 * - わけ means "reason" or "case"
 * - では/じゃ is the particle combination for negative assertion
 * - ない is the negative of ある (to be)
 *
 * GiNZA parse structure:
 * - Often parses という as a single token
 * - May split into と (quotative particle) + いう (verb: to say)
 * - わけ (noun: reason/case)
 * - では (particle combination: de + wa) or じゃ (casual)
 * - ない (auxiliary: negative)
 *
 * Different from:
 * - わけではない (without という) - simpler negation
 * - というものではない (not necessarily - different nuance)
 * - ということだ (that means - positive assertion)
 * - からといって (just because... doesn't mean - concessive)
 */
export default linguisticRule('というわけではない', (r) => {
  r.either(
    // Pattern 1: Combined token という + わけ + では + ない
    // Most common case - という is parsed as single token
    // e.g., 謙遜というわけではない, 嫌いだというわけではない
    (b1) => {
      const toiu = b1.tok({ text: 'という' }, 'toiu');
      const wake = b1.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b1.tok({ text: 'では' }, 'dewa');
      const nai = b1.aux({ lemma: 'ない' }, 'nai');

      b1.inOrder(toiu, wake, 2);
      b1.inOrder(wake, dewa, 2);
      b1.inOrder(dewa, nai, 2);

      b1.captureSpan('というわけではない', toiu, nai);
    },

    // Pattern 1b: Combined token という + わけ + じゃ + ない (casual)
    (b1b) => {
      const toiu = b1b.tok({ text: 'という' }, 'toiu');
      const wake = b1b.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const ja = b1b.tok({ text: 'じゃ' }, 'ja');
      const nai = b1b.aux({ lemma: 'ない' }, 'nai');

      b1b.inOrder(toiu, wake, 2);
      b1b.inOrder(wake, ja, 2);
      b1b.inOrder(ja, nai, 2);

      b1b.captureSpan('というわけではない', toiu, nai);
    },

    // Pattern 1c: Combined token という + わけ + で + は + ない (split dewa into de + wa)
    // For cases where では is parsed as で + は
    (b1c) => {
      const toiu = b1c.tok({ text: 'という' }, 'toiu');
      const wake = b1c.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const de = b1c.tok({ text: 'で' }, 'de');
      const wa = b1c.tok({ text: 'は' }, 'wa');
      const nai = b1c.aux({ lemma: 'ない' }, 'nai');

      b1c.inOrder(toiu, wake, 2);
      b1c.inOrder(wake, de, 2);
      b1c.inOrder(de, wa, 1);
      b1c.inOrder(wa, nai, 1);

      b1c.captureSpan('というわけではない', toiu, nai);
    },

    // Pattern 1d: Combined token という + わけ + は + じゃ + ない (split dewa)
    // For cases where では is parsed as は + じゃ
    (b1d) => {
      const toiu = b1d.tok({ text: 'という' }, 'toiu');
      const wake = b1d.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const ha = b1d.tok({ text: 'は' }, 'ha');
      const ja = b1d.tok({ text: 'じゃ' }, 'ja');
      const nai = b1d.aux({ lemma: 'ない' }, 'nai');

      b1d.inOrder(toiu, wake, 2);
      b1d.inOrder(wake, ha, 2);
      b1d.inOrder(ha, ja, 2);
      b1d.inOrder(ja, nai, 2);

      b1d.captureSpan('というわけではない', toiu, nai);
    },

    // Pattern 1e: Combined token という + わけ + では/じゃ + なく (te-form)
    // e.g., 苦情というわけではなく、提案です。
    (b1e) => {
      const toiu = b1e.tok({ text: 'という' }, 'toiu');
      const wake = b1e.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b1e.tok({ textOneOf: ['では', 'じゃ'] }, 'dewa');
      const naku = b1e.tok({ text: 'なく' }, 'naku');

      b1e.inOrder(toiu, wake, 2);
      b1e.inOrder(wake, dewa, 2);
      b1e.inOrder(dewa, naku, 2);

      b1e.captureSpan('というわけではない', toiu, naku);
    },

    // Pattern 2: Split と + いう + わけ + では/じゃ + ない
    // e.g., some cases where GiNZA splits という
    (b2) => {
      const to = b2.particle('と', 'to');
      const iu = b2.tok({ textOneOf: ['いう', '言う'] }, 'iu');
      const wake = b2.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b2.tok({ textOneOf: ['では', 'じゃ'] }, 'dewa');
      const nai = b2.aux({ lemma: 'ない' }, 'nai');

      b2.inOrder(to, iu, 1);  // Must be adjacent
      b2.inOrder(iu, wake, 2);
      b2.inOrder(wake, dewa, 2);
      b2.inOrder(dewa, nai, 2);

      b2.captureSpan('というわけではない', to, nai);
    },

    // Pattern 3: Noun + だ + いう + わけ + では/じゃ + ない
    // For 嫌いだいうというわけではない - where だいう is the pattern
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'] }, 'noun');
      const da = b3.tok({ text: 'だ' }, 'da');
      const to = b3.particle('と', 'to');
      const iu = b3.tok({ textOneOf: ['いう', '言う'] }, 'iu');
      const wake = b3.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b3.tok({ textOneOf: ['では', 'じゃ'] }, 'dewa');
      const nai = b3.aux({ lemma: 'ない' }, 'nai');

      b3.inOrder(noun, da, 3);
      b3.inOrder(da, to, 3);
      b3.inOrder(to, iu, 1);  // Must be adjacent
      b3.inOrder(iu, wake, 2);
      b3.inOrder(wake, dewa, 2);
      b3.inOrder(dewa, nai, 2);

      b3.captureSpan('というわけではない', noun, nai);
    },

    // Pattern 4: Noun/Adj/Verb + という + わけ + では/じゃ + ない
    // More flexible pattern
    (b4) => {
      const predicate = b4.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const toiu = b4.tok({ text: 'という' }, 'toiu');
      const wake = b4.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b4.tok({ textOneOf: ['では', 'じゃ'] }, 'dewa');
      const nai = b4.aux({ lemma: 'ない' }, 'nai');

      b4.inOrder(predicate, toiu, 10);
      b4.inOrder(toiu, wake, 2);
      b4.inOrder(wake, dewa, 2);
      b4.inOrder(dewa, nai, 2);

      b4.captureSpan('というわけではない', predicate, nai);
    },

    // Pattern 5: Combined token というわけ + ではない/じゃない
    // For cases where multiple tokens combine
    (b5) => {
      const toiuwake = b5.tok({ textOneOf: ['というわけ', 'という訳', 'このわけ', 'この訳'] }, 'toiuwake');
      const dewanai = b5.tok({ textOneOf: ['ではない', 'じゃない'] }, 'dewanai');

      b5.inOrder(toiuwake, dewanai, 3);

      b5.captureSpan('というわけではない', toiuwake, dewanai);
    },

    // Pattern 6: Predicate + というわけ + ではない/じゃない
    (b6) => {
      const predicate = b6.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const toiuwake = b6.tok({ textOneOf: ['というわけ', 'という訳', 'このわけ', 'この訳'] }, 'toiuwake');
      const dewanai = b6.tok({ textOneOf: ['ではない', 'じゃない'] }, 'dewanai');

      b6.inOrder(predicate, toiuwake, 10);
      b6.inOrder(toiuwake, dewanai, 3);

      b6.captureSpan('というわけではない', predicate, dewanai);
    },

    // Pattern 7: Combined token というわけ + ではなく (te-form)
    // e.g., 苦情というわけではなく、提案です。
    (b7) => {
      const toiuwake = b7.tok({ textOneOf: ['このわけ', 'この訳'] }, 'toiuwake');
      const dewanaku = b7.tok({ textOneOf: ['ではなく', 'じゃなく'] }, 'dewanaku');

      b7.inOrder(toiuwake, dewanaku, 3);

      b7.captureSpan('というわけではない', toiuwake, dewanaku);
    },

    // Pattern 8: Ultra-loose - just match the core pattern
    // Matches to + iu + wake + dewa + nai
    (b8) => {
      const to = b8.tok({ text: 'と' }, 'to');
      const iu = b8.tok({ textOneOf: ['いう', '言う', 'ゆう'] }, 'iu');
      const wake = b8.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b8.tok({ textOneOf: ['では', 'じゃ'] }, 'dewa');
      const nai = b8.aux({ lemma: 'ない' }, 'nai');

      b8.inOrder(to, iu, 1);  // Must be adjacent
      b8.inOrder(iu, wake, 5);
      b8.inOrder(wake, dewa, 5);
      b8.inOrder(dewa, nai, 5);

      b8.captureSpan('というわけではない', to, nai);
    },

    // Pattern 9: Skip "iu" entirely - match と + wake + dewa + nai
    // For cases where GiNZA doesn't tokenize いう separately
    (b9) => {
      const to = b9.tok({ text: 'と' }, 'to');
      const wake = b9.tok({ textOneOf: ['わけ', '訳'] }, 'wake');
      const dewa = b9.tok({ textOneOf: ['では', 'じゃ', 'は'] }, 'dewa');
      const nai = b9.aux({ lemma: 'ない' }, 'nai');

      b9.inOrder(to, wake, 3);
      b9.inOrder(wake, dewa, 3);
      b9.inOrder(dewa, nai, 3);

      b9.captureSpan('というわけではない', to, nai);
    }
  );
});
