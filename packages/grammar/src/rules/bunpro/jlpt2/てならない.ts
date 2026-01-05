import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: てならない (te naranai) - "can't help but, irresistibly"
 *
 * A grammar pattern expressing that an emotion, feeling, or sensation is so
 * intense that one cannot help but feel it. The feeling arises spontaneously
 * and uncontrollably, often continuing over time rather than being momentary.
 *
 * Structure:
 * - Verb[te-form] + ならない
 * - I-adjective[te-form] + ならない
 * - Na-adjective + で + ならない
 *
 * Examples:
 * - 暑くてならない
 *   (It's extremely hot - I can't help but feel it's hot.)
 * - 心配でならない
 *   (I can't help but worry.)
 * - 会いたくてならない
 *   (I can't help wanting to see you.)
 *
 * Key discriminators:
 * - なら is the negative stem of なる (to become)
 * - ない is the negative auxiliary
 * - The て/で form indicates the cause/state that leads to the uncontrollable feeling
 * - Used primarily for emotions, feelings, and sensations
 * - Different from てしょうがない (similar but more colloquial)
 * - Different from てたまらない (unbearable intensity, not for thinking/feeling verbs)
 *
 * GiNZA parse structure:
 * - Verb te-form: VERB with inflectionForm=連用形-一般 or text ending in て
 * - I-adj te-form: ADJ with text ending in くて
 * - Na-adj: NOUN/ADJ followed by で (ADP)
 * - なら: VERB with lemma=なる, inflectionForm=未然形-一般
 * - ない: AUX/VERB with lemma=ない
 */
export default linguisticRule('てならない', (r) => {
  r.either(
    // Pattern 1: Verb[te-form] + なら + ない
    // e.g., むかついてならない、思い出されてならない
    (b1) => {
      const verbTe = b1.tok({
        posOneOf: ['VERB', 'AUX'],
        textOneOf: ['て', 'いて', 'して', 'えて', 'されて', 'われて'],
      }, 'verbTe');
      const nara = b1.verb({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');
      const nai = b1.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB', 'ADJ'],
      }, 'nai');

      b1.inOrder(verbTe, nara, 10);
      b1.inOrder(nara, nai, 1);
      b1.captureSpan('てならない', verbTe, nai);
    },

    // Pattern 2: Verb[て] + なら + ない (wider matching for verb te-forms)
    // Matches any verb in te-form + naranai
    (b2) => {
      const verbTe = b2.tok({
        posOneOf: ['VERB', 'AUX'],
        text: 'て',
      }, 'verbTe');
      const nara = b2.tok({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');
      const nai = b2.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB', 'ADJ'],
      }, 'nai');

      b2.inOrder(verbTe, nara, 10);
      b2.inOrder(nara, nai, 1);
      b2.captureSpan('てならない', verbTe, nai);
    },

    // Pattern 3: I-adjective[te-form] + なら + ない
    // e.g., 暑くてならない、寂しくてならない
    (b3) => {
      const adjTe = b3.tok({
        pos: 'ADJ',
        text: 'くて',
      }, 'adjTe');
      const nara = b3.tok({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');
      const nai = b3.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB', 'ADJ'],
      }, 'nai');

      b3.inOrder(adjTe, nara, 10);
      b3.inOrder(nara, nai, 1);
      b3.captureSpan('てならない', adjTe, nai);
    },

    // Pattern 4: Na-adjective + で + なら + ない
    // e.g., 残念でならない、心配でならない
    (b4) => {
      const naAdj = b4.tok({
        posOneOf: ['NOUN', 'ADJ', 'PROPN'],
      }, 'naAdj');
      const de = b4.particle('で', 'de');
      const nara = b4.tok({
        lemma: 'なる',
        inflectionForm: '未然形-一般',
      }, 'nara');
      const nai = b4.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB', 'ADJ'],
      }, 'nai');

      b4.inOrder(naAdj, de, 2);
      b4.inOrder(de, nara, 10);
      b4.inOrder(nara, nai, 1);
      b4.captureSpan('てならない', naAdj, nai);
    },

    // Pattern 5: Polite form - Na-adj + で + なり + ません
    // e.g., 残念でなりません、心配でなりません
    (b5) => {
      const naAdj = b5.tok({
        posOneOf: ['NOUN', 'ADJ', 'PROPN'],
      }, 'naAdj');
      const de = b5.particle('で', 'de');
      const nari = b5.tok({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');
      const masen = b5.tok({
        text: 'ません',
        lemma: 'ます',
      }, 'masen');

      b5.inOrder(naAdj, de, 2);
      b5.inOrder(de, nari, 10);
      b5.inOrder(nari, masen, 1);
      b5.captureSpan('てならない', naAdj, masen);
    },

    // Pattern 6: I-adj/verb te-form + なり + ません (polite)
    (b6) => {
      const teForm = b6.tok({
        textOneOf: ['て', 'くて'],
        posOneOf: ['VERB', 'AUX', 'ADJ'],
      }, 'teForm');
      const nari = b6.tok({
        lemma: 'なる',
        inflectionForm: '連用形-一般',
      }, 'nari');
      const masen = b6.tok({
        text: 'ません',
        lemma: 'ます',
      }, 'masen');

      b6.inOrder(teForm, nari, 10);
      b6.inOrder(nari, masen, 1);
      b6.captureSpan('てならない', teForm, masen);
    },

    // Pattern 7: Wider pattern - any te/de form + nara/nari + nai/masen
    // Catches all remaining cases
    (b7) => {
      const teForm = b7.tok({
        textOneOf: ['て', 'くて', 'で'],
        posOneOf: ['VERB', 'AUX', 'ADJ', 'SCONJ', 'ADP'],
      }, 'teForm');
      const naru = b7.tok({
        lemma: 'なる',
      }, 'naru');
      const ending = b7.tok({
        textOneOf: ['ない', 'ません', 'ませんでした'],
        lemmaOneOf: ['ない', 'ます'],
      }, 'ending');

      b7.inOrder(teForm, naru, 3);
      b7.inOrder(naru, ending, 3);
      b7.captureSpan('てならない', teForm, ending);
    }
  );
});
