import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: こととて (kototote) - "due to, since, on account of"
 *
 * A formal, old-fashioned particle-based expression meaning "on account of (A), (B)".
 * Used to give a reason, often in apologies or explanations.
 *
 * Structure:
 * - Noun + の + こと + とて
 * - Verb (negative form, often with ぬ) + こと + とて
 * - I-adjective + こと + とて
 * - Adverb + の + こと + とて
 *
 * Examples:
 * - 新人のこととて、まだ分からないことがたくさんあります。
 *   (Since I am still new, there are many things I don't know.)
 * - 慣れないこととて、間違いをしてしまいました。
 *   (Due to not being accustomed to it, I made a mistake.)
 * - 知らぬこととて、失礼をお許しください。
 *   (Please forgive my rudeness due to my lack of knowledge.)
 *
 * Key discriminators:
 * - こと is a noun (NOUN) meaning "thing, matter"
 * - とて is parsed as NOUN (not ADP/PART) by GiNZA
 * - Formal/literary register
 * - Often used with negative verbs (ない, ぬ)
 * - Often followed by apologies or explanations
 *
 * GiNZA parse structure:
 * - Noun + の(ADP) + こと(NOUN) + とて(NOUN, dep=aux)
 * - Verb + こと(NOUN) + とて(NOUN, dep=aux)
 * - Adv + の(ADP) + こと(NOUN) + とて(NOUN, dep=aux)
 *
 * Different from:
 * - ことだから (given that, more predictive)
 * - ことだし (and so, more conversational)
 * - だから (therefore, not formal/archaic)
 */
export default bunproLinguisticRule('こととて', (r) => {
  r.either(
    // Pattern 1: Noun + の + こと + とて (most common pattern)
    // Example: 新人のこととて、まだ分からないことがたくさんあります。
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b1.particle('の', 'no');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b1.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      b1.inOrder(noun, no, 1);
      b1.inOrder(no, koto, 1);
      b1.inOrder(koto, tote, 1);

      b1.captureSpan('こととて', noun, tote);
    },

    // Pattern 2: Verb (+ aux) + こと + とて
    // Example: 慣れないこととて、仕事になれぬこととて、子供がやったこととて
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b2.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      // Allow multiple tokens between verb and こと (for auxiliaries like ない, ぬ, た)
      b2.inOrder(verb, koto, 5);
      b2.inOrder(koto, tote, 1);

      b2.captureSpan('こととて', verb, tote);
    },

    // Pattern 2b: I-adjective + こと + とて
    // Example: 耳が生まれつき悪いこととて
    (b2b) => {
      const adj = b2b.adj({}, 'adj');
      const koto = b2b.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b2b.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      b2b.inOrder(adj, koto, 1);
      b2b.inOrder(koto, tote, 1);

      b2b.captureSpan('こととて', adj, tote);
    },

    // Pattern 3: Adverb + の + こと + とて
    // Example: 初めてのこととて、突然のこととて
    (b3) => {
      const adv = b3.tok({ posOneOf: ['ADV', 'NOUN', 'PROPN', 'PRON'] }, 'adv');
      const no = b3.particle('の', 'no');
      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b3.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      b3.inOrder(adv, no, 1);
      b3.inOrder(no, koto, 1);
      b3.inOrder(koto, tote, 1);

      b3.captureSpan('こととて', adv, tote);
    },

    // Pattern 4: とのこと + とて (quoted clause + こと + とて)
    // Example: 担当者が席を外しているとのこととて、後で掛け直させて下さい。
    (b4) => {
      const to = b4.particle('と', 'to');
      const no = b4.particle('の', 'no');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b4.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      b4.inOrder(to, no, 1);
      b4.inOrder(no, koto, 1);
      b4.inOrder(koto, tote, 1);

      // Capture from との
      b4.captureSpan('こととて', to, tote);
    },

    // Pattern 5: Any token + の + こと + とて (loosest pattern for quoted forms)
    (b5) => {
      const prev = b5.tok({}, 'prev');
      const no = b5.particle('の', 'no');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const tote = b5.tok({ text: 'とて', lemma: 'とて', posOneOf: ['NOUN', 'ADP', 'AUX', 'PART'] }, 'tote');

      b5.inOrder(prev, no, 1);
      b5.inOrder(no, koto, 1);
      b5.inOrder(koto, tote, 1);

      b5.captureSpan('こととて', prev, tote);
    }
  );
});
