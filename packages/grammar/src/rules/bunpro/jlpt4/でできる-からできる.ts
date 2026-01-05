import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: でできる・からできる - made of/made from
 *
 * Matches noun + でできる/からできる (made of/made from)
 * Expresses what something is made of or where it's made
 *
 * Structure:
 * - Noun + で + できる/できます/できている/できています (made with/by means of)
 * - Noun + から + できる/できます/できている/できています (made from)
 *
 * Examples:
 * - 木でできている (is made of wood)
 * - 石油からできます (can be made from oil)
 * - ブドウからできている (is made from grapes)
 * - コンクリートでできる家 (houses made of concrete)
 *
 * Negative forms (to avoid):
 * - Just できる alone (potential verb "can do")
 * - で as instrumental case marker (doing something WITH something)
 * - から as source particle (from somewhere)
 *
 * GiNZA parse structure:
 * - 木でできている: 木(noun) + で(particle) + できて(verb/aux) + いる(aux)
 * - 石油からできます: 石油(noun) + から(particle) + でき(verb/aux) + ます(aux)
 */
export default bunproLinguisticRule('でできる-からできる', (r) => {
  r.either(
    // Pattern 1: Noun + でできる (casual) - Accept both VERB and AUX
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('で', 'de');
      b.inOrder(noun, particle, 1);

      const dekiru = b.tok({ lemma: 'できる', posOneOf: ['VERB', 'AUX'] }, 'dekiru');
      b.inOrder(particle, dekiru, 1);

      b.captureSpan('でできる-からできる', noun, dekiru);
    },

    // Pattern 2: Noun + からできる (casual) - Accept both VERB and AUX
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('から', 'kara');
      b.inOrder(noun, particle, 1);

      const dekiru = b.tok({ lemma: 'できる', posOneOf: ['VERB', 'AUX'] }, 'dekiru');
      b.inOrder(particle, dekiru, 1);

      b.captureSpan('でできる-からできる', noun, dekiru);
    },

    // Pattern 3: Noun + でできます (polite)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('で', 'de');
      b.inOrder(noun, particle, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'deki');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(deki, masu);
      b.inOrder(particle, deki, 1);

      b.captureSpan('でできる-からできる', noun, masu);
    },

    // Pattern 4: Noun + からできます (polite)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('から', 'kara');
      b.inOrder(noun, particle, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'deki');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(deki, masu);
      b.inOrder(particle, deki, 1);

      b.captureSpan('でできる-からできる', noun, masu);
    },

    // Pattern 5: Noun + でできている (progressive/state - casual)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('で', 'de');
      b.inOrder(noun, particle, 1);

      const dekite = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'dekite');
      const iru = b.aux({ lemma: 'いる' }, 'iru');
      b.auxOf(dekite, iru);
      b.inOrder(particle, dekite, 1);

      b.captureSpan('でできる-からできる', noun, iru);
    },

    // Pattern 6: Noun + からできている (progressive/state - casual)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('から', 'kara');
      b.inOrder(noun, particle, 1);

      const dekite = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'dekite');
      const iru = b.aux({ lemma: 'いる' }, 'iru');
      b.auxOf(dekite, iru);
      b.inOrder(particle, dekite, 1);

      b.captureSpan('でできる-からできる', noun, iru);
    },

    // Pattern 7: Noun + でできています (progressive/state - polite)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('で', 'de');
      b.inOrder(noun, particle, 1);

      const dekite = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'dekite');
      const imasu = b.aux({ lemma: 'います' }, 'imasu');
      b.auxOf(dekite, imasu);
      b.inOrder(particle, dekite, 1);

      b.captureSpan('でできる-からできる', noun, imasu);
    },

    // Pattern 8: Noun + からできています (progressive/state - polite)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const particle = b.particle('から', 'kara');
      b.inOrder(noun, particle, 1);

      const dekite = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['VERB', 'AUX'] }, 'dekite');
      const imasu = b.aux({ lemma: 'います' }, 'imasu');
      b.auxOf(dekite, imasu);
      b.inOrder(particle, dekite, 1);

      b.captureSpan('でできる-からできる', noun, imasu);
    }
  );
});
