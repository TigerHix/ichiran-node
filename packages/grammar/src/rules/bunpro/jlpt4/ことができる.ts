import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ことができる - can do / be able to
 *
 * Matches verb + ことができる / ことができます (potential / possibility)
 * Also matches noun + ができる / ができます (without こと)
 *
 * Structure:
 * - Verb［dictionary form］+ こと + が + できる/できます (casual/polite)
 * - Noun + が + できる/できます (for suru-verbs)
 *
 * Examples:
 * - 馬に乗ることができる (can ride a horse)
 * - 日本語を読むことができる (can read Japanese)
 * - 運転ができる (can drive)
 * - 泳ぐことができますか (can you swim?)
 *
 * Negative forms:
 * - ことができない (cannot do)
 * - ことができません (cannot do - polite)
 * - ことができなくて (unable to do - conjunctive)
 *
 * Past forms:
 * - ことができた (was able to do)
 * - ことができました (was able to do - polite)
 *
 * GiNZA parse structure:
 * - 乗ることができる: 乗る(verb) + こと(noun) + が(particle) + できる(verb)
 * - 運転ができる: 運転(noun) + が(particle) + できる(verb)
 * - できません: でき(verb) + ません(aux)
 */
export default linguisticRule('ことができる', (r) => {
  r.either(
    // Pattern 1: Verb dictionary form + ことができる (casual)
    // Verb is in 連体形-一般 (attributive) before こと, and できる is parsed as AUX
    // Note: Verb can be either VERB or AUX (for suru-verbs like 提出する)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // できる after こと is parsed as AUX, not VERB
      const dekiru = b.tok({ lemma: 'できる', posOneOf: ['AUX', 'VERB'] }, 'dekiru');
      b.inOrder(ga, dekiru, 1);

      b.captureSpan('ことができる', verb, dekiru);
    },

    // Pattern 2: Verb dictionary form + ことができます (polite)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      // でき is AUX (連用形), ます is AUX
      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(deki, masu);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, masu);
    },

    // Pattern 3: Noun + ができる (casual)
    // Here できる is parsed as VERB (standalone), not AUX
    (b) => {
      const noun = b.noun({}, 'noun');
      const ga = b.particle('が', 'ga');
      b.inOrder(noun, ga, 1);

      const dekiru = b.verb({ lemma: 'できる' }, 'dekiru');
      b.inOrder(ga, dekiru, 1);

      b.captureSpan('ことができる', noun, dekiru);
    },

    // Pattern 4: Noun + ができます (polite)
    (b) => {
      const noun = b.noun({}, 'noun');
      const ga = b.particle('が', 'ga');
      b.inOrder(noun, ga, 1);

      const deki = b.verb({ lemma: 'できる', inflectionForm: '連用形-一般' }, 'deki');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(deki, masu);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', noun, masu);
    },

    // Pattern 5: Verb dictionary form + ことができない (negative casual)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '未然形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(deki, nai);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, nai);
    },

    // Pattern 6: Verb dictionary form + ことができません (negative polite)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '未然形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const masen = b.aux({ lemma: 'ません' }, 'masen');
      b.auxOf(deki, masen);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, masen);
    },

    // Pattern 7: Verb dictionary form + ことができなくて (conjunctive negative)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const nakute = b.aux({ lemma: 'なくて' }, 'nakute');
      b.auxOf(deki, nakute);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, nakute);
    },

    // Pattern 8: Verb dictionary form + ことができた (past casual)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(deki, ta);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, ta);
    },

    // Pattern 9: Verb dictionary form + ことができました (past polite)
    (b) => {
      const verb = b.tok({ inflectionForm: '連体形-一般', posOneOf: ['VERB', 'AUX'] }, 'verb');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const deki = b.tok({ lemma: 'できる', inflectionForm: '連用形-一般', posOneOf: ['AUX', 'VERB'] }, 'deki');
      const mashita = b.aux({ lemma: 'ました' }, 'mashita');
      b.auxOf(deki, mashita);
      b.inOrder(ga, deki, 1);

      b.captureSpan('ことができる', verb, mashita);
    }
  );
});
