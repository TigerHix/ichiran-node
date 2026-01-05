import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('がほしい', (r) => {
  // Noun + が + ほしい (I want Noun)
  // Examples: 機会がほしい, お金がほしい, 犬がほしい
  //
  // GiNZA parsing:
  // - Noun: pos=NOUN/PROPN/PRON, dep=nsubj/obj/obl, points to ほしい
  // - が: pos=ADP, lemma=が, dep=case, points to noun
  // - ほしい: pos=ADJ, lemma=ほしい, dep=root
  //
  // Variations:
  // - Positive: ほしい (present), ほしかった (past)
  // - Negative: ほしくない, ほしくなかった
  // - Polite: ほしいです, ほしくないです, ほしかったです
  // - With verb: ほしくなった (became to want - ほしい + なる)

  r.either(
    // Branch 1: Present positive (ほしい)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshii = b.adj({ lemma: 'ほしい', inflectionForm: '終止形-一般' }, 'hoshii');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshii, 1);
      b.captureSpan('がほしい', ga, hoshii);
    },

    // Branch 2: Past positive (ほしかった)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshikatta = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-促音便', '終止形-一般'],
      }, 'hoshikatta');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshikatta, 1);
      b.auxOf(hoshikatta, ta);
      b.captureSpan('がほしい', ga, ta);
    },

    // Branch 3: Present negative (ほしくない)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshiku = b.adj({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshiku, 1);
      b.inOrder(hoshiku, nai, 1);
      b.captureSpan('がほしい', ga, nai);
    },

    // Branch 4: Polite form (～ほしいです)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshii = b.adj({ lemma: 'ほしい' }, 'hoshii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshii, 1);
      b.auxOf(hoshii, desu);
      b.captureSpan('がほしい', ga, desu);
    },

    // Branch 5: Polite negative (～ほしくないです)
    // Structure: noun + が + ほしく + ない + です
    // Note: です attaches to ない, not to ほしい
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshiku = b.adj({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b.adj({ lemma: 'ない' }, 'nai');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshiku, 1);
      b.inOrder(hoshiku, nai, 1);
      b.auxOf(nai, desu);
      b.captureSpan('がほしい', ga, desu);
    },

    // Branch 6: With なる (ほしくなる - to become desirous)
    // Structure: noun + が + ほしい(連用形) + なる
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshiku = b.adj({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const naru = b.verb({ lemma: 'なる' }, 'naru');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshiku, 1);
      b.inOrder(hoshiku, naru, 1);
      b.captureSpan('がほしい', ga, naru);
    },

    // Branch 7: With なった (ほしくなった - became desirous)
    // Structure: noun + が + ほしい(連用形) + なった
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obj', 'obl'],
      }, 'noun');
      const ga = b.particle('が', 'ga');
      const hoshiku = b.adj({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const naru = b.verb({ lemma: 'なる', inflectionForm: '連用形-促音便' }, 'naru');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.caseMarker(noun, ga);
      b.inOrder(ga, hoshiku, 1);
      b.inOrder(hoshiku, naru, 1);
      b.auxOf(naru, ta);
      b.captureSpan('がほしい', ga, ta);
    }
  );
});
