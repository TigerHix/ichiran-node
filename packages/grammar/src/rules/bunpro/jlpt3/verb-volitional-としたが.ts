import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verb-volitional-としたが', (r) => {
  // Linguistic view:
  // - volitional clause (…よう) + と + した + conjunction
  // - した is the past tense of する (must be past form!)
  // - conjunctions: が, けど, けれど, けれども, たら
  //
  // Simplified pattern matching approach based on -ようとしない rule

  r.either(
    // Pattern 1: verb with 意志推量形 as advcl, ga as mark
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, ga);
    },
    // Pattern 2: verb with 意志推量形 as advcl, ga as advcl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'advcl');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, ga);
    },
    // Pattern 3: verb with 意志推量形 as advmod
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advmod');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, ga);
    },
    // Pattern 4: verb with 意志推量形 as obl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, ga);
    },
    // Pattern 5: noun with 意志推量形 as obl
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, ga);
    },
    // Pattern 6: サ変 verb with しよう as AUX
    (b) => {
      const sahen = b.verb({}, 'sahen');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      b.headChild(suru, sahen, 'advcl');
      const shiyou = b.aux({ lemma: 'する', inflectionForm: '意志推量形' }, 'shiyou');
      b.auxOf(sahen, shiyou);
      const to = b.particle('と', 'to');
      b.caseMarker(sahen, to);
      const ga = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'ga');
      b.headChild(suru, ga, 'mark');
      b.captureAs('volitional', sahen);
      b.captureSpan('volitional-としたが', sahen, ga);
    }
  );
});
