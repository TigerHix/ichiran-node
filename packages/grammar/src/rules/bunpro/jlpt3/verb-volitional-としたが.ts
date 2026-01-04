import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verb-volitional-としたが', (r) => {
  // Linguistic view:
  // - volitional clause (…よう) + と + した + conjunction
  // - した is the past tense of する (must be past form!)
  // - conjunctions: が, けど, けれど, けれども, たら
  //
  // GiNZA parses した inconsistently:
  // - Single token: "した" (VERB, lemma=する)
  // - Split: "し" (VERB/SCONJ, lemma=する) + "た" (AUX, lemma=た)
  //
  // We need to handle both cases and ensure past tense to avoid matching
  // present tense "する" forms like "開けようとするが"

  r.either(
    // Pattern 1: した as single token, volitional as advcl, conjunction as mark
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 2: した as single token, volitional as advcl, conjunction as advcl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'advcl');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 3: した as single token, volitional as advmod
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'advmod');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 4: した as single token, volitional as obl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 5: した split into し + た, volitional as advcl, し as VERB
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 6: した split into し + た, volitional as advcl, し as VERB, conjunction as advcl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'advcl');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 7: した split into し + た, volitional as obl, し as VERB
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 8: noun with 意志推量形 as obl, した as single token
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 9: サ変 verb with しよう as AUX, した as single token
    (b) => {
      const sahen = b.verb({}, 'sahen');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, sahen, 'advcl');
      const shiyou = b.aux({ lemma: 'する', inflectionForm: '意志推量形' }, 'shiyou');
      b.auxOf(sahen, shiyou);
      const to = b.particle('と', 'to');
      b.caseMarker(sahen, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', sahen);
      b.captureSpan('volitional-としたが', sahen, conj);
    },
    // Pattern 10: NOUN with 意志推量形 as advcl, した split into し + た
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 11: PROPN with 意志推量形 as obl, した split into し + た
    (b) => {
      const vol = b.tok({ pos: 'PROPN', inflectionForm: '意志推量形' }, 'vol');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 12: PROPN with 意志推量形 as obl, した as single token
    (b) => {
      const vol = b.tok({ pos: 'PROPN', inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 13: PROPN with 意志推量形 as advcl, した as single token
    (b) => {
      const vol = b.tok({ pos: 'PROPN', inflectionForm: '意志推量形' }, 'vol');
      const shita = b.verb({ text: 'した', lemma: 'する' }, 'shita');
      b.headChild(shita, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shita, conj, 'mark');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 14: サ変 verb with しよう as AUX, した split into し + た
    (b) => {
      const sahen = b.verb({}, 'sahen');
      const shi = b.verb({ lemma: 'する' }, 'shi');
      b.headChild(shi, sahen, 'advcl');
      const shiyou = b.aux({ lemma: 'する', inflectionForm: '意志推量形' }, 'shiyou');
      b.auxOf(sahen, shiyou);
      const to = b.particle('と', 'to');
      b.caseMarker(sahen, to);
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(shi, ta);
      const conj = b.tok({ textOneOf: ['が', 'けど', 'けれど', 'けれども', 'たら'] }, 'conj');
      b.headChild(shi, conj, 'mark');
      b.captureAs('volitional', sahen);
      b.captureSpan('volitional-としたが', sahen, conj);
    },
    // Pattern 15: したら/けど/けれど as fixed to と particle (conditional forms)
    // Structure: volitional + と + し + た/けれど/etc (all fixed to と)
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const shi = b.tok({ lemma: 'する', dep: 'fixed' }, 'shi');
      b.headChild(to, shi, 'fixed');  // し is fixed to と
      const conj = b.tok({
        textOneOf: ['た', 'が', 'けど', 'けれど', 'けれども', 'たら'],
        dep: 'fixed'
      }, 'conj');
      b.headChild(to, conj, 'fixed');  // conjunction is also fixed to と
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    },
    // Pattern 16: Same as 15 but for NOUN volitional forms
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      const shi = b.tok({ lemma: 'する', dep: 'fixed' }, 'shi');
      b.headChild(to, shi, 'fixed');
      const conj = b.tok({
        textOneOf: ['た', 'が', 'けど', 'けれど', 'けれども', 'たら'],
        dep: 'fixed'
      }, 'conj');
      b.headChild(to, conj, 'fixed');
      b.captureAs('volitional', vol);
      b.captureSpan('volitional-としたが', vol, conj);
    }
  );
});
