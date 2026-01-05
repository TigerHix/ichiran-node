import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('verb-volitionalとする', (r) => {
  // Linguistic view:
  // - volitional clause (…よう/おう) + と + する
  // - する is the main verb (present tense, non-past)
  // - volitional attaches to する as advcl/advmod/obl
  // - と is a case marker attached to the volitional element
  //
  // GiNZA parses volitional forms inconsistently:
  // - 諦めよう: VERB with 意志推量形, dep=advcl
  // - おりよう: VERB with 意志推量形, dep=advmod
  // - みとめよう: VERB with 意志推量形, dep=obl
  // - でよう: NOUN (!) with 意志推量形, dep=obl
  // - すおう: Sometimes NOUN without 意志推量形 attribute
  // - 隠蔽しよう: 隠蔽 is VERB, しよう is AUX with 意志推量形; と attaches to 隠蔽
  //
  // This rule matches present tense とする (not past とした which is a different rule)
  // We must NOT match:
  // - ようとしない (negative - different rule)
  // - ようとしたが (past + conjunction - different rule)
  // - ようとしている (progressive - different pattern)

  r.either(
    // Pattern 1: verb with 意志推量形 as advcl, する as single token
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 2: verb with 意志推量形 as advmod, する as single token
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advmod');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 3: verb with 意志推量形 as obl, する as single token
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 4: noun with 意志推量形 as obl, する as single token
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 5: noun without 意志推量形 as obl (handles すおう, でよ parsed as NOUN)
    (b) => {
      const vol = b.noun({}, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 6: noun without 意志推量形 as advcl
    (b) => {
      const vol = b.noun({}, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 7: noun without 意志推量形 as advmod
    (b) => {
      const vol = b.noun({}, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advmod');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 8: サ変 verb with しよう as AUX, する as single token
    (b) => {
      const sahen = b.verb({}, 'sahen');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, sahen, 'advcl');
      const shiyou = b.aux({ lemma: 'する', inflectionForm: '意志推量形' }, 'shiyou');
      b.auxOf(sahen, shiyou);
      const to = b.particle('と', 'to');
      b.caseMarker(sahen, to);
      b.captureAs('volitional', sahen);
      b.captureSpan('verb-volitionalとする', sahen, suru);
    },
    // Pattern 9: PROPN with 意志推量形 as obl, する as single token
    (b) => {
      const vol = b.tok({ pos: 'PROPN', inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 10: PROPN with 意志推量形 as advcl, する as single token
    (b) => {
      const vol = b.tok({ pos: 'PROPN', inflectionForm: '意志推量形' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 11: PROPN without 意志推量形 as obl
    (b) => {
      const vol = b.tok({ pos: 'PROPN' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    },
    // Pattern 12: PROPN without 意志推量形 as advcl
    (b) => {
      const vol = b.tok({ pos: 'PROPN' }, 'vol');
      const suru = b.verb({ text: 'する', lemma: 'する' }, 'suru');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('verb-volitionalとする', vol, suru);
    }
  );
});
