import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('-ようとしない', (r) => {
  // Linguistic view:
  // - main predicate is する (ROOT or advcl)
  // - negation is AUX ない attached as aux
  // - volitional clause (…よう) attaches to suru as advcl/advmod/obl
  // - と is a case marker attached to the volitional element
  //
  // GiNZA parses volitional forms inconsistently:
  // - 諦めよう: VERB with 意志推量形, dep=advcl
  // - おりよう: VERB with 意志推量形, dep=advmod
  // - みとめよう: VERB with 意志推量形, dep=obl
  // - でよう: NOUN (!) with 意志推量形, dep=obl
  // - 隠蔽しよう: 隠蔽 is VERB, しよう is AUX with 意志推量形; と attaches to 隠蔽
  const suru = r.verb({ lemma: 'する' }, 'suru');
  const nai = r.aux({ lemma: 'ない' }, 'nai');
  r.auxOf(suru, nai);

  r.either(
    // Pattern 1: verb with 意志推量形 as advcl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      b.headChild(suru, vol, 'advcl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('ようとしない', vol, nai);
    },
    // Pattern 2: verb with 意志推量形 as advmod
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      b.headChild(suru, vol, 'advmod');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('ようとしない', vol, nai);
    },
    // Pattern 3: verb with 意志推量形 as obl
    (b) => {
      const vol = b.verb({ inflectionForm: '意志推量形' }, 'vol');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('ようとしない', vol, nai);
    },
    // Pattern 4: noun with 意志推量形 as obl (e.g., でよう parsed as NOUN)
    (b) => {
      const vol = b.noun({ inflectionForm: '意志推量形' }, 'vol');
      b.headChild(suru, vol, 'obl');
      const to = b.particle('と', 'to');
      b.caseMarker(vol, to);
      b.captureAs('volitional', vol);
      b.captureSpan('ようとしない', vol, nai);
    },
    // Pattern 5: サ変 verb (e.g., 隠蔽, 復習) with しよう as AUX
    // Structure: 隠蔽(VERB) --advcl--> suru, しよう(AUX) --aux--> 隠蔽
    (b) => {
      const sahen = b.verb({}, 'sahen');
      b.headChild(suru, sahen, 'advcl');
      const shiyou = b.aux({ lemma: 'する', inflectionForm: '意志推量形' }, 'shiyou');
      b.auxOf(sahen, shiyou);
      const to = b.particle('と', 'to');
      b.caseMarker(sahen, to);
      b.captureAs('volitional', sahen);
      b.captureSpan('ようとしない', sahen, nai);
    }
  );
});

