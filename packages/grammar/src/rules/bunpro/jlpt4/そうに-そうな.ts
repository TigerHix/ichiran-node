import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('そうに-そうな', (r) => {
  // そうに・そうな attaches to verb/adjective stems to express "seeming/appearing"
  // Matches patterns like:
  // - 優しそうに (stem + そう + に) - adverbial form
  // - 楽しそうな (stem + そう + な) - adnominal form
  // - つまらなそうな (negative stem + な + そう + な) - special negative case
  // - わかりやすそうな (stem + aux + そう + な) - compound verb case
  //
  // The stem can be:
  // - i-adjective stem (inflectionForm=語幹-一般)
  // - verb in various forms (連用形, 未然形, etc.)
  // - na-adjective (no inflectionForm)
  //
  // Key: そう (dep=aux) attaches to the stem (possibly via intermediate auxiliaries)

  r.either(
    // Pattern 1: そうに (adverbial - modifies verbs)
    (b) => {
      const stem = b.tok({}, 'stem');
      const sou = b.aux({ lemma: 'そう', dep: 'aux' }, 'sou');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      // sou and ni both attach to stem as aux (possibly via intermediate aux)
      b.auxOf(stem, sou);
      b.auxOf(stem, ni);
      // Order: stem -> sou -> ni (contiguous)
      b.inOrder(stem, sou, 1);
      b.inOrder(sou, ni, 1);
      b.captureSpan('そうに-そうな', stem, ni);
    },
    // Pattern 2: そうな (adnominal - modifies nouns)
    (b) => {
      const stem = b.tok({}, 'stem');
      const sou = b.aux({ lemma: 'そう', dep: 'aux' }, 'sou');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      // sou and na both attach to stem as aux
      b.auxOf(stem, sou);
      b.auxOf(stem, na);
      // Order: stem -> sou -> na (contiguous)
      b.inOrder(stem, sou, 1);
      b.inOrder(sou, na, 1);
      b.captureSpan('そうに-そうな', stem, na);
    },
    // Pattern 3: negative stem + な + そうな (special case for negative i-adj/verbs)
    // e.g., つまらなそうな (from つまらない)
    (b) => {
      const stem = b.tok({}, 'stem');
      const naNai = b.aux({ lemma: 'ない', inflectionForm: '語幹-一般' }, 'naNai');
      const sou = b.aux({ lemma: 'そう', dep: 'aux' }, 'sou');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      // All attach to stem as aux
      b.auxOf(stem, naNai);
      b.auxOf(stem, sou);
      b.auxOf(stem, na);
      // Order: stem -> naNai -> sou -> na (contiguous)
      b.inOrder(stem, naNai, 1);
      b.inOrder(naNai, sou, 1);
      b.inOrder(sou, na, 1);
      b.captureSpan('そうに-そうな', stem, na);
    },
    // Pattern 4: stem + intermediate aux + そうな (compound verb case)
    // e.g., わかりやすそうな (わかる + やす + そう + な)
    (b) => {
      const stem = b.tok({}, 'stem');
      const midAux = b.aux({ inflectionForm: '語幹-一般' }, 'midAux');
      const sou = b.aux({ lemma: 'そう', dep: 'aux' }, 'sou');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      // All attach to stem as aux
      b.auxOf(stem, midAux);
      b.auxOf(stem, sou);
      b.auxOf(stem, na);
      // Order: stem -> midAux -> sou -> na (contiguous)
      b.inOrder(stem, midAux, 1);
      b.inOrder(midAux, sou, 1);
      b.inOrder(sou, na, 1);
      b.captureSpan('そうに-そうな', stem, na);
    }
  );
});
