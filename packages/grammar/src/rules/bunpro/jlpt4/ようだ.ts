import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ようだ', (r) => {
  // ようだ/ようです expresses "seems like" or "appears to be" based on objective/visual evidence
  // Pattern: plain form + ようだ/ようです (for verbs/i-adj)
  //          noun + の + ようだ/ようです
  //          na-adj + な + ようだ/ようです
  //
  // GiNZA parses this as:
  // - よう (AUX, pos=AUX or shape詞-助動詞語幹, lemma=よう, dep=aux or cop or root)
  // - だ (AUX, lemma=だ, dep=aux or fixed)
  //
  // Forms:
  // - ようだ (plain casual)
  // - ようです (polite)
  // - のようだ (after noun)
  // - なようだ (after na-adjective)

  r.either(
    // Pattern 1: Verb/i-adj + ようだ (aux attaches to predicate)
    (b) => {
      const pred = b.tok({}, 'pred');
      const you = b.aux({ lemma: 'よう', dep: 'aux' }, 'you');
      // Use text constraint to avoid matching ように (連用形-ニ)
      // ようだ has text=だ, ように has text=に (both lemma=だ)
      const da = b.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b.auxOf(pred, you);
      b.auxOf(pred, da);
      b.inOrder(you, da, 1);

      b.captureSpan('ようだ', you, da);
    },

    // Pattern 2: Noun + の + ようだ (copula pattern)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      const you = b.tok({ lemma: 'よう', posOneOf: ['AUX', 'ADJ'] }, 'you');
      // Use text constraint to avoid matching ように
      const da = b.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b.inOrder(noun, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, da, 1);

      b.captureSpan('ようだ', no, da);
    },

    // Pattern 3: Na-adj + な + ようだ (copula pattern)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      const you = b.aux({ lemma: 'よう' }, 'you');
      // Use text constraint to avoid matching ように
      const da = b.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b.auxOf(adj, na);
      b.auxOf(adj, you);
      b.auxOf(adj, da);
      b.inOrder(na, you, 1);
      b.inOrder(you, da, 1);

      b.captureSpan('ようだ', na, da);
    },

    // Pattern 4: ようです (polite form - よう + です)
    (b) => {
      const pred = b.tok({}, 'pred');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(pred, you);
      b.auxOf(pred, desu);
      b.inOrder(you, desu, 1);

      b.captureSpan('ようだ', you, desu);
    },

    // Pattern 5: Noun + の + ようです (polite copula)
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      const you = b.tok({ lemma: 'よう' }, 'you');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.inOrder(noun, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, desu, 1);

      b.captureSpan('ようだ', no, desu);
    },

    // Pattern 6: Na-adj + な + ようです (polite copula)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(adj, na);
      b.auxOf(adj, you);
      b.auxOf(adj, desu);
      b.inOrder(na, you, 1);
      b.inOrder(you, desu, 1);

      b.captureSpan('ようだ', na, desu);
    }
  );
});
