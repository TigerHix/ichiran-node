import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('かのようだ', (r) => {
  // かのようだ/かのようです expresses "as if" or "just like" - a figurative comparison
  // Pattern: plain form + かの + ようだ/ようです/ような/ように
  // The "かの" adds uncertainty/emphasis to the comparison
  //
  // GiNZA parses this as:
  // - か (particle, case marker)
  // - の (particle, case marker)
  // - よう (AUX, lemma=よう)
  // - だ/です (AUX, copula)
  //
  // Forms:
  // - かのようだ (plain casual)
  // - かのようです (polite)
  // - かのような (adnominal, before noun)
  // - かのように (adverbial)
  // - であるかのようだ (after noun/na-adj)

  r.either(
    // Pattern 1: Verb/i-adj + かのようだ (plain form)
    (b) => {
      const pred = b.tok({}, 'pred');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const da = b.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, da, 1);

      b.captureSpan('かのようだ', ka, da);
    },

    // Pattern 2: Verb/i-adj + かのようです (polite)
    (b) => {
      const pred = b.tok({}, 'pred');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, desu, 1);

      b.captureSpan('かのようだ', ka, desu);
    },

    // Pattern 3: Verb/i-adj + かのような (adnominal, before noun)
    (b) => {
      const pred = b.tok({}, 'pred');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const na = b.tok({ text: 'な', lemma: 'だ' }, 'na');

      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, na, 1);

      b.captureSpan('かのようだ', ka, na);
    },

    // Pattern 4: Verb/i-adj + かのように (adverbial form)
    (b) => {
      const pred = b.tok({}, 'pred');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const ni = b.tok({ text: 'に', lemma: 'だ' }, 'ni');

      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, ni, 1);

      b.captureSpan('かのようだ', ka, ni);
    },

    // Pattern 5: Noun/na-adj + である + かのようだ
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'ADJ'] }, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const da = b.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b.inOrder(noun, dearu, 3);
      b.inOrder(dearu, ka, 3);
      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, da, 1);

      b.captureSpan('かのようだ', dearu, da);
    },

    // Pattern 6: Noun/na-adj + である + かのようです (polite)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'ADJ'] }, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.inOrder(noun, dearu, 3);
      b.inOrder(dearu, ka, 3);
      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, desu, 1);

      b.captureSpan('かのようだ', dearu, desu);
    },

    // Pattern 7: Noun/na-adj + である + かのような (adnominal)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'ADJ'] }, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const na = b.tok({ text: 'な', lemma: 'だ' }, 'na');

      b.inOrder(noun, dearu, 3);
      b.inOrder(dearu, ka, 3);
      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, na, 1);

      b.captureSpan('かのようだ', dearu, na);
    },

    // Pattern 8: Noun/na-adj + である + かにように (adverbial)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'ADJ'] }, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const ni = b.tok({ text: 'に', lemma: 'だ' }, 'ni');

      b.inOrder(noun, dearu, 3);
      b.inOrder(dearu, ka, 3);
      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);
      b.inOrder(you, ni, 1);

      b.captureSpan('かのようだ', dearu, ni);
    },

    // Pattern 9: Verb/i-adj + かのよう (stem form without copula, conjunctive)
    (b) => {
      const pred = b.tok({}, 'pred');
      const ka = b.particle('か', 'ka');
      const no = b.particle('の', 'no');
      const you = b.tok({ lemma: 'よう' }, 'you');

      b.inOrder(ka, no, 1);
      b.inOrder(no, you, 1);

      b.captureSpan('かのようだ', ka, you);
    }
  );
});
