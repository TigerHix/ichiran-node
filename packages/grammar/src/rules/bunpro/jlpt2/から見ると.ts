import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('から見ると', (r) => {
  // から見ると/から見れば/から見て/から見たら - "from the standpoint of, judging from"
  // Pattern: noun + から + 見る(inflected) + conjunction particle
  //
  // Meaning: "From the point of view of (A), (B)" or "Judging from (A), (B)"
  // A hypothetical expression that points out how something looks from the perspective of (A).
  //
  // Examples:
  // - 素人から見るとかなりうまい人でも、プロの世界では全然通用しないらしい。
  // - 老人夫婦から見ると、若者の恋愛は懐かしい。
  // - 外から見ると、やりやすそうです。
  // - 普段から見て、今年は稀な気象が多かった。
  //
  // Note: GiNZA uses lemma="見る" (hiragana) regardless of surface form (見る vs 見)
  // The inflection suffix (と/ば/て/たら) may be tokenized separately from the verb stem

  const kara = r.particle('から', 'kara');

  r.either(
    // Pattern 1: から見ると (conditional form with と)
    (b) => {
      const miru = b.verb({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const to = b.tok({ text: 'と' }, 'to');
      b.inOrder(kara, miru, 3);
      b.inOrder(miru, to, 3);
      b.captureSpan('から見ると', kara, to);
    },
    // Pattern 2: から見れば (conditional form with ば)
    (b) => {
      const miru = b.verb({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const ba = b.tok({ text: 'ば' }, 'ba');
      b.inOrder(kara, miru, 3);
      b.inOrder(miru, ba, 3);
      b.captureSpan('から見れば', kara, ba);
    },
    // Pattern 3: から見て (te-form)
    (b) => {
      const miru = b.verb({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const te = b.tok({ text: 'て' }, 'te');
      b.inOrder(kara, miru, 3);
      b.inOrder(miru, te, 3);
      b.captureSpan('から見て', kara, te);
    },
    // Pattern 4a: から見たら (conditional form with たら as single token)
    (b) => {
      const miru = b.verb({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const tara = b.tok({ text: 'たら' }, 'tara');
      b.inOrder(kara, miru, 3);
      b.inOrder(miru, tara, 3);
      b.captureSpan('から見たら', kara, tara);
    },
    // Pattern 4b: から見た + ら (split tokenization)
    // GiNZA sometimes parses 見たら as 見た (VERB/AUX) + ら (AUX)
    (b) => {
      const verbEndingInTa = b.tok({
        posOneOf: ['VERB', 'AUX'],
        text: 'た',
      }, 'verbEndingInTa');

      // ら is the conditional marker
      const ra = b.aux({
        text: 'ら',
        lemma: 'ら',
      }, 'ra');

      // ら attaches to the verb ending in た
      b.auxOf(verbEndingInTa, ra);

      // から comes before the verb
      b.inOrder(kara, verbEndingInTa, 3);

      b.captureSpan('から見たら', kara, ra);
    }
  );
});
