import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('にきがつく', (r) => {
  // に気がつく / に気づく (to notice/realize)
  //
  // Structure variations:
  // 1. Noun + に + 気がつく (notice something)
  //    - 自分のミスに気がついた
  //    - 私の新しい靴に気がついてくれた
  //
  // 2. Verb + ことに + 気がつく (realize a fact/situation)
  //    - ４時間が経ったことに気がついた
  //    - 忘れたことに気がついた
  //
  // 3. Noun + の + ことに + 気がつく
  //    - 犬に餌をあげていないのに気が付いた
  //
  // 4. Just 気がつく (standalone - without target particle)
  //    - わぁ！気がついたの？
  //
  // Verb forms:
  // - Present: 気がつく
  // - Past: 気がついた
  // - Te-form: 気がついて
  // - Negative: 気がつかない
  // - Progressive: 気がついている
  // - Negative past progressive: 気がついていなかった
  // - Polite variants: 気がつきます, 気がついていません
  //
  // GiNZA parsing notes:
  // - 気 (き) can be parsed as:
  //   - lemma=くる (from 来る), pos=NOUN or VERB, dep=nsubj (most common)
  //   - lemma=き, pos=NOUN, dep=nsubj (when used standalone in casual speech)
  //   - lemma=くる, pos=VERB, dep=dep (when embedded in complex clauses)
  // - が is ADP with lemma=が, dep=case
  // - つく (つき/ついた/ついて) is VERB with lemma=つく
  // - The compound verb 気づく may be parsed as single VERB with lemma=きづく
  // - The particle に connects to what is noticed (dep=obl or dep=case)

  r.either(
    // Branch 1: Noun + に + 気がつく (direct object notice)
    // Example: 自分のミスに気がつきましたか
    // GiNZA: ミス(NOUN) + に(ADP,case) + き(NOUN,nsubj,lemma=くる) + が(ADP,case) + つき(VERB,lemma=つく)
    (b) => {
      const target = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['obl', 'obj', 'nsubj'],
      }, 'target');
      const ni = b.particle('に', 'ni');
      const ki = b.noun({ lemmaOneOf: ['くる', 'き'] }, 'ki');
      const ga = b.particle('が', 'ga');
      const tsuku = b.verb({ lemma: 'つく' }, 'tsuku');

      b.caseMarker(target, ni);
      b.inOrder(ni, ki, 3);
      b.caseMarker(ki, ga);
      b.inOrder(ga, tsuku, 1);
      b.captureSpan('にきがつく', ni, tsuku);
    },

    // Branch 2: (Verb/Noun + の/こと) + に + 気がつく (realize that)
    // Example: ４時間が経ったことに気がついた
    // Example: 犬に餌をあげていないのに気が付いた
    (b) => {
      const koto = b.noun({
        lemmaOneOf: ['こと', 'の'],
        depOneOf: ['obl', 'obj', 'nsubj'],
      }, 'koto');
      const ni = b.particle('に', 'ni');
      const ki = b.tok({ lemmaOneOf: ['くる', 'き'] }, 'ki');
      const ga = b.particle('が', 'ga');
      const tsuku = b.verb({ lemma: 'つく' }, 'tsuku');

      b.caseMarker(koto, ni);
      b.inOrder(ni, ki, 3);
      b.caseMarker(ki, ga);
      b.inOrder(ga, tsuku, 1);
      b.captureSpan('にきがつく', ni, tsuku);
    },

    // Branch 3: 気がつく without target particle (standalone)
    // Example: わぁ！気がついたの？嬉しい！
    // GiNZA: き(NOUN,nsubj,lemma=き or くる) + が(ADP,case) + ついた(VERB,lemma=つく)
    (b) => {
      const ki = b.tok({
        lemmaOneOf: ['くる', 'き'],
        depOneOf: ['nsubj', 'dep'],
      }, 'ki');
      const ga = b.particle('が', 'ga');
      const tsuku = b.verb({ lemma: 'つく' }, 'tsuku');

      b.caseMarker(ki, ga);
      b.inOrder(ga, tsuku, 1);
      b.captureSpan('にきがつく', ki, tsuku);
    },

    // Branch 4: Noun + に + 気づく (variant using compound verb 気づく)
    // Example: 自分のミスに気づきました
    // GiNZA may parse this as a single verb or as separate tokens
    (b) => {
      const target = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['obl', 'obj', 'nsubj'],
      }, 'target');
      const ni = b.particle('に', 'ni');
      const kizuku = b.verb({ lemma: 'きづく' }, 'kizuku');

      b.caseMarker(target, ni);
      b.inOrder(ni, kizuku, 1);
      b.captureSpan('にきがつく', ni, kizuku);
    },

    // Branch 5: (Verb/Noun + の/こと) + に + 気づく
    // Example: ４時間が経ったことに気づいた
    (b) => {
      const koto = b.noun({
        lemmaOneOf: ['こと', 'の'],
        depOneOf: ['obl', 'obj', 'nsubj'],
      }, 'koto');
      const ni = b.particle('に', 'ni');
      const kizuku = b.verb({ lemma: 'きづく' }, 'kizuku');

      b.caseMarker(koto, ni);
      b.inOrder(ni, kizuku, 1);
      b.captureSpan('にきがつく', ni, kizuku);
    },

    // Branch 6: 気づく without target particle (standalone compound verb)
    // Example: わぁ！気づいたの？
    (b) => {
      const kizuku = b.verb({ lemma: 'きづく' }, 'kizuku');
      b.captureSpan('にきがつく', kizuku, kizuku);
    }
  );
});
