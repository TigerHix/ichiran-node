import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('どころか', (r) => {
  // どころか (dokoroka) - "far from, on the contrary, let alone"
  //
  // Used to strongly negate the preceding statement while emphasizing what follows.
  // Meaning: "Far from (A), actually (B)" or "Let alone (A), not even (B)"
  //
  // Patterns:
  // 1. Verb + どころか: 休むどころか, 行くどころか, やむどころか
  // 2. I-adjective + どころか: 甘いどころか, 寒いどころか, 辛いどころか
  // 3. Na-adjective + な + どころか: 謙虚などろか, 楽などろか
  // 4. Noun + どころか: 英語どころか, 酎ハイどころか
  //
  // GiNZA parsing notes:
  // - どころか is typically a single particle (PART) or adverb (ADV)
  // - May also be tokenized as どころ (noun) + か (particle)

  r.either(
    // Pattern 1: Single token どころか with any preceding word
    // This catches most cases where GiNZA tokenizes as one unit
    (b) => {
      const dokoroka = b.tok({ text: 'どころか' }, 'dokoroka');
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, dokoroka, 5);
      b.captureSpan('どころか', prev, dokoroka);
    },

    // Pattern 2: Multiple tokens - どころ (noun) + か (particle)
    // GiNZA may tokenize this way in some cases
    (b) => {
      const dokoro = b.tok({ textOneOf: ['どころ', '所'] }, 'dokoro');
      const ka = b.particle('か', 'ka');
      b.inOrder(dokoro, ka, 1);
      const prev = b.tok({}, 'prev');
      b.inOrder(prev, dokoro, 5);
      b.captureSpan('どころか', prev, ka);
    }
  );
});
