import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と同じくらい', (r) => {
  // と同じくらい (to onaji kurai) - "about the same as" or "equally"
  // Patterns:
  // 1. Noun + と + 同じ/おなじ + くらい/ぐらい: プリンターと同じくらい, トモキとおなじぐらい
  // 2. Noun + と + 同じ/おなじ + くらい/ぐらい + の + Noun: と同じくらいの大きさ, とおなじぐらいの年齢
  //
  // Variants: くらい and ぐらい are both acceptable, 同じ and おなじ are both acceptable
  //
  // GiNZA parsing notes:
  // - と is ADP with dep=case
  // - 同じ/おなじ is ADJ with tag=連体詞
  // - くらい/ぐらい is ADP/PART with dep=case or dep=mark

  r.either(
    // Pattern 1: Noun + と + 同じ/おなじ + くらい/ぐらい (without trailing の)
    // プリンターと同じくらい, 昨日とおなじぐらい, トモキとおなじくらい
    (b) => {
      const to = b.particle('と', 'to');
      const onaji = b.tok({
        pos: 'ADJ',
        textOneOf: ['同じ', 'おなじ'],
      }, 'onaji');
      const kurai = b.tok({
        posOneOf: ['ADP', 'PART'],
        textOneOf: ['くらい', 'ぐらい'],
        depOneOf: ['case', 'mark'],
      }, 'kurai');
      b.inOrder(to, onaji, 1);
      b.inOrder(onaji, kurai, 1);
      b.captureSpan('と同じくらい', to, kurai);
    },

    // Pattern 2: Noun + と + 同じ/おなじ + くらい/ぐらい + の (with trailing の)
    // と同じくらいの, とおなじぐらいの
    (b) => {
      const to = b.particle('と', 'to');
      const onaji = b.tok({
        pos: 'ADJ',
        textOneOf: ['同じ', 'おなじ'],
      }, 'onaji');
      const kurai = b.tok({
        posOneOf: ['ADP', 'PART'],
        textOneOf: ['くらい', 'ぐらい'],
        depOneOf: ['case', 'mark'],
      }, 'kurai');
      const no = b.particle('の', 'no');
      b.inOrder(to, onaji, 1);
      b.inOrder(onaji, kurai, 1);
      b.inOrder(kurai, no, 1);
      b.captureSpan('と同じくらい', to, kurai);
    }
  );
});
