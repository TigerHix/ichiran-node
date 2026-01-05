import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と並んで', (r) => {
  // と並んで (to narande) - "alongside, on par with, comparable to"
  // Formal expression showing something is comparable to (A), in line with it
  //
  // This grammar point covers TWO patterns:
  // 1. Noun + と並んで (te-form) - "comparable with, alongside"
  // 2. Noun + と並ぶほど (dictionary form + ほど) - "to the extent of being comparable"
  //
  // Examples:
  // - きしめんも手羽先と並んで人気です (Kishimen is as popular as chicken wings)
  // - ラーメンもまた、寿司と並んで人気の日本食だ (Ramen is popular alongside sushi)
  // - この映画はあの映画と並ぶほどいい (This movie is as good as that one)
  //
  // GiNZA parsing notes:
  // - と is a particle (ADP)
  // - The verb may be written as 並ぶ/並んで (kanji) or ならぶ/ならんで (hiragana)
  // - We use lemma matching which should be consistent regardless of surface form

  r.either(
    // Pattern 1: Noun + と + 並んで (te-form)
    (b) => {
      const to = b.particle('と', 'to');

      // Match the verb with lemma=並ぶ (kanji) or lemma=ならぶ (hiragana) in te-form
      // When written in hiragana, GiNZA uses hiragana lemma
      // Use tok() instead of verb() to handle cases where POS varies
      const verb = b.tok({
        lemmaOneOf: ['並ぶ', 'ならぶ'],
      }, 'verb');

      b.inOrder(to, verb, 1);
      b.captureSpan('と並んで', to, verb);
    },

    // Pattern 2: Noun + と + 並ぶ + ほど (dictionary form + hodo)
    (b) => {
      const to = b.particle('と', 'to');

      // Match the verb with lemma=並ぶ (kanji) or lemma=ならぶ (hiragana) in dictionary form
      // Use tok() instead of verb() to handle cases where POS varies
      const verb = b.tok({
        lemmaOneOf: ['並ぶ', 'ならぶ'],
      }, 'verb');

      const hodo = b.tok({ text: 'ほど' }, 'hodo');

      b.inOrder(to, verb, 1);
      b.inOrder(verb, hodo, 1);
      b.captureSpan('と並ぶほど', to, hodo);
    }
  );
});
