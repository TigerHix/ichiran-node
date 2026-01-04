import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ぎみ', (r) => {
  // ぎみ/気味 (gimi) - suffix indicating slight sign or tendency
  // Pattern: Verb [stem] or Noun + ぎみ
  //
  // Examples:
  // - つかれぎみ (tsukare-gimi): feeling a bit tired
  // - おくれぎみ (okure-gimi): tending to be late
  // - 風邪ぎみ (kaze-gimi): feeling a bit sick
  // - 太りぎみ (futori-gimi): getting a bit fat
  // - 貧血ぎみ (hinketsu-gimi): somewhat anemic
  //
  // GiNZA parsing observations:
  // - When used as suffix: lemma is "ぎみ" (hiragana only in test data)
  // - When used as independent noun: lemma is "気味" (kanji)
  // - POS is always NOUN
  // - Dependency varies: compound, advcl, ccomp, root (depending on sentence structure)
  //
  // Key discriminator: lemma="ぎみ" identifies the suffix usage
  // Negative case: lemma="気味" is the independent noun meaning "feeling/sensation"

  r.either(
    // Pattern 1: ぎみ as NOUN
    // When attached to noun/verb stem in predicate position
    // Example: つかれぎみだ, おくれぎみだから
    (b) => {
      const gimi = b.tok({
        lemma: 'ぎみ',
        pos: 'NOUN'
      }, 'gimi');
      b.capture(gimi);
    },
    // Pattern 2: ぎみ as ADJ
    // When at clause end or followed by auxiliaries (だっ, らしい)
    // Examples: 風邪ぎみだった, ふとりぎみらしい, あれぎみだ
    (b) => {
      const gimi = b.tok({
        lemma: 'ぎみ',
        pos: 'ADJ'
      }, 'gimi');
      b.capture(gimi);
    }
  );
});
