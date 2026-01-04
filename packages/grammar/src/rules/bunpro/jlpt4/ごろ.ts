import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ごろ (頃/ころ) - Around/approximate time
 *
 * Matches patterns where ごろ/ころ/頃 is used to express "around" or "approximately" for time expressions.
 *
 * Structures:
 * - Time/Counter + ごろ (around [time])
 * - Noun + の + ごろ (around the time of [Noun])
 * - Noun + の + 頃 (around the time of [Noun] - kanji form)
 * - Noun + の + ころ (around the time of [Noun] - hiragana form)
 *
 * Examples:
 * - ８時ごろに帰りました (I went home around 8 o'clock)
 * - 子供の頃はよく公園で遊びました (When I was around a child, I often played in the park)
 * - １１時ごろに帰ってくる (I'll be back around 11)
 * - 小学生の頃日本語を習い始めた (Around when I was an elementary student, I started learning Japanese)
 * - 今頃着いているでしょう (They're probably arriving around now)
 *
 * Key discriminators:
 * - Matches text "ごろ", "ころ", or "頃" (kanji)
 * - Can follow time expressions directly or after noun + の
 * - Different from くらい which is for degree/extent, not time spans
 *
 * GiNZA parse structure:
 * - ８時ごろ: ８(NUM) + 時(NOUN) + ごろ(NOUN/PART)
 * - 子供の頃: 子供(NOUN) + の(PART) + 頃(NOUN)
 */
export default linguisticRule('ごろ', (r) => {
  r.either(
    // Branch 1: ごろ (colloquial form)
    (b) => {
      const goro = b.tok({ text: 'ごろ' }, 'goro');
      b.capture(goro);
    },
    // Branch 2: ころ (plain hiragana form)
    (b) => {
      const koro = b.tok({ text: 'ころ' }, 'koro');
      b.capture(koro);
    },
    // Branch 3: 頃 (kanji form - lemma is ころ)
    (b) => {
      const koroKanji = b.tok({ text: '頃', lemma: 'ころ' }, 'koroKanji');
      b.capture(koroKanji);
    },
    // Branch 4: 今ごろ (imagoro - "around now", tokenized as single ADV)
    (b) => {
      const imagoro = b.tok({ text: '今ごろ', lemma: '今ごろ' }, 'imagoro');
      b.capture(imagoro);
    },
    // Branch 5: 何時ごろ (nanjigoro - "around what time", tokenized as single ADJ/NOUN)
    (b) => {
      const nanjigoro = b.tok({ text: '何時ごろ', lemma: '何時ごろ' }, 'nanjigoro');
      b.capture(nanjigoro);
    }
  );
});
