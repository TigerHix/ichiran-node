import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: だに-しない (dani-shinai) - Not even, Won't even
 *
 * A formal/literary particle structure indicating that even the slightest
 * example of (A) didn't happen. Used in limited set expressions.
 *
 * Structure: Noun + だに + しない/しなかった (negative of する)
 * Exception: 夢 + (に) + だに + 思わない
 *
 * Examples:
 * - 予想だにしなかった (could not even predict)
 * - 想像だにしない (not even imagine)
 * - 微動だにしない (not even moving a quiver)
 * - 夢にだ思わなかった (couldn't even dream of)
 *
 * Key discriminators:
 * - だに is an adverbial particle
 * - Followed by negative form of する (しない, しなかった, せず) or similar verb
 * - Preceded by limited set of nouns (予想, 想像, 微動, 夢, etc.)
 * - Literary/formal register
 *
 * GiNZA parse structure:
 * - NOUN + だに + [negative verb]
 * - The token between だに and the negative verb varies (しない, いなかった, せず, etc.)
 * - Note: "しない" may be tokenized as "し" + "ない" by GiNZA
 * - Note: "していなかった" may be split into many tokens
 * - Note: "だに" may be tokenized as "だ" + "に" in some cases
 *
 * Different from:
 * - さえ/すら (more common "even" particles)
 * - だに alone with positive verbs (Verb+だに pattern)
 * - Simple だに followed by non-する verbs
 */
export default bunproLinguisticRule('だに-しない', (r) => {
  r.either(
    // Pattern 1: だに + し + ない (split tokenization)
    // GiNZA often splits "しない" into "し" (aux) + "ない" (aux)
    (b1) => {
      const dani = b1.tok({ text: 'だに' }, 'dani');
      const shi = b1.tok({ text: 'し' }, 'shi');
      const nai = b1.tok({ text: 'ない' }, 'nai');

      b1.inOrder(dani, shi, 1);
      b1.inOrder(shi, nai, 1);

      b1.captureSpan('だに-しない', dani, nai);
    },

    // Pattern 2: だに + し + なかった (past negative, split tokenization)
    (b2) => {
      const dani = b2.tok({ text: 'だに' }, 'dani');
      const shi = b2.tok({ text: 'し' }, 'shi');
      const nakatta = b2.tok({ text: 'なかった' }, 'nakatta');

      b2.inOrder(dani, shi, 1);
      b2.inOrder(shi, nakatta, 1);

      b2.captureSpan('だに-しない', dani, nakatta);
    },

    // Pattern 2b: だに + し + なかっ + た (past negative, fully split)
    (b2b) => {
      const dani = b2b.tok({ text: 'だに' }, 'dani');
      const shi = b2b.tok({ text: 'し' }, 'shi');
      const nakat = b2b.tok({ text: 'なかっ' }, 'nakat');
      const ta = b2b.tok({ text: 'た' }, 'ta');

      b2b.inOrder(dani, shi, 1);
      b2b.inOrder(shi, nakat, 1);
      b2b.inOrder(nakat, ta, 1);

      b2b.captureSpan('だに-しない', dani, ta);
    },

    // Pattern 2c: だ + に + し + なかった (dani split into two tokens)
    (b2c) => {
      const da = b2c.tok({ text: 'だ' }, 'da');
      const ni = b2c.particle('に', 'ni');
      const shi = b2c.tok({ text: 'し' }, 'shi');
      const nakatta = b2c.tok({ text: 'なかった' }, 'nakatta');

      b2c.inOrder(da, ni, 1);
      b2c.inOrder(ni, shi, 1);
      b2c.inOrder(shi, nakatta, 1);

      b2c.captureSpan('だに-しない', da, nakatta);
    },

    // Pattern 2d: だ + に + し + なかっ + た (dani split + なかった split)
    (b2d) => {
      const da = b2d.tok({ text: 'だ' }, 'da');
      const ni = b2d.particle('に', 'ni');
      const shi = b2d.tok({ text: 'し' }, 'shi');
      const nakat = b2d.tok({ text: 'なかっ' }, 'nakat');
      const ta = b2d.tok({ text: 'た' }, 'ta');

      b2d.inOrder(da, ni, 1);
      b2d.inOrder(ni, shi, 1);
      b2d.inOrder(shi, nakat, 1);
      b2d.inOrder(nakat, ta, 1);

      b2d.captureSpan('だに-しない', da, ta);
    },

    // Pattern 3: だに + (して) + い + なかっ + た (progressive past negative, fully split)
    // Examples: 予想だにしていなかった, 想像だにしていなかった
    // Tokenization: し + て + い + なかっ + た
    (b3) => {
      const dani = b3.tok({ text: 'だに' }, 'dani');
      const i = b3.tok({ text: 'い' }, 'i');
      const nakat = b3.tok({ text: 'なかっ' }, 'nakat');
      const ta = b3.tok({ text: 'た' }, 'ta');

      b3.inOrder(dani, i, 10);
      b3.inOrder(i, nakat, 1);
      b3.inOrder(nakat, ta, 1);

      b3.captureSpan('だに-しない', dani, ta);
    },

    // Pattern 4: だに + (して) + いません (polite negative, combined)
    // Example: 微動だにしていません
    (b4) => {
      const dani = b4.tok({ text: 'だに' }, 'dani');
      const imasen = b4.tok({ text: 'いません' }, 'imasen');

      b4.inOrder(dani, imasen, 10);

      b4.captureSpan('だに-しない', dani, imasen);
    },

    // Pattern 4b: だに + (して) + い + ません (polite negative, split)
    // Tokenization: し + て + い + ません
    (b4b) => {
      const dani = b4b.tok({ text: 'だに' }, 'dani');
      const i = b4b.tok({ text: 'い' }, 'i');
      const imasen = b4b.tok({ text: 'ません' }, 'imasen');

      b4b.inOrder(dani, i, 10);
      b4b.inOrder(i, imasen, 1);

      b4b.captureSpan('だに-しない', dani, imasen);
    },

    // Pattern 4c: だに + (して) + い + ま + せ + ん (polite negative, fully split)
    // Tokenization: し + て + い + ま + せ + ん
    (b4c) => {
      const dani = b4c.tok({ text: 'だに' }, 'dani');
      const i = b4c.tok({ text: 'い' }, 'i');
      const ma = b4c.tok({ text: 'ま' }, 'ma');
      const se = b4c.tok({ text: 'せ' }, 'se');
      const n = b4c.tok({ text: 'ん' }, 'n');

      b4c.inOrder(dani, i, 10);
      b4c.inOrder(i, ma, 1);
      b4c.inOrder(ma, se, 1);
      b4c.inOrder(se, n, 1);

      b4c.captureSpan('だに-しない', dani, n);
    },

    // Pattern 5: だに + せ + ず (literary negative form)
    // Example: 一顧だにせず
    (b5) => {
      const dani = b5.tok({ text: 'だに' }, 'dani');
      const sezu = b5.tok({ text: 'せず' }, 'sezu');

      b5.inOrder(dani, sezu, 1);

      b5.captureSpan('だに-しない', dani, sezu);
    },

    // Pattern 5b: だ + に + せ + ず (dani split)
    (b5b) => {
      const da = b5b.tok({ text: 'だ' }, 'da');
      const ni = b5b.particle('に', 'ni');
      const sezu = b5b.tok({ text: 'せず' }, 'sezu');

      b5b.inOrder(da, ni, 1);
      b5b.inOrder(ni, sezu, 1);

      b5b.captureSpan('だに-しない', da, sezu);
    },

    // Pattern 6: 夢 + に + だ + に (alternative pattern for 夢にだに)
    // Some parsings might treat "だに" differently
    (b6) => {
      const yume = b6.tok({ textOneOf: ['夢', 'ゆめ'] }, 'yume');
      const ni1 = b6.particle('に', 'ni1');
      const da = b6.tok({ text: 'だ' }, 'da');
      const ni2 = b6.particle('に', 'ni2');
      const omowanai = b6.tok({ text: '思わない' }, 'omowanai');

      b6.inOrder(yume, ni1, 1);
      b6.inOrder(ni1, da, 1);
      b6.inOrder(da, ni2, 1);
      b6.inOrder(ni2, omowanai, 10);

      b6.captureSpan('だに-しない', yume, omowanai);
    },

    // Pattern 6b: 夢 + だ + に + 思わない (dani split version of pattern 6)
    (b6b) => {
      const yume = b6b.tok({ textOneOf: ['夢', 'ゆめ'] }, 'yume');
      const da = b6b.tok({ text: 'だ' }, 'da');
      const ni = b6b.particle('に', 'ni');
      const omowanai = b6b.tok({ text: '思わない' }, 'omowanai');

      b6b.inOrder(yume, da, 1);
      b6b.inOrder(da, ni, 1);
      b6b.inOrder(ni, omowanai, 10);

      b6b.captureSpan('だに-しない', yume, omowanai);
    },

    // Pattern 7: 一顧 + だ + に + せ + ず (compound noun pattern)
    // For 一顧だにせず where "一顧" might be parsed as a compound
    (b7) => {
      const ikko = b7.tok({ text: '一顧' }, 'ikko');
      const da = b7.tok({ text: 'だ' }, 'da');
      const ni = b7.particle('に', 'ni');
      const se = b7.tok({ text: 'せ' }, 'se');
      const zu = b7.tok({ text: 'ず' }, 'zu');

      b7.inOrder(ikko, da, 1);
      b7.inOrder(da, ni, 1);
      b7.inOrder(ni, se, 1);
      b7.inOrder(se, zu, 1);

      b7.captureSpan('だに-しない', ikko, zu);
    },

    // Pattern 7b: 一顧 + だに + せず (dani combined)
    (b7b) => {
      const ikko = b7b.tok({ text: '一顧' }, 'ikko');
      const dani = b7b.tok({ text: 'だに' }, 'dani');
      const sezu = b7b.tok({ text: 'せず' }, 'sezu');

      b7b.inOrder(ikko, dani, 1);
      b7b.inOrder(dani, sezu, 1);

      b7b.captureSpan('だに-しない', ikko, sezu);
    },

    // Pattern 8: 夢 + に + だ + に + 思わなかった (with past tense)
    // For 夢にだに思わなかった
    (b8) => {
      const yume = b8.tok({ textOneOf: ['夢', 'ゆめ'] }, 'yume');
      const ni1 = b8.particle('に', 'ni1');
      const da = b8.tok({ text: 'だ' }, 'da');
      const ni2 = b8.particle('に', 'ni2');
      const omowanakatta = b8.tok({ text: '思わなかった' }, 'omowanakatta');

      b8.inOrder(yume, ni1, 1);
      b8.inOrder(ni1, da, 1);
      b8.inOrder(da, ni2, 1);
      b8.inOrder(ni2, omowanakatta, 10);

      b8.captureSpan('だに-しない', yume, omowanakatta);
    },

    // Pattern 9: Catch-all for remaining patterns
    // Match any token followed by "だ" + "に" + negative verb
    (b9) => {
      const noun = b9.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'noun');
      const da = b9.tok({ text: 'だ' }, 'da');
      const ni = b9.particle('に', 'ni');
      const neg = b9.tok({ textOneOf: ['ない', 'なかった', 'せず', '思わない', '思わなかった'] }, 'neg');

      b9.inOrder(noun, da, 1);
      b9.inOrder(da, ni, 1);
      b9.inOrder(ni, neg, 10);

      b9.captureSpan('だに-しない', noun, neg);
    },

    // Pattern 10: Ultra-loose pattern - match "だ" followed by anything ending in "ない"
    // This is a last-resort pattern for cases where GiNZA tokenization is unexpected
    (b10) => {
      const noun = b10.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'] }, 'noun');
      const da = b10.tok({ text: 'だ' }, 'da');
      const neg = b10.tok({ text: 'ない' }, 'neg');

      b10.inOrder(noun, da, 1);
      b10.inOrder(da, neg, 20);

      b10.captureSpan('だに-しない', noun, neg);
    }
  );
});
