import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からすると・からすれば (karasuruto/karasureba) - "Judging from, Considering"
 *
 * A hypothetical expression used to cast judgments based on evidence or from a particular standpoint.
 *
 * Structure:
 * - Noun + からすると (kara + suru + to)
 * - Noun + からすれば (kara + sure + ba)
 *
 * Examples:
 * - 彼の表情からすると、試験は悪くなかったようだ。
 *   (Judging from his expression, the exam didn't go bad.)
 * - 今の状況からすれば、成功は難しい。
 *   (Considering the current situation, success is difficult.)
 * - 歴史からすると、京都はかつて首都でした。
 *   (Judging from history, Kyoto was once the capital.)
 *
 * Key discriminators:
 * - Follows a noun (evidence or standpoint)
 * - Uses から (from) + する (suru) + conditional (と/ば)
 * - More objective than からして
 * - Shows judgment/inference based on evidence
 *
 * GiNZA parse structure:
 * - 表情からすると:
 *   - 表情(NOUN) + から(ADP) + する(VERB) + と(ADP)
 */
export default linguisticRule('からすると-からすれば', (r) => {
  // Preceding noun (evidence or standpoint)
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  // Particle から (from)
  const kara = r.particle('から', 'kara');
  r.inOrder(noun, kara, 1);

  r.either(
    // Pattern 1: から + する + と (からすると)
    (b) => {
      const suru = b.tok({ text: 'する', lemma: 'する' }, 'suru');
      const to = b.tok({ text: 'と' }, 'to');
      b.inOrder(kara, suru, 1);
      b.inOrder(suru, to, 1);
      b.captureSpan('からすると-からすれば', noun, to);
    },

    // Pattern 2: から + すれ + ば (からすれば)
    (b) => {
      const sure = b.tok({ text: 'すれ', lemma: 'する' }, 'sure');
      const ba = b.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b.inOrder(kara, sure, 1);
      b.inOrder(sure, ba, 1);
      b.captureSpan('からすると-からすれば', noun, ba);
    }
  );
});
