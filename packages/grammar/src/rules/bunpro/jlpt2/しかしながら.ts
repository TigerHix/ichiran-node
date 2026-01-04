import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: しかしながら (shikashinagara) - "however, nevertheless"
 *
 * A formal conjunction meaning "however" or "nevertheless". This is an
 * emphatic variant of しかし (however), formed by combining しかし with
 * the particle ながら (while/although).
 *
 * Structure:
 * - Phrase。しかしながら、Phrase
 * - Can also appear as 併し乍ら (kanji form, rare)
 *
 * Examples:
 * - 努力した。しかしながら、失敗した。
 *   (I made an effort. However, I failed.)
 * - 確かに便利だ。しかしながら、高価だ。
 *   (It is certainly convenient. However, it is expensive.)
 * - 日本は安全な国だと言われている。しかしながら、１００％安全というわけでもない。
 *   (Japan is said to be a safe country. However, it is not 100% safe.)
 *
 * Key discriminators:
 * - Formal conjunction used at the beginning of sentences
 * - More formal and emphatic than しかし alone
 * - Can be written as しかしながら (hiragana) or 併し乍ら (kanji)
 * - Typically followed by comma in written Japanese
 * - Used in formal writing, speeches, and literature
 *
 * GiNZA parse structure:
 * - Split into two tokens: しかし (CCONJ, dep=ROOT) + ながら (SCONJ, dep=fixed)
 * - The ながら particle has dep="fixed" relationship to しかし
 *
 * Different from:
 * - しかし (shikashi) - "however" (less formal, without ながら)
 * - ですが (desuga) - "but" (polite, conversational)
 * - だが (daga) - "but" (plain, conversational)
 * - ところが (tokoroga) - "however" (less formal, more conversational)
 * - それなのに (sorennoni) - "and yet" (expresses surprise/disappointment)
 */
export default linguisticRule('しかしながら', (r) => {
  // GiNZA parses しかしながら as two separate tokens:
  // 1. しかし (CCONJ, dep=ROOT)
  // 2. ながら (SCONJ, dep=fixed)
  const shikashi = r.tok({
    text: 'しかし',
    lemma: 'しかし',
    pos: 'CCONJ',
  }, 'shikashi');

  const nagara = r.tok({
    text: 'ながら',
    lemma: 'ながら',
    pos: 'SCONJ',
    dep: 'fixed',
  }, 'nagara');

  r.inOrder(shikashi, nagara, 1);
  r.captureSpan('しかしながら', shikashi, nagara);
});
