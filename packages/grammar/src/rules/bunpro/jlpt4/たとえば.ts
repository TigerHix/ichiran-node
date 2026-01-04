import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: たとえば (たとえば) - For example
 *
 * Matches たとえば (for example / for instance), an adverb used to present examples.
 *
 * Structure:
 * - たとえば (adverb at beginning of sentence or clause)
 *
 * Examples:
 * - たとえば、ドイツとかは？ (For example, how about Germany?)
 * - たとえば、冷たい物を食べたときに歯が痛いです。 (For example, my teeth hurt when I eat cold things.)
 * - 漢字を覚えるサイトがたくさんあります。たとえば、ワニカニだ。 (There are many sites for learning kanji. For example, Wanikani.)
 *
 * Key discriminators:
 * - Must be ADV (adverb) with lemma=たとえば
 * - This distinguishes from other たとえ compound forms
 *
 * GiNZA parse structure:
 * - たとえば: text=たとえば, lemma=たとえば, pos=ADV, dep=advmod
 */
export default linguisticRule('たとえば', (r) => {
  const tatoeba = r.adv({ lemma: 'たとえば' }, 'tatoeba');
  r.capture(tatoeba);
});
