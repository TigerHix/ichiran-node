import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: と考えられる (to kangaerareru) - "Can be considered as, Is thought to be"
 *
 * An expression used to state an objective opinion or judgment based on evidence
 * or reasoning. Translates as "it can be considered (A)", "it is thought to be (A)",
 * "one can think that (A)", or "it is conceivable that (A)".
 *
 * Structure:
 * - Phrase + と + 考えられる (potential/passive form of 考える)
 *
 * Examples:
 * - あの事故は煽り運転のせいだと考えられる。
 *   (Tailgating is thought to be the cause of this accident.)
 * - 日本語は文字種が多いことから、第二言語として習うのが難しいと考えられる。
 *   (It is thought that Japanese is difficult to learn as a second language due to having several different character types.)
 * - 光の性質は、実は粒子と波動の両方だと考えられる。
 *   (The nature of light is actually thought to be both a particle and a wave.)
 * - 中国は経済成長が続いていけば新たな超大国となると考えられる。
 *   (China may well be considered the new superpower if it maintains its current economic growth.)
 *
 * Key discriminators:
 * - Expresses objective opinion based on reasoning/evidence
 * - More objective than subjective と思われる (to omowareru)
 * - Different from と考えられている (more general/widely accepted opinion)
 * - と marks the quoted content being considered
 * - 考えられる is potential/passive form of 考える (kangaeru)
 *
 * GiNZA parse structure:
 * - ... [quoted content] ...(VERB/NOUN/ADJ) + と(ADP) + 考えられる(VERB/AUX,lemma=考える)
 *
 * Different from:
 * - Simple と quoting (say, think) - different verb
 * - と思われる (more subjective opinion) - different verb
 * - と考えられている (widely accepted opinion) - continuous form (ている)
 */
export default bunproLinguisticRule('と考えられる', (r) => {
  r.either(
    // Pattern 1: と + 考えられる as single verb token (full potential form)
    (b1) => {
      const to = b1.particle('と', 'to');
      const verb = b1.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['考える', 'かんがえる'],
        // Match forms containing potential suffix (may have particles after)
        textRe: /.*?られ/,
      }, 'verb');
      b1.inOrder(to, verb, 3);
      b1.captureSpan('と考えられる', to, verb);
    },
    // Pattern 2: と + 考え(VERB) + られる(AUX) - split parsing
    (b2) => {
      const to = b2.particle('と', 'to');
      const kangaee = b2.tok({
        pos: 'VERB',
        lemmaOneOf: ['考える', 'かんがえる'],
      }, 'kangaee');
      const reru = b2.aux({
        textOneOf: ['られる', 'れます'],
        lemmaOneOf: ['れる', 'られる'],
      }, 'reru');
      b2.auxOf(kangaee, reru);
      b2.inOrder(to, kangaee, 5);
      b2.captureSpan('と考えられる', to, reru);
    },
    // Pattern 3: と + 考え(VERB) + potential auxiliary + polite auxiliary
    (b3) => {
      const to = b3.particle('と', 'to');
      const kangaee = b3.tok({
        pos: 'VERB',
        lemmaOneOf: ['考える', 'かんがえる'],
      }, 'kangaee');
      const rare = b3.aux({
        lemmaOneOf: ['れる', 'られる'],
      }, 'rare');
      const masu = b3.aux({
        lemma: 'ます',
      }, 'masu');
      b3.auxOf(kangaee, rare);
      b3.auxOf(rare, masu);
      b3.inOrder(to, kangaee, 5);
      b3.captureSpan('と考えられる', to, rare);
    }
  );
});
