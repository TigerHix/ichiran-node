import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことなく - "without doing, never doing"
 *
 * Matches: verb-dictionary form + こと + なく (formal expression for "without doing X")
 *
 * This is a formal/literary expression indicating that (A) does not happen before (B),
 * or literally "with not having happened (A)". It's more formal than ないで or ずに.
 *
 * Structure: Verb［辞書形］+ ことなく
 * Also accepts: こともなく (variant with mo)
 *
 * Examples:
 * - 彼は休むことなく働き続けた (He continued working without rest)
 * - 遅れることなく到着した (To arrive without being late)
 * - 誰にも知らせることなく出発した (Left without telling anyone)
 *
 * GiNZA parse structure (for "彼は休むことなく働き続けた"):
 * - 休む(VERB) --acl--> こと(NOUN)
 * - こと(NOUN) --obl--> なく(ADJ)
 * - なく --advcl--> 働き(VERB)
 *
 * The verb can be any predicate (verb, adjective) in dictionary form.
 * なく is the adverbial form (連用形) of ない, tagged as ADJ in GiNZA.
 */
export default linguisticRule('ことなく', (r) => {
  r.either(
    // Branch 1: Standard pattern 〜ことなく
    (b) => {
      // Preceding predicate (verb in dictionary form)
      // In GiNZA: dep=acl pointing to koto
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer) - dep=obl pointing to naku
      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');

      // pred must be head of koto with dep=acl (edge: pred -> koto)
      // Actually in GiNZA: pred.dep='acl', pred.head=koto
      // So koto is the head, pred is the child
      b.headChild(koto, pred, 'acl');

      // Followed by なく (adverbial form of ない, tagged as ADJ)
      // dep=advcl pointing to the following verb
      const naku = b.adj({ lemma: 'ない', dep: 'advcl' }, 'naku');
      b.inOrder(koto, naku, 1);

      // koto must point to naku with dep=obl (edge: koto -> naku)
      // So naku is the head, koto is the child
      b.headChild(naku, koto, 'obl');

      b.captureSpan('ことなく', pred, naku);
    },
    // Branch 2: Variant with も (〜こともなく)
    (b) => {
      // Preceding predicate
      const pred = b.tok({}, 'pred');

      // Followed by こと
      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');
      b.headChild(koto, pred, 'acl');

      // Followed by も (particle)
      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 1);

      // Followed by なく
      const naku = b.adj({ lemma: 'ない', dep: 'advcl' }, 'naku');
      b.inOrder(mo, naku, 1);
      b.headChild(naku, koto, 'obl');

      b.captureSpan('ことなく', pred, naku);
    }
  );
});
