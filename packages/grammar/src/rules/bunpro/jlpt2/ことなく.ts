import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことなく (koto-naku) - "without doing"
 *
 * Matches: verb-dictionary form + こと + なく (without doing)
 *
 * A formal expression indicating that something happens without (A) happening,
 * or literally "with not having happened". This is the formal/literary version
 * of ないで (naide) and ずに (zu-ni), used in contrastive contexts.
 *
 * Structure variants:
 * - Verb［る］+ ことなく (basic form, most common)
 * - Verb［る］+ こともなく (emphatic variant with particle も)
 * - Verb［る］+ ことなしに (alternative form with なしで)
 *
 * The grammar point is formed from:
 * - こと (koto): nominalizer that turns the preceding verb into a noun
 * - なく (naku): adverbial form of ない (nai - negative)
 *
 * Key discriminators:
 * - こと must be a NOUN with dep=compound (attached to preceding verb)
 * - なく is an AUX/ADV with lemma=ない, inflectionForm=連用形-一般
 * - Must follow a verb in dictionary form
 * - Different from simple なく (not): this specifically negates the nominalized verb phrase
 *
 * Examples:
 * - 犠牲者を出すことなく救出する (rescue without casualties)
 * - 遅刻することなく職場に着いた (arrived at work without being late)
 * - 彼は社長に何も言うことなく会社を辞めた (he quit without telling the boss)
 * - 真実を知ることなく終わる (ends without knowing the truth)
 * - 遅れることなく到着した (arrived without being late)
 *
 * GiNZA parse structure (for "遅れることなく"):
 * - 遅れる(VERB) --compound--> こと(NOUN)
 * - こと --obl--> なく(ADV/AUX)
 *
 * For "こともなく" variant:
 * - こと(NOUN) --advcl--> も(PART/ADP)
 * - も --obl--> なく(ADV/AUX)
 */
export default linguisticRule('ことなく', (r) => {
  r.either(
    // Branch 1: Basic form (verb + ことなく)
    (b) => {
      // Preceding predicate (verb in dictionary form)
      // This is the verb that precedes こと (e.g., 捕まる in 捕まることなく)
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer)
      // GiNZA parses こと as NOUN with dep=obl
      // The verb (pred) has dep=acl and head pointing to こと
      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');
      b.headChild(koto, pred, 'acl');

      // Followed by なく (adverbial form of ない)
      // GiNZA parses this as ADJ (adjective) with lemma=ない
      // inflection=形容詞,連用形-一般
      const naku = b.tok({
        lemma: 'ない',
        text: 'なく',
        pos: 'ADJ',
        inflectionForm: '連用形-一般',
      }, 'naku');

      // こと has head pointing to なく (dep=advcl)
      b.headChild(naku, koto, 'obl');

      b.captureSpan('ことなく', pred, naku);
    },

    // Branch 2: Emphatic form with も (verb + こともなく)
    (b) => {
      const pred = b.tok({}, 'pred');

      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');
      b.headChild(koto, pred, 'acl');

      // Particle も (also/emphatic) - follows こと
      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 2);

      // Followed by なく
      const naku = b.tok({
        lemma: 'ない',
        text: 'なく',
        pos: 'ADJ',
        inflectionForm: '連用形-一般',
      }, 'naku');

      // When も is present, なく's head might still point to the main verb
      // but we can verify order constraint
      b.inOrder(mo, naku, 3);

      b.captureSpan('ことなく', pred, naku);
    },

    // Branch 3: Alternative form with なしに (verb + ことなしに)
    (b) => {
      const pred = b.tok({}, 'pred');

      const koto = b.noun({ lemma: 'こと', dep: 'obl' }, 'koto');
      b.headChild(koto, pred, 'acl');

      // なし (archaic negative form, equivalent to ない)
      // This is the classical negative form acting as an adverb/noun
      const nashi = b.tok({
        textOneOf: ['なし', '無し'],
        posOneOf: ['NOUN', 'ADV', 'ADJ'],
      }, 'nashi');

      b.inOrder(koto, nashi, 5);

      // Particle に (case marker for adverbial usage)
      const ni = b.particle('に', 'ni');
      b.inOrder(nashi, ni, 2);

      b.captureSpan('ことなく', pred, ni);
    }
  );
});
