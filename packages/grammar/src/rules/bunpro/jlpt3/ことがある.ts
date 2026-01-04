import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことがある - sometimes / there are times when
 *
 * Matches verb/adj in dictionary form + こと + が + ある/あります
 * Also matches variant with も instead of が: こともある
 *
 * This is DIFFERENT from たことがある (JLPT5 - past experience):
 * - JLPT5: Verb-た + ことがある = "have done before"
 * - JLPT3: Verb-dict form + ことがある = "sometimes do"
 *
 * Structure:
 * - Verb［る］+ こと + が/も + ある/あります (casual/polite)
 * - ［い］Adj + こと + が/も + ある/あります
 * - ［な］Adj + な + こと + が/も + ある/あります
 *
 * Examples:
 * - この馬は人を蹴ることがある (This horse sometimes kicks people)
 * - たまに楽しいことがある (There are times when it's fun)
 * - 大変なこともある (There are also times when it's difficult)
 * - 高級な洋服店だが服が安いこともある (It's a high-end store, but there are also times when clothes are cheap)
 *
 * GiNZA parse structure:
 * - 蹴ることがある: 蹴(verb) + こと(noun) + が(particle) + ある(verb)
 * - 楽しいことがある: 楽しい(adj) + こと(noun) + が(particle) + ある(verb)
 * - 安いこともある: 安い(adj) + こと(noun) + も(particle) + ある(verb)
 *
 * To exclude たことがある (past experience), we require verb directly before koto.
 * This prevents matching 食べたことがある (verb + た + こと).
 */
export default linguisticRule('ことがある', (r) => {
  r.either(
    // Branch 1: Verb + ことがある (casual)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('ことがある', verb, aru);
    },
    // Branch 2: Verb + こともある (casual)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(mo, aru, 1);

      b.captureSpan('ことがある', verb, aru);
    },
    // Branch 3: Verb + ことがあります (polite)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 3); // Allow adverbs like よく between koto and ga

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 3); // Allow adverbs between ga and aru

      b.captureSpan('ことがある', verb, masu);
    },
    // Branch 4: Verb + こともあります (polite)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 3); // Allow adverbs like よく between koto and mo

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(mo, aru, 3); // Allow adverbs between mo and aru

      b.captureSpan('ことがある', verb, masu);
    },
    // Branch 5: ［い］Adj + ことがある (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('ことがある', adj, aru);
    },
    // Branch 6: ［い］Adj + こともある (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(mo, aru, 1);

      b.captureSpan('ことがある', adj, aru);
    },
    // Branch 7: ［い］Adj + ことがあります (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 3); // Allow adverbs like よく between koto and ga

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 3); // Allow adverbs between ga and aru

      b.captureSpan('ことがある', adj, masu);
    },
    // Branch 8: ［い］Adj + こともあります (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(adj, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 3); // Allow adverbs like よく between koto and mo

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(mo, aru, 3); // Allow adverbs between mo and aru

      b.captureSpan('ことがある', adj, masu);
    },
    // Branch 9: ［な］Adj + な + ことがある (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('ことがある', adj, aru);
    },
    // Branch 10: ［な］Adj + な + こともある (casual)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(mo, aru, 1);

      b.captureSpan('ことがある', adj, aru);
    },
    // Branch 11: ［な］Adj + な + ことがあります (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(koto, ga, 3); // Allow adverbs like よく between koto and ga

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 3); // Allow adverbs between ga and aru

      b.captureSpan('ことがある', adj, masu);
    },
    // Branch 12: ［な］Adj + な + こともあります (polite)
    (b) => {
      const adj = b.adj({}, 'pred');
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.auxOf(adj, na);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(na, koto, 1);

      const mo = b.particle('も', 'mo');
      b.inOrder(koto, mo, 3); // Allow adverbs like よく between koto and mo

      const aru = b.verb({ lemma: 'ある', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(mo, aru, 3); // Allow adverbs between mo and aru

      b.captureSpan('ことがある', adj, masu);
    }
  );
});
