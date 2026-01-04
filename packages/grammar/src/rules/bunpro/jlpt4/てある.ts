import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てある (transitive verb[te] + ある)
 *
 * Expresses a state resulting from a transitive verb action done intentionally.
 * "Something is done / Has been done / Remaining state"
 *
 * Examples:
 * - 置いてある (has been placed/left there)
 * - 開けてある (has been opened)
 * - 書いてある (has been written)
 * - 準備してある (preparations have been made)
 *
 * Key characteristics:
 * - Only used with TRANSITIVE verbs (vs ている which can be intransitive)
 * - Focuses on the resulting state of the object
 * - Implies the action was done intentionally for future purpose
 * - Object is marked with が (not を) since it's now the subject
 *
 * Grammar structure:
 * - Transitive verb in te-form (連用形 + て/で SCONJ)
 * - ある auxiliary verb (VERB with lemma=ある)
 * - Optional: ます (AUX) for polite form
 * - Optional: た (AUX) for past tense (てあった)
 *
 * GiNZA parses this with varying dependencies:
 * - Standard: VERB(連用形, dep=fixed/advcl) + SCONJ(て, dep=mark) + VERB(ある, dep=fixed/root/acl)
 * - Suru-verbs: AUX(する, 連用形) + SCONJ(て, dep=mark) + VERB(ある, dep=*)
 *
 * IMPORTANT: We exclude common intransitive verbs via negative constraints
 * because てある is ONLY used with transitive verbs
 */
export default linguisticRule('てある', (r) => {
  // Common intransitive verbs that should NOT match てある
  const intransitiveLemmas = [
    'ある',      // exist (inanimate)
    'いる',      // exist (animate)
    'くる',      // come
    'いく',      // go
    'できる',    // be able to
    '見える',    // be visible
    '聞こえる',  // be audible
    '起きる',    // get up / happen (intransitive)
    '続く',      // continue (intransitive)
    '始まる',    // begin (intransitive)
    '終わる',    // end (intransitive)
    '開く',      // open (intransitive - e.g. ドアが開く)
    '閉まる',    // close (intransitive)
    'つく',      // turn on (intransitive)
    '消える',    // turn off / disappear (intransitive)
    '壊れる',    // break (intransitive)
    '落ちる',    // fall (intransitive)
    '止まる',    // stop (intransitive)
    '死ぬ',      // die (intransitive)
    '残る',      // remain (intransitive)
  ];

  r.either(
    // Pattern 1: Standard form (てある) - normal verbs
    // e.g., 置いてある, 開けてある, 書いてある
    // GiNZA: verb(dep=fixed/advcl/root) + て(dep=mark) + ある(dep=fixed/root)
    (b1) => {
      const verb = b1.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b1.verb({
        lemma: 'ある',
        depOneOf: ['fixed', 'root', 'acl'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般']
      }, 'aru');

      // Exclude intransitive verbs via not() clause
      // Use the same variable name to check the already-bound verb
      b1.not((nb) => {
        nb.verb({ lemmaOneOf: intransitiveLemmas }, 'verb');
      });

      b1.inOrder(verb, te, 1);
      b1.inOrder(te, aru, 1);
      b1.captureSpan('てある', verb, aru);
    },

    // Pattern 1b: Standard form (てある) - suru-verbs (noun+する)
    // e.g., 準備してある, 考えてある, 作ってある
    // GiNZA: AUX(する, 連用形) + SCONJ(て, dep=mark) + VERB(ある, dep=*)
    (b1b) => {
      const verb = b1b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b1b.verb({
        lemma: 'ある',
        depOneOf: ['fixed', 'root', 'acl'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般']
      }, 'aru');

      b1b.inOrder(verb, te, 1);
      b1b.inOrder(te, aru, 1);
      b1b.captureSpan('てある', verb, aru);
    },

    // Pattern 2: Polite form (てあります)
    // e.g., 置いてあります, 開けてあります
    (b2) => {
      const verb = b2.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b2.verb({ lemma: 'ある', depOneOf: ['fixed', 'root', 'acl'], inflectionForm: '連用形-一般' }, 'aru');
      const masu = b2.aux({ lemma: 'ます', dep: 'aux' }, 'masu');

      // Exclude intransitive verbs via not() clause
      // Use the same variable name to check the already-bound verb
      b2.not((nb) => {
        nb.verb({ lemmaOneOf: intransitiveLemmas }, 'verb');
      });

      b2.inOrder(verb, te, 1);
      b2.inOrder(te, aru, 1);
      b2.inOrder(aru, masu, 1);
      b2.auxOf(verb, masu);
      b2.captureSpan('てある', verb, masu);
    },

    // Pattern 2b: Polite form (てあります) - suru-verbs
    // e.g., 準備してあります, 考えてあります
    (b2b) => {
      const verb = b2b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b2b.verb({ lemma: 'ある', depOneOf: ['fixed', 'root', 'acl'], inflectionForm: '連用形-一般' }, 'aru');
      const masu = b2b.aux({ lemma: 'ます', dep: 'aux' }, 'masu');

      b2b.inOrder(verb, te, 1);
      b2b.inOrder(te, aru, 1);
      b2b.inOrder(aru, masu, 1);
      b2b.captureSpan('てある', verb, masu);
    },

    // Pattern 3: Past form (てあった)
    // e.g., 置いてあった, 開けてあった, 貼ってあった
    // GiNZA: verb(dep=advcl/acl) + て(dep=mark) + ある(dep=fixed/acl, 連用形) + た(dep=aux)
    (b3) => {
      const verb = b3.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b3.verb({ lemma: 'ある', depOneOf: ['fixed', 'acl'], inflectionFormOneOf: ['連用形-一般', '連用形-促音便'] }, 'aru');
      const ta = b3.aux({ lemma: 'た', dep: 'aux' }, 'ta');

      // Exclude intransitive verbs via not() clause
      // Use the same variable name to check the already-bound verb
      b3.not((nb) => {
        nb.verb({ lemmaOneOf: intransitiveLemmas }, 'verb');
      });

      b3.inOrder(verb, te, 1);
      b3.inOrder(te, aru, 1);
      b3.inOrder(aru, ta, 1);
      b3.captureSpan('てある', verb, ta);
    },

    // Pattern 3b: Past form (てあった) - suru-verbs
    // e.g., 準備してあった, 考えてあった
    (b3b) => {
      const verb = b3b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const aru = b3b.verb({ lemma: 'ある', depOneOf: ['fixed', 'acl'], inflectionFormOneOf: ['連用形-一般', '連用形-促音便'] }, 'aru');
      const ta = b3b.aux({ lemma: 'た', dep: 'aux' }, 'ta');

      b3b.inOrder(verb, te, 1);
      b3b.inOrder(te, aru, 1);
      b3b.inOrder(aru, ta, 1);
      b3b.captureSpan('てある', verb, ta);
    }
  );
});
