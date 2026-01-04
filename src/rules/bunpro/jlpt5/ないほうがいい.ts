import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: ないほうがいい (Verb[nai] + hou + ga + ii - had better not)
 *
 * Matches verb negative form + ほう + が + いい, meaning "had better not", "it's better not to"
 * This is the negative advice form, suggesting that a certain action is the worse choice.
 *
 * Structure:
 * - Verb[未然形] + ない (negative auxiliary)
 * - ほう (noun: way/direction)
 * - が (subject particle)
 * - いい (adjective: good) - optionally followed by です (polite)
 *
 * Examples:
 * - 行かないほうがいい (had better not go)
 * - 食べないほうがいい (had better not eat)
 * - しないほうがいいです (had better not do - polite)
 *
 * This rule should NOT match:
 * - Positive advice form (たほうがいい)
 * - Simple negative (～ない)
 * - ほうがいい without verb negative (e.g., このほうがいい)
 */
export default linguisticRule('ないほうがいい', (r) => {
  r.either(
    // Branch 1: Standard parsing (verb + ない as auxiliary)
    // Example: 食べないほうがいい, 吸わないほうがいい, たべないほうがいい
    (b1) => {
      const verb = b1.verb(
        {
          inflectionForm: '未然形-一般',
        },
        'verb'
      );

      // nai can be pos=AUX (standard) or pos=ADJ/dep=aux (GiNZA inconsistency)
      const nai = b1.tok(
        {
          lemma: 'ない',
          dep: 'aux',
          posOneOf: ['AUX', 'ADJ'],
        },
        'nai'
      );

      const hou = b1.tok(
        {
          lemma: 'ほう',
          pos: 'NOUN',
        },
        'hou'
      );

      const ga = b1.particle('が', 'ga');

      const ii = b1.tok(
        {
          lemmaOneOf: ['いい', 'よい'],
          pos: 'ADJ',
        },
        'ii'
      );

      // Optional: polite copula です
      b1.optional((opt) => {
        const desu = opt.aux(
          {
            lemma: 'です',
          },
          'desu'
        );
        opt.auxOf(ii, desu);
      });

      // Structural constraints
      b1.auxOf(verb, nai);
      b1.headChild(hou, verb, 'acl');
      b1.caseMarker(hou, ga);
      b1.headChild(ii, hou, 'nsubj');

      // Order constraints
      b1.inOrder(verb, nai, 1);
      b1.inOrder(nai, hou, 1);
      b1.inOrder(hou, ga, 1);
      b1.inOrder(ga, ii, 1);

      b1.captureSpan('ないほうがいい', verb, ii);
    },
    // Branch 2: GiNZA alternative parsing (ichiran verbs split incorrectly)
    // Example: 汚れないほうがいい, まがらないほうがいい
    // Here: stem (NOUN) + ない (ADJ/acl), ない directly modifies ほう
    (b2) => {
      const stem = b2.tok(
        {
          inflectionForm: '未然形-一般',
          posOneOf: ['VERB', 'NOUN'],
        },
        'stem'
      );

      const nai = b2.tok(
        {
          lemma: 'ない',
          pos: 'ADJ',
          dep: 'acl',
        },
        'nai'
      );

      const hou = b2.tok(
        {
          lemma: 'ほう',
          pos: 'NOUN',
        },
        'hou'
      );

      const ga = b2.particle('が', 'ga');

      const ii = b2.tok(
        {
          lemmaOneOf: ['いい', 'よい'],
          pos: 'ADJ',
        },
        'ii'
      );

      // Optional: polite copula です
      b2.optional((opt) => {
        const desu = opt.aux(
          {
            lemma: 'です',
          },
          'desu'
        );
        opt.auxOf(ii, desu);
      });

      // Structural constraints
      // nai --acl--> hou (nai modifies hou directly)
      b2.headChild(hou, nai, 'acl');
      // ga --case--> hou (ga is case marker for hou)
      b2.caseMarker(hou, ga);
      // hou --nsubj--> ii (hou is subject of ii)
      b2.headChild(ii, hou, 'nsubj');
      // stem comes before nai (no specific dep required due to GiNZA inconsistency)

      // Order constraints
      b2.inOrder(stem, nai, 1);
      b2.inOrder(nai, hou, 1);
      b2.inOrder(hou, ga, 1);
      b2.inOrder(ga, ii, 1);

      b2.captureSpan('ないほうがいい', stem, ii);
    }
  );
});
