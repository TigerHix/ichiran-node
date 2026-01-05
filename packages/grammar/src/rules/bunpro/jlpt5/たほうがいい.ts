import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たほうがいい', (r) => {
  // Match Verb[た] + ほう + が + いい/いいです (had better do, should do)
  // e.g., きいたほうがいい, かえったほうがいいです, たべたほうがいい
  // Also matches Verb[dictionary form] + ほう + が + いい/いいです (general opinion)
  // e.g., たべるほうがいい, かたづけるほうがいい

  // Pattern 1: Casual form (Verb[た] + ほうがいい)
  r.either(
    // Main pattern: VERB (stem) + た (past auxiliary) + ほう + が + いい
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-一般' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');

      // Require verb in past form (stem + た)
      b.auxOf(verbStem, ta);
      // Require sequential order with small gaps
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-イ音便 (e.g., きいた)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-イ音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');

      b.auxOf(verbStem, ta);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-促音便 (e.g., かえった)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-促音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');

      b.auxOf(verbStem, ta);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-撥音便 (e.g., すんだ)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-撥音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');

      b.auxOf(verbStem, ta);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Dictionary form pattern (e.g., たべるほうがいい, かたづけるほうがいい)
    (b) => {
      const verb = b.tok({
        inflectionForm: '連体形-一般',
        pos: 'VERB', // Only VERB, not AUX (excludes ない which is AUX)
        textRe: /^(?!ない$)/ // Exclude auxiliary "ない" (negative form has separate rule: ないほうがいい)
      }, 'verb');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');

      b.inOrder(verb, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verb, ii);
    }
  );

  // Pattern 2: Polite form (Verb[た] + ほうがいいです)
  r.either(
    // Main pattern: VERB (stem) + た (past auxiliary) + ほう + が + いい + です
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-一般' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(verbStem, ta);
      b.auxOf(ii, desu);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-イ音便 (e.g., きいた)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-イ音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(verbStem, ta);
      b.auxOf(ii, desu);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-促音便 (e.g., かえった)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-促音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(verbStem, ta);
      b.auxOf(ii, desu);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Pattern with 連用形-撥音便 (e.g., すんだ)
    (b) => {
      const verbStem = b.verb({ inflectionForm: '連用形-撥音便' }, 'verbStem');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'], conjugationClass: '助動詞-タ' }, 'ta');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.auxOf(verbStem, ta);
      b.auxOf(ii, desu);
      b.inOrder(ta, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);

      b.captureSpan('たほうがいい', verbStem, ii);
    },
    // Dictionary form pattern (e.g., たべるほうがいいです)
    (b) => {
      const verb = b.tok({
        inflectionForm: '連体形-一般',
        pos: 'VERB', // Only VERB, not AUX (excludes ない which is AUX)
        textRe: /^(?!ない$)/ // Exclude auxiliary "ない" (negative form has separate rule: ないほうがいい)
      }, 'verb');
      const hou = b.noun({ lemmaOneOf: ['ほう', '方'] }, 'hou');
      const ga = b.particle('が', 'ga');
      const ii = b.adj({ lemma: 'いい' }, 'ii');
      const desu = b.aux({ lemma: 'です' }, 'desu');

      b.inOrder(verb, hou, 2);
      b.inOrder(hou, ga, 1);
      b.inOrder(ga, ii, 1);
      b.auxOf(ii, desu);

      b.captureSpan('たほうがいい', verb, ii);
    }
  );
});
