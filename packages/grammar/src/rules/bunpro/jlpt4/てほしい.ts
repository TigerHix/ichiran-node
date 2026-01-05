import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てほしい (verb[te] + ほしい)
 *
 * Expresses desire for someone else to do something.
 * "I want you to do ~" / "I want someone to do ~"
 *
 * Examples:
 * - してほしい (I want you to do)
 * - 買ってほしい (I want you to buy)
 * - 来てほしい (I want you to come)
 * - 来てほしかった (I wanted you to come)
 * - 待っていてほしい (I want you to keep waiting)
 * - 読んでほしいです (I want you to read [polite])
 *
 * Key characteristics:
 * - Verb in te-form (連用形 + て/で)
 * - ほしい as auxiliary (lemma=ほしい)
 * - Different from がほしい which follows nouns
 * - Expresses desire for someone ELSE's action (not your own)
 *
 * Grammar structure:
 * - Verb in te-form (連用形 + て/で SCONJ)
 * - ほしい auxiliary (AUX with lemma=ほしい)
 * - Optional: です (AUX) for polite form
 * - Optional: た (AUX) for past tense (ほしかった)
 * - Optional: ない (ADJ) for negative (ほしくない)
 *
 * GiNZA parses this as:
 * - VERB/AUX(連用形) + SCONJ(て/で, dep=mark) + AUX(ほしい, dep=fixed/root)
 * - For suru-verbs: NOUN + AUX(する, 連用形) + SCONJ(て) + AUX(ほしい)
 * - です can have dep=aux or dep=fixed depending on context
 */
export default bunproLinguisticRule('てほしい', (r) => {
  r.either(
    // Pattern 1a: Present positive (てほしい) - regular verbs
    // e.g., してほしい, 買ってほしい, 来てほしい
    // GiNZA: verb(連用形) + て/で(mark) + ほしい(fixed/root, 終止形-一般)
    (b1a) => {
      const verb = b1a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b1a.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b1a.inOrder(verb, te, 1);
      b1a.inOrder(te, hoshii, 1);
      b1a.captureSpan('てほしい', verb, hoshii);
    },

    // Pattern 1b: Present positive (てほしい) - suru-verbs (noun+する)
    // e.g., 勉強してほしい, 説明してほしい
    // GiNZA: AUX(する, 連用形) + SCONJ(て, dep=mark) + AUX(ほしい, 終止形-一般)
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
      const hoshii = b1b.aux({
        lemma: 'ほしい',
        inflectionForm: '終止形-一般',
      }, 'hoshii');

      b1b.inOrder(verb, te, 1);
      b1b.inOrder(te, hoshii, 1);
      b1b.captureSpan('てほしい', verb, hoshii);
    },

    // Pattern 2a: Polite form (てほしいです) - regular verbs
    // e.g., してほしいです, 読んでほしいです
    (b2a) => {
      const verb = b2a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b2a.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b2a.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b2a.inOrder(verb, te, 1);
      b2a.inOrder(te, hoshii, 1);
      b2a.inOrder(hoshii, desu, 1);
      b2a.captureSpan('てほしい', verb, desu);
    },

    // Pattern 2b: Polite form (てほしいです) - suru-verbs
    // e.g., 勉強してほしいです
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
      const hoshii = b2b.aux({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-一般', '連体形-一般'],
      }, 'hoshii');
      const desu = b2b.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b2b.inOrder(verb, te, 1);
      b2b.inOrder(te, hoshii, 1);
      b2b.inOrder(hoshii, desu, 1);
      b2b.captureSpan('てほしい', verb, desu);
    },

    // Pattern 3a: Past positive (てほしかった) - regular verbs
    // e.g., 来てほしかった, してほしかった
    (b3a) => {
      const verb = b3a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b3a.tok({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-促音便', '連用形-一般'],
      }, 'hoshii');
      const ta = b3a.aux({ lemma: 'た', dep: 'aux' }, 'ta');

      b3a.inOrder(verb, te, 1);
      b3a.inOrder(te, hoshii, 1);
      b3a.inOrder(hoshii, ta, 1);
      b3a.captureSpan('てほしい', verb, ta);
    },

    // Pattern 3b: Past positive (てほしかった) - suru-verbs
    // e.g., 勉強してほしかった
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
      const hoshii = b3b.tok({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-促音便', '連用形-一般'],
      }, 'hoshii');
      const ta = b3b.aux({ lemma: 'た', dep: 'aux' }, 'ta');

      b3b.inOrder(verb, te, 1);
      b3b.inOrder(te, hoshii, 1);
      b3b.inOrder(hoshii, ta, 1);
      b3b.captureSpan('てほしい', verb, ta);
    },

    // Pattern 4a: Past polite (てほしかったです) - regular verbs
    // e.g., 来てほしかったです
    (b4a) => {
      const verb = b4a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b4a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b4a.tok({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-促音便', '連用形-一般'],
      }, 'hoshii');
      const ta = b4a.aux({ lemma: 'た', dep: 'aux' }, 'ta');
      const desu = b4a.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b4a.inOrder(verb, te, 1);
      b4a.inOrder(te, hoshii, 1);
      b4a.inOrder(hoshii, ta, 1);
      b4a.inOrder(ta, desu, 1);
      b4a.captureSpan('てほしい', verb, desu);
    },

    // Pattern 4b: Past polite (てほしかったです) - suru-verbs
    // e.g., 勉強してほしかったです
    (b4b) => {
      const verb = b4b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b4b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshii = b4b.tok({
        lemma: 'ほしい',
        inflectionFormOneOf: ['連用形-促音便', '連用形-一般'],
      }, 'hoshii');
      const ta = b4b.aux({ lemma: 'た', dep: 'aux' }, 'ta');
      const desu = b4b.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b4b.inOrder(verb, te, 1);
      b4b.inOrder(te, hoshii, 1);
      b4b.inOrder(hoshii, ta, 1);
      b4b.inOrder(ta, desu, 1);
      b4b.captureSpan('てほしい', verb, desu);
    },

    // Pattern 5a: Present negative (てほしくない) - regular verbs
    // e.g., してほしくない
    (b5a) => {
      const verb = b5a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b5a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b5a.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b5a.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b5a.inOrder(verb, te, 1);
      b5a.inOrder(te, hoshiku, 1);
      b5a.inOrder(hoshiku, nai, 1);
      b5a.captureSpan('てほしい', verb, nai);
    },

    // Pattern 5b: Present negative (てほしくない) - suru-verbs
    // e.g., 勉強してほしくない
    (b5b) => {
      const verb = b5b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b5b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b5b.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b5b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b5b.inOrder(verb, te, 1);
      b5b.inOrder(te, hoshiku, 1);
      b5b.inOrder(hoshiku, nai, 1);
      b5b.captureSpan('てほしい', verb, nai);
    },

    // Pattern 6a: Present negative polite (てほしくないです) - regular verbs
    // e.g., してほしくないです
    (b6a) => {
      const verb = b6a.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b6a.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b6a.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b6a.adj({
        lemma: 'ない',
        inflectionForm: '連用形-一般',
      }, 'nai');
      const desu = b6a.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b6a.inOrder(verb, te, 1);
      b6a.inOrder(te, hoshiku, 1);
      b6a.inOrder(hoshiku, nai, 1);
      b6a.inOrder(nai, desu, 1);
      b6a.captureSpan('てほしい', verb, desu);
    },

    // Pattern 6b: Present negative polite (てほしくないです) - suru-verbs
    // e.g., 勉強してほしくないです
    (b6b) => {
      const verb = b6b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b6b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const hoshiku = b6b.aux({
        lemma: 'ほしい',
        inflectionForm: '連用形-一般',
      }, 'hoshiku');
      const nai = b6b.adj({
        lemma: 'ない',
        inflectionForm: '連用形-一般',
      }, 'nai');
      const desu = b6b.aux({ lemma: 'です', depOneOf: ['aux', 'fixed'] }, 'desu');

      b6b.inOrder(verb, te, 1);
      b6b.inOrder(te, hoshiku, 1);
      b6b.inOrder(hoshiku, nai, 1);
      b6b.inOrder(nai, desu, 1);
      b6b.captureSpan('てほしい', verb, desu);
    }
  );
});
