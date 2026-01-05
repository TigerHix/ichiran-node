import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('causative-passive', (r) => {
  // Causative-passive form: Verb + せられる/される (casual) or せられます/されます (polite)
  // Meaning: To be made/forced to do something (usually against one's will)
  //
  // Examples:
  // - させる + られる = させられる (full form)
  // - せる + られる = せられる (full form for godan verbs)
  //
  // Conjugations:
  // - Present: させられる, せられる
  // - Past: させられた, せられた
  // - Te-form: させられて, せられて
  // - Polite: させられます, せられます
  //
  // GiNZA parsing shows:
  // - Verb stem: VERB/AUX with inflectionForm 未然形-一般, 連用形-一般, or 未然形-サ
  // - せる/させる: AUX, lemma=せる/させる, inflectionForm=未然形-一般, dep=aux
  // - られる: AUX, lemma=られる, inflectionForm=連用形-一般 or 終止形-一般, dep=aux
  // - Optional auxiliaries attach: た, て, ます all attach to the chain
  //
  // Key insight: Both せる/させる and られる attach to the verb with dep=aux

  r.either(
    // Branch 1: Present tense with させる (ichidan verbs, suru-verbs, irregulars)
    // Example: 食べさせる + られる = 食べさせられる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '終止形-一般',
      }, 'reru');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.captureSpan('causative-passive', verb, reru);
    },

    // Branch 2: Present tense with せる (godan verbs)
    // Example: 飲む + せる + られる = 飲ませられる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '終止形-一般',
      }, 'reru');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.captureSpan('causative-passive', verb, reru);
    },

    // Branch 3: Present tense (連体形) with させる
    // Example: 嫌なことをさせられる (before nominalizer の)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連体形-一般',
      }, 'reru');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.captureSpan('causative-passive', verb, reru);
    },

    // Branch 4: Present tense (連体形) with せる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連体形-一般',
      }, 'reru');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.captureSpan('causative-passive', verb, reru);
    },

    // Branch 5: Past tense with させる
    // Example: 食べさせられた
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '終止形-一般' }, 'ta');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.auxOf(verb, ta);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    },

    // Branch 6: Past tense with せる
    // Example: 飲ませられた
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '終止形-一般' }, 'ta');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.auxOf(verb, ta);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    },

    // Branch 7: Te-form with させる
    // Example: させられて
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, te, 1);
      b.captureSpan('causative-passive', verb, te);
    },

    // Branch 8: Te-form with せる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, te, 1);
      b.captureSpan('causative-passive', verb, te);
    },

    // Branch 9: Progressive (te-form + iru) with させる
    // Example: させられていた
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const iru = b.aux({
        lemmaOneOf: ['いる', 'る'],
        inflectionForm: '連用形-一般',
      }, 'iru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '終止形-一般' }, 'ta');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.auxOf(verb, iru);
      b.auxOf(verb, ta);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, te, 1);
      b.inOrder(te, iru, 3);
      b.inOrder(iru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    },

    // Branch 10: Progressive (te-form + iru) with せる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const iru = b.aux({
        lemmaOneOf: ['いる', 'る'],
        inflectionForm: '連用形-一般',
      }, 'iru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '終止形-一般' }, 'ta');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.auxOf(verb, iru);
      b.auxOf(verb, ta);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, te, 1);
      b.inOrder(te, iru, 3);
      b.inOrder(iru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    },

    // Branch 11: Polite with させる
    // Example: させられます
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const masu = b.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'masu');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.auxOf(verb, masu);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, masu, 1);
      b.captureSpan('causative-passive', verb, masu);
    },

    // Branch 12: Polite with せる
    // Example: 払わせられます
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const masu = b.aux({ lemma: 'ます', inflectionForm: '終止形-一般' }, 'masu');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.auxOf(verb, masu);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, masu, 1);
      b.captureSpan('causative-passive', verb, masu);
    },

    // Branch 13: Past polite with させる
    // Example: させられました
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const mashita = b.aux({ lemma: 'ます' }, 'mashita');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.auxOf(verb, mashita);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, mashita, 1);
      b.captureSpan('causative-passive', verb, mashita);
    },

    // Branch 14: Past polite with せる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const mashita = b.aux({ lemma: 'ます' }, 'mashita');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.auxOf(verb, mashita);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, mashita, 1);
      b.captureSpan('causative-passive', verb, mashita);
    },

    // Branch 15: Te-form + ので (nominalized clause)
    // Example: させられたのです
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const saseru = b.aux({ lemma: 'させる', inflectionForm: '未然形-一般' }, 'saseru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '連体形-一般' }, 'ta');

      b.auxOf(verb, saseru);
      b.auxOf(verb, reru);
      b.auxOf(verb, ta);
      b.inOrder(verb, saseru, 3);
      b.inOrder(saseru, reru, 1);
      b.inOrder(reru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    },

    // Branch 16: Te-form + ので with せる
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const seru = b.aux({ lemma: 'せる', inflectionForm: '未然形-一般' }, 'seru');
      const reru = b.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'reru');
      const ta = b.aux({ lemma: 'た', inflectionForm: '連体形-一般' }, 'ta');

      b.auxOf(verb, seru);
      b.auxOf(verb, reru);
      b.auxOf(verb, ta);
      b.inOrder(verb, seru, 3);
      b.inOrder(seru, reru, 1);
      b.inOrder(reru, ta, 1);
      b.captureSpan('causative-passive', verb, ta);
    }
  );
});
