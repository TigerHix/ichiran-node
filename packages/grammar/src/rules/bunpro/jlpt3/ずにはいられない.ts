import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずにはいられない', (r) => {
  // ずにはいられない (zu ni wa irarenai) - "can't help but do something"
  // Double negative expression meaning "cannot be without doing X"
  // attaches to verb negative stem (未然形/mizenkei)
  //
  // Pattern: verb[negative stem] + ず + に + は + いられない
  //
  // Examples:
  //   食べずにはいられない (can't help but eat)
  //   泣かずにはいられない (can't help but cry)
  //   疑わずにはいられない (can't help but suspect)
  //   せずにはいられない (can't help but do - irregular する→せず)
  //
  // Key linguistic points:
  // - ず is the negative auxiliary (classical form of ぬ)
  // - に is a case particle
  // - は is the topic particle
  // - いられない is the negative potential of いる (can stay/be)
  //
  // GiNZA parsing:
  // - ず: AUX, dep=aux, attaches to verb
  // - に: ADP, dep=fixed, head points to ず
  // - は: ADP, dep=case, head points to main verb
  // - い: VERB, lemma=いる (main verb)
  // - られ: AUX, lemma=られる, dep=aux, head points to い
  // - ない: AUX, lemma=ない, dep=aux, head points to い

  r.either(
    // Branch 1: Standard parsing (most common)
    // ず (aux) + に (fixed) + は (case) + い (verb) + られ (aux) + ない (aux)
    (b) => {
      const zu = b.aux({
        text: 'ず',
        dep: 'aux',
      }, 'zu');
      const ni = b.tok({
        text: 'に',
        dep: 'fixed',
      }, 'ni');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
        dep: 'aux',
      }, 'rare');
      const nai = b.aux({
        lemma: 'ない',
        dep: 'aux',
      }, 'nai');

      b.inOrder(zu, ni, 2);
      b.inOrder(ni, wa, 5);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, nai);

      b.captureSpan('ずにはいられない', zu, nai);
    },

    // Branch 2: Polite form (いません)
    // ず + には + い + られ + ませ + ん
    // GiNZA parses いません as two tokens: ませ (lemma=ます) + ん (lemma=ぬ)
    (b) => {
      const zu = b.aux({
        text: 'ず',
      }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const mase = b.aux({
        lemma: 'ます',
      }, 'mase');
      const nu = b.aux({
        lemmaOneOf: ['ぬ', 'ない'],
      }, 'nu');

      b.inOrder(zu, ni, 2);
      b.inOrder(ni, wa, 5);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, mase);
      b.auxOf(iru, nu);

      b.captureSpan('ずにはいられない', zu, nu);
    },

    // Branch 3: More flexible particle dependencies
    // Some parsings may have different dep labels for に
    (b) => {
      const zu = b.tok({
        text: 'ず',
      }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const iru = b.verb({
        lemma: 'いる',
      }, 'iru');
      const rare = b.aux({
        lemma: 'られる',
      }, 'rare');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');

      b.inOrder(zu, ni, 2);
      b.inOrder(ni, wa, 5);
      b.inOrder(wa, iru, 5);
      b.auxOf(iru, rare);
      b.auxOf(iru, nai);

      b.captureSpan('ずにはいられない', zu, nai);
    },

    // Branch 4: Alternative where い is parsed as いられる (single token)
    // Less common but possible in some contexts
    (b) => {
      const zu = b.tok({
        text: 'ず',
      }, 'zu');
      const ni = b.particle('に', 'ni');
      const wa = b.particle('は', 'wa');
      const irarenai = b.aux({
        lemma: 'いられる',
      }, 'irarenai');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');

      b.inOrder(zu, ni, 2);
      b.inOrder(ni, wa, 5);
      b.inOrder(wa, irarenai, 5);
      b.auxOf(irarenai, nai);

      b.captureSpan('ずにはいられない', zu, nai);
    }
  );
});
