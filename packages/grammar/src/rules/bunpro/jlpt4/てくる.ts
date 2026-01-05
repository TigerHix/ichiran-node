import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('てくる', (r) => {
  // Verb[te-form] + くる/きます (to come doing, starting to, change toward now)
  // Examples: 歩いてくる, 食べてくる, 増えてくる, なってきた
  //
  // Meaning:
  // - "come doing": go and come back (行ってきます = go and come back)
  // - "starting to": change from past toward now (太ってきた = has come to be fat)
  // - "bring": bring something (持ってくる = bring)
  //
  // GiNZA parses as:
  //   VERB (stem in conjunctive form) + SCONJ (て/で) + VERB/AUX (くる)
  //   Examples:
  //     かえってくる → かえ [VERB, 連用形-一般] + って [SCONJ, dep=mark] + くる [VERB, dep=aux]
  //     なってきた → な [VERB, 連用形-一般] + って [SCONJ, dep=mark] + き [AUX, dep=aux] + た [AUX, dep=aux]
  //     いってきます → い [VERB, 連用形-一般] + って [SCONJ, dep=mark] + き [AUX, dep=aux] + ま [AUX, dep=aux] + す [AUX, dep=aux]
  //
  // Key discriminators vs ていく:
  //   てくる ( toward speaker/now) uses くる
  //   ていく ( away from speaker/into future) uses いく
  //   The lemma=くる vs lemma=いく distinguishes them

  // Match: any verb te-form + くる (polite or casual, various conjugations)
  // GiNZA parses て/で variably - sometimes SCONJ, sometimes ADP
  // We rely on text + structural pattern to distinguish from other て-forms
  const verb = r.tok({}, 'verb');
  const te = r.tok({ textOneOf: ['て', 'で'] }, 'te');
  const kuru = r.tok({
    lemmaOneOf: ['くる', '来る'],
    posOneOf: ['VERB', 'AUX']
  }, 'kuru');

  r.inOrder(verb, te, 1);
  r.inOrder(verb, kuru);
  r.inOrder(te, kuru, 3);
  r.captureSpan('てくる', verb, kuru);
});
