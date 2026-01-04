import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('でよければ', (r) => {
  // でよければ - "if X is acceptable/if X works"
  // Pattern: Noun + で + よければ (conditional form of いい)
  //
  // Examples:
  // - この傘でよければお貸しできます。
  // - 私でよければ手伝うよ。
  // - 明日でよければ、あの本を持ってきますよ。
  //
  // The で is a case particle meaning "with/by/using"
  // よければ is the conditional form (仮定形) of the adjective いい

  // The particle で (case marking particle, means "with/by/using")
  const de = r.particle('で', 'de');

  // The conditional form of いい: よければ
  // GiNZA parses this as lemma=いい or よい with inflectionForm=仮定形-一般
  const yokereba = r.adj({
    lemmaOneOf: ['いい', 'よい'],
    inflectionForm: '仮定形-一般',
  }, 'yokereba');

  // Ensure they appear together with at most 1 token between them
  r.inOrder(de, yokereba, 1);

  // Capture the span from で through よければ
  r.captureSpan('でよければ', de, yokereba);
});
