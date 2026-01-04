import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('くせに', (r) => {
  // くせに - "despite/even though" with critical/complaining nuance
  // Patterns:
  // 1. Verb/Adj + くせに (direct attachment)
  // 2. Noun + のくせに (no + kuse + ni)
  // 3. な-adj + なくせに (na + kuse + ni)
  //
  // くせ is a noun (癖) + case marker に, used as a conjunction
  // It attaches to attributive forms and expresses criticism/contempt
  //
  // GiNZA may tokenize くせに as either:
  // - Two tokens: くせ/癖 (noun) + に (particle)
  // - Single token: くせに (ADP/SCONJ)

  r.either(
    // Pattern 7: Special case - がる verb suffix + くせ merged as なくせ
    // This handles cases like "暑がりなくせに" where がる + な + くせ becomes がる + なくせ
    (b) => {
      const garu = b.tok({ lemmaOneOf: ['がる', 'がり'], tag: '接尾辞-動詞的' }, 'garu');
      const nakuse = b.tok({ text: 'なくせ', pos: 'NOUN' }, 'nakuse');
      b.inOrder(garu, nakuse, 10);
      const ni = b.particle('に', 'ni');
      b.inOrder(nakuse, ni, 1);
      b.captureSpan('くせに', garu, ni);
    }
  );
});
