import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('って-slang', (r) => {
  // って (casual topic marker) - casual version of は
  // "As for, Speaking of"
  //
  // This is the casual topic marker usage of って, different from:
  // - Formal quotation marker と (e.g., 彼と言う - "called he")
  // - Hearsay だって (e.g., 全員合格だってよ - "I heard everyone passed")
  //
  // Pattern: [Noun/Topic] + って + [Predicate]
  //
  // GiNZA parsing notes:
  // - って as topic marker has pos=ADP, tag=助詞-副助詞, dep=case, lemma=って
  // - It follows the topic noun (PRON, NOUN, DET+NOUN compound)
  // - The って attaches to the topic noun (head points to the noun)
  // - Different from formal と which has pos=ADP, tag=助詞-格助詞, dep=case
  //
  // Examples:
  // - 私って皆に嫌われている？ (As for me, am I disliked by everyone?)
  // - トマトって野菜なの？ (Are tomatoes vegetables?)
  // - 先生って優しいね。(The teacher is kind, isn't she?)
  // - 冬って寒いけど、雪は綺麗だ。(Winter is cold, but snow is pretty.)
  // - ギターって弾けますか？(Can you play guitar?)
  //
  // Key discriminators:
  // - tag=助詞-副助詞 (adverbial particle, not 格助詞 case particle like と)
  // - dep=case (marks the topic as a case role)
  // - lemma=って (unique form)

  const tte = r.tok({
    text: 'って',
    lemma: 'って',
    pos: 'ADP',
    tag: '助詞-副助詞',
    dep: 'case',
  }, 'tte');

  // Capture just the って particle itself
  r.captureSpan('って-slang', tte, tte);
});
