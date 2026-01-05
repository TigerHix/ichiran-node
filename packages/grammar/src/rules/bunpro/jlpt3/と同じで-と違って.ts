import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と同じで-と違って', (r) => {
  // と同じで・と違って (to onaji de / to chigatte) - "same as" / "different from"
  //
  // Two patterns for comparison:
  // 1. Noun + と同じで (same as / like)
  // 2. Noun + と違って (different from / unlike)
  //
  // Examples:
  // - 彼は僕と同じで、猫アレルギーです。(Just like me, he is allergic to cats.)
  // - 私は弟と違って、本を読むのが大好きです。(Unlike my brother, I love to read books.)
  // - フィンランドと違って、ハワイは一年中暖かい。(Unlike Finland, Hawaii is warm year round.)
  // - 先生と同じで、僕も日本語が大好きです。(Like my teacher, I love Japanese.)
  //
  // GiNZA parsing notes:
  // - と同じで: Noun/PROPN + particle と + ADJ 同じ (lemma=同じ) + AUX で (lemma=だ)
  // - と違って: Noun/PROPN + particle と + VERB 違っ (lemma=違う) + SCONJ て (lemma=て)
  // - 同じ is always ADJ with tag 形状詞-一般
  // - 違っ is always VERB with tag 動詞-一般 and lemma=違う
  // - The て form after 違っ is SCONJ (助詞-接続助詞) with lemma=て

  r.either(
    // Pattern 1: Noun + と同じで (same as / like)
    // 彼は僕と同じで (kanji form)
    // 彼は僕とおなじで (hiragana form)
    // 先生と同じで
    // 空気と同じで
    // 君と同じで
    //
    // NOTE: GiNZA inconsistently parses で as either AUX (copula te-form) or ADP (case marker).
    // When followed by comma、, it's usually AUX with dep=aux/cop.
    // When not followed by comma, it's often ADP with dep=case.
    // We match both by using posOneOf.
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const to = b.particle('と', 'to');
      const onaji = b.tok({
        pos: 'ADJ',
        tag: '形状詞-一般',
        lemmaOneOf: ['同じ', 'おなじ'],
      }, 'onaji');
      const de = b.tok({
        posOneOf: ['AUX', 'ADP'],
        lemma: 'だ',
        text: 'で',
      }, 'de');

      b.inOrder(noun, to, 1);
      b.inOrder(to, onaji, 1);
      b.inOrder(onaji, de, 1);
      b.captureSpan('と同じで', noun, de);
    },

    // Pattern 2: Noun + と違って (different from / unlike)
    // 私は弟と違って (kanji form)
    // 私は弟とちがって (hiragana form)
    // フィンランドと違って
    // 他人と違って
    // 大統領と違って
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const to = b.particle('と', 'to');
      const chigat = b.verb({
        lemmaOneOf: ['違う', 'ちがう'],
        tag: '動詞-一般',
      }, 'chigat');
      const te = b.tok({
        pos: 'SCONJ',
        lemma: 'て',
      }, 'te');

      b.inOrder(noun, to, 1);
      b.inOrder(to, chigat, 1);
      b.inOrder(chigat, te, 1);
      b.captureSpan('と違って', noun, te);
    }
  );
});
