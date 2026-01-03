import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verb-にいく', (r) => {
  // Match verb stem + に + motion verb (go/come to do something)
  // e.g., たべにいく (go to eat), しにいきます (go to do)

  // GiNZA parses verb stems (masu form, 連用形-一般) inconsistently:
  // - たべ (from たべる): pos=NOUN, tag=動詞-一般
  // - み (from みる): pos=VERB, tag=動詞-非自立可能
  // - し (from する): pos=AUX, tag=動詞-非自立可能
  // - あそび (from あそぶ): pos=VERB, tag=名詞-普通名詞-一般
  // Use r.either() to handle all patterns
  r.either(
    // Pattern 1: NOUN with tag=動詞-一般 (e.g., たべ)
    (b) => {
      const purposeVerb = b.tok({ tag: '動詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({ lemmaOneOf: ['いく', 'くる', '来る'] }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 2: VERB with tag containing 動詞 (e.g., み, あそび)
    (b) => {
      const purposeVerb = b.verb({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({ lemmaOneOf: ['いく', 'くる', '来る'] }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 3: AUX with tag=動詞-非自立可能 (e.g., し from する)
    (b) => {
      const purposeVerb = b.aux({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({ lemmaOneOf: ['いく', 'くる', '来る'] }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 4: VERB with tag=名詞-普通名詞-一般 (verbal noun, e.g., あそび)
    (b) => {
      const purposeVerb = b.verb({ tag: '名詞-普通名詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({ lemmaOneOf: ['いく', 'くる', '来る'] }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    }
  );
});
