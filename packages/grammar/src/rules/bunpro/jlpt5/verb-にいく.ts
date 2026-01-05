import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('verb-にいく', (r) => {
  // Match verb stem + に + motion verb (go/come to do something)
  // e.g., たべにいく (go to eat), しにいきます (go to do)
  //
  // Both casual and polite forms:
  // - Casual: verb stem + に + いく/くる/来る
  // - Polite: verb stem + に + いき/き/き + ます

  // GiNZA parses verb stems (masu form, 連用形-一般) inconsistently:
  // - たべ (from たべる): pos=NOUN, tag=動詞-一般
  // - み (from みる): pos=NOUN, tag=名詞-普通名詞-一般
  // - し (from する): pos=AUX, tag=動詞-非自立可能
  // - あそび (from あそぶ): pos=NOUN, tag=名詞-普通名詞-一般
  // Use r.either() to handle all patterns

  r.either(
    // ===== CASUAL FORMS (～にいく/～にくる) =====

    // Pattern 1: NOUN with tag=動詞-一般 + casual motion verb (e.g., たべにいく)
    (b) => {
      const purposeVerb = b.tok({ tag: '動詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
        // NOT in polite form (not inflectionForm=連用形-一般)
      }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 2: NOUN with tag=名詞-普通名詞-一般 + casual motion verb (e.g., みにいく, あそびにいく)
    (b) => {
      const purposeVerb = b.tok({ tag: '名詞-普通名詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
      }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 3: VERB with tag=動詞-非自立可能 + casual motion verb
    (b) => {
      const purposeVerb = b.verb({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
      }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },
    // Pattern 4: AUX with tag=動詞-非自立可能 + casual motion verb (e.g., しにいく)
    (b) => {
      const purposeVerb = b.aux({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerb = b.verb({ lemmaOneOf: ['いく', 'くる', '来る'] }, 'motionVerb');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerb, 1);
      b.captureSpan('verb-にいく', purposeVerb, motionVerb);
    },

    // ===== POLITE FORMS (～にいきます/～にきます) =====

    // Pattern 5: NOUN with tag=動詞-一般 + polite motion verb (e.g., たべにいきます)
    (b) => {
      const purposeVerb = b.tok({ tag: '動詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般', // stem before ます
      }, 'motionVerbStem');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('verb-にいく', purposeVerb, masu);
    },
    // Pattern 6: NOUN with tag=名詞-普通名詞-一般 + polite motion verb (e.g., みにいきます, あそびにいきます)
    (b) => {
      const purposeVerb = b.tok({ tag: '名詞-普通名詞-一般' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('verb-にいく', purposeVerb, masu);
    },
    // Pattern 7: VERB with tag=動詞-非自立可能 + polite motion verb
    (b) => {
      const purposeVerb = b.verb({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('verb-にいく', purposeVerb, masu);
    },
    // Pattern 8: AUX with tag=動詞-非自立可能 + polite motion verb (e.g., しにいきます)
    (b) => {
      const purposeVerb = b.aux({ tag: '動詞-非自立可能' }, 'purposeVerb');
      const ni = b.particle('に', 'ni');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');

      b.inOrder(purposeVerb, ni, 1);
      b.inOrder(ni, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('verb-にいく', purposeVerb, masu);
    }
  );
});
