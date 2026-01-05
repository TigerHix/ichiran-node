import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('って', (r) => {
  // Match casual quotation particle って (quotative marker)
  // This is the casual version of と (quotation)
  // Used in casual speech before verbs like 言う, 聞く, 思う, 書く

  const tte = r.particle('って', 'tte');

  // Unlike the formal と, casual って has broader usage and less strict parsing
  // The key is that って appears between quoted content and a speech/thought verb
  // We match って when followed by communication verbs (言う, 聞く, 書く, 思う)

  r.either(
    // Pattern 1: Before 言う (say) and its conjugations
    (r1) => {
      const iu = r1.tok({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      r1.inOrder(tte, iu, 3); // って within 3 tokens of 言う
      r1.capture(tte);
    },
    // Pattern 2: Before 聞く (ask/hear)
    (r2) => {
      const kiku = r2.tok({ lemmaOneOf: ['聞く', 'きく'] }, 'kiku');
      r2.inOrder(tte, kiku, 3);
      r2.capture(tte);
    },
    // Pattern 3: Before 書く (write)
    (r3) => {
      const kaku = r3.tok({ lemmaOneOf: ['書く', 'かく'] }, 'kaku');
      r3.inOrder(tte, kaku, 3);
      r3.capture(tte);
    },
    // Pattern 4: Before 思う (think)
    (r4) => {
      const omou = r4.tok({ lemmaOneOf: ['思う', 'おもう'] }, 'omou');
      r4.inOrder(tte, omou, 3);
      r4.capture(tte);
    },
    // Pattern 5: At end of sentence (verb omitted - common in casual speech)
    // Example: トムは「今日休む」って。
    // Must have quotation brackets 「 or 」 to distinguish from hearsay (んだって)
    (r5) => {
      const quoteMark = r5.tok({ textOneOf: ['「', '」'] }, 'quoteMark');
      const period = r5.tok({ text: '。' }, 'period');
      r5.inOrder(quoteMark, tte, 10); // って after quote mark
      r5.inOrder(tte, period, 5); // って before period
      r5.capture(tte);
    }
  );
});
