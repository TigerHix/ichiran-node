import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('にする', (r) => {
  // にする: to decide on / to choose (noun + ni/to + suru)
  // This expresses the speaker's choice/decision: "I'll have X", "Let's go with X"
  // Both にする (more definite/direct) and とする (softer/less pushy) are supported

  r.either(
    // Pattern 1: Standard/casual present (にする)
    (b) => {
      const particle = b.particle('に', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '終止形-一般' }, 'suru');
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, suru);
    },
    // Pattern 2: Standard/casual present (とする - softer variant)
    (b) => {
      const particle = b.particle('と', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '終止形-一般' }, 'suru');
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, suru);
    },
    // Pattern 3: Polite present (にします)
    (b) => {
      const particle = b.particle('に', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(suru, masu);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, masu);
    },
    // Pattern 4: Polite present (とします - softer variant)
    (b) => {
      const particle = b.particle('と', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.auxOf(suru, masu);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, masu);
    },
    // Pattern 5: Casual past (にした)
    (b) => {
      const particle = b.particle('に', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(suru, ta);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, ta);
    },
    // Pattern 6: Casual past (とした - softer variant)
    (b) => {
      const particle = b.particle('と', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(suru, ta);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, ta);
    },
    // Pattern 7: Polite past (にしました)
    (b) => {
      const particle = b.particle('に', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const mashita = b.aux({ lemma: 'ました' }, 'mashita');
      b.auxOf(suru, mashita);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, mashita);
    },
    // Pattern 8: Polite past (としました - softer variant)
    (b) => {
      const particle = b.particle('と', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const mashita = b.aux({ lemma: 'ました' }, 'mashita');
      b.auxOf(suru, mashita);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, mashita);
    },
    // Pattern 9: Desire (にしたい - "I want to choose")
    (b) => {
      const particle = b.particle('に', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const tai = b.aux({ lemma: 'たい' }, 'tai');
      b.auxOf(suru, tai);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, tai);
    },
    // Pattern 10: Desire (としたい - softer variant)
    (b) => {
      const particle = b.particle('と', 'particle');
      const suru = b.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      const tai = b.aux({ lemma: 'たい' }, 'tai');
      b.auxOf(suru, tai);
      b.inOrder(particle, suru, 1);
      b.captureSpan('にする', particle, tai);
    }
  );
});
