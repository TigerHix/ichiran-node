import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('から言うと', (r) => {
  // から言うと/から言えば/から言って - "from X's standpoint"
  // Pattern: noun + から + 言う(inflected) + conjunction particle
  //
  // Examples:
  // - 法律から言うと、それは違反です。
  // - 私の立場から言えば、彼は絶対いつか成功する。
  // - この結果から言って、この計画はあまりよくありませんでした。
  //
  // Note: GiNZA uses lemma="いう" (hiragana) regardless of surface form

  const kara = r.particle('から', 'kara');

  r.either(
    // Pattern 1: から言うと/からいうと (conditional form)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const to = b.tok({ text: 'と', dep: 'mark' }, 'to');
      b.inOrder(kara, iu, 1);
      b.inOrder(iu, to, 1);
      b.captureSpan('から言うと', kara, to);
    },
    // Pattern 2: から言えば/からいえば (conditional form with ば)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const ba = b.tok({ text: 'ば', dep: 'mark' }, 'ba');
      b.inOrder(kara, iu, 1);
      b.inOrder(iu, ba, 1);
      b.captureSpan('から言えば', kara, ba);
    },
    // Pattern 3: から言って/からいって (te-form)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const te = b.tok({ text: 'て', dep: 'mark' }, 'te');
      b.inOrder(kara, iu, 1);
      b.inOrder(iu, te, 1);
      b.captureSpan('から言って', kara, te);
    }
  );
});
