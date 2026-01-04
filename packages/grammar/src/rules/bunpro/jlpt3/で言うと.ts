import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('で言うと', (r) => {
  // で言うと/でいうと/で言えば/でいえば - "in terms of/speaking of"
  // Pattern: noun + で + 言う(inflected) + conjunction particle
  //
  // Examples:
  // - 統計で言うと、客の７０％が観光客だ。
  // - 日本でいうとパンみたいなもの？
  // - 一言で言うと彼は本当に凄い人だ。
  // - 西暦の２０１９年は、令和で言うと元年に当たります。
  //
  // Note: GiNZA uses lemma="いう" (hiragana) regardless of surface form

  const de = r.particle('で', 'de');

  r.either(
    // Pattern 1: で言うと/でいうと (conditional form)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const to = b.tok({ text: 'と', dep: 'mark' }, 'to');
      b.inOrder(de, iu, 1);
      b.inOrder(iu, to, 1);
      b.captureSpan('で言うと', de, to);
    },
    // Pattern 2: で言えば/でいえば (conditional form with ば)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const ba = b.tok({ text: 'ば', dep: 'mark' }, 'ba');
      b.inOrder(de, iu, 1);
      b.inOrder(iu, ba, 1);
      b.captureSpan('で言えば', de, ba);
    },
    // Pattern 3: で言って/でいって (te-form)
    (b) => {
      const iu = b.verb({ lemmaOneOf: ['言う', 'いう'] }, 'iu');
      const te = b.tok({ text: 'て', dep: 'mark' }, 'te');
      b.inOrder(de, iu, 1);
      b.inOrder(iu, te, 1);
      b.captureSpan('で言って', de, te);
    }
  );
});
