import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('うverb--ない', (r) => {
  // u-verb (godan verb) negative form
  // Matches: 歩かない, 歌わない, 泳がない, 話さない, etc.
  //
  // Structure:
  // - VERB with conjugationClass 五段-* (godan verb: 五段-カ行, 五段-ガ行, etc.)
  // - in inflectionForm 未然形-一般 (irrealis form)
  // - followed by AUX ない (lemma=ない, conjugationClass=助動詞-ナイ)
  //
  // Negative cases to exclude:
  // - ru-verb negatives: 食べない (下一段), 見ない (上一段)
  // - i-adjective negatives: 高くない (base is ADJ with 連用形)
  // - ある → ない (parsed as ADJ 形容詞, not AUX)

  const nai = r.aux({
    lemma: 'ない',
    conjugationClass: '助動詞-ナイ',
  }, 'nai');

  // Match all godan verb conjugation classes
  r.either(
    // 五段-カ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-カ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-ガ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-ガ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-サ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-サ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-タ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-タ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-ナ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-ナ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-バ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-バ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-マ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-マ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-ラ行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-ラ行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    },
    // 五段-ワア行
    (b) => {
      const verb = b.verb({
        conjugationClass: '五段-ワア行',
        inflectionForm: '未然形-一般',
      }, 'verb');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    }
  );
});
