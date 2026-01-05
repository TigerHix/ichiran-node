import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('あり', (r) => {
  // Linguistic view:
  // あり is the literary form (文語形) of ある used to indicate:
  // 1. "one possibility among many" (～というあり)
  // 2. "with X" (inclusion: X + あり + の/で + Y)
  //
  // Key discriminators from regular ある:
  // - inflectionForm=連用形-一般 (nominal form, not complete predicate) when lemma=ある
  // - lemma=あり (when GiNZA parses it as NOUN)
  // - Followed by copula だ/じゃ or particle の/で
  //
  // Structural patterns:
  // 1. Noun modifier: [noun] あり の [noun] → "noun WITH noun"
  //    駐車場ありのホテル (hotel with parking lot)
  // 2. Instrumental: [noun] あり で [verb] → "do [verb] WITH [noun]"
  //    字幕ありで見たい (want to watch WITH subtitles)
  // 3. Predicative: [noun] も あり だ → "noun is a possibility"
  //    遊園地もありだ (theme park is also a possibility)
  // 4. Predicative + conjunction: [noun] も あり だけど/じゃない
  //    ラーメンもありじゃない (isn't ramen also a possibility?)

  r.either(
    // Pattern 1: あり + の (noun modifier)
    // [noun] あり の [noun] = "noun WITH noun"
    // GiNZA parses as: あり with lemma=あり, POS=NOUN, dep=nmod
    (b) => {
      const ari = b.tok({
        lemma: 'あり',
        posOneOf: ['NOUN', 'VERB'],
      }, 'ari');
      const no = b.particle('の', 'no');
      b.inOrder(ari, no, 1);
      b.captureSpan('あり', ari, ari);
    },
    // Pattern 2: あり + で (instrumental case)
    // [noun] あり で [verb] = "do [verb] WITH [noun]"
    // GiNZA parses as: あり with lemma=あり, POS=VERB, dep=advcl
    // 字幕ありで見たい (want to watch WITH subtitles)
    (b) => {
      const ari = b.tok({
        lemma: 'あり',
        posOneOf: ['NOUN', 'VERB'],
      }, 'ari');
      const de = b.particle('で', 'de');
      b.inOrder(ari, de, 1);
      b.captureSpan('あり', ari, ari);
    },
    // Pattern 3: ある + 連用形-一般 + だ (copula)
    // [noun] も あり だ = "is a possibility"
    (b) => {
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(ari, da);
      b.captureSpan('あり', ari, da);
    },
    // Pattern 4: ある + 連用形-一般 + じゃ + ない (copula negation)
    // [noun] も あり じゃ ない = "isn't... a possibility?"
    (b) => {
      const ari = b.tok({
        lemmaOneOf: ['ある', 'あり'],
        inflectionForm: '連用形-一般',
      }, 'ari');
      const ja = b.aux({ lemma: 'だ' }, 'ja');
      b.headChild(ari, ja, 'cop');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.headChild(ja, nai, 'fixed');
      b.captureSpan('あり', ari, nai);
    },
    // Pattern 5: ある + 連用形-一般 + だ + けど/けど + embedded clause
    // お酒もありだけど (alcohol is also a possibility, but...)
    (b) => {
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(ari, da);
      const kedo = b.tok({ lemma: 'けど' }, 'kedo');
      b.headChild(da, kedo, 'mark');
      b.captureSpan('あり', ari, da);
    },
    // Pattern 6: ある + 連用形-一般 + だし (conjunction)
    // あの白いドレスもありだし (that dress is a possibility, and...)
    // GiNZA parses だし inconsistently: sometimes as lemma=だす, sometimes as lemma=だし
    // When parsed as lemma=だす, it's a mistake - we need to match text=だし instead
    (b) => {
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      // だし can be parsed as lemma=だし or lemma=だす, so we match text instead
      const dashi = b.tok({ text: 'だし' }, 'dashi');
      b.headChild(ari, dashi, 'advcl');
      b.captureSpan('あり', ari, ari);
    },
    // Pattern 7: あり + だ + だって (quotation/reporting)
    // GiNZA parses as: あり with lemma=あり, POS=NOUN, dep=root
    // キッズスペースありだって (it has a kids space, they say!)
    (b) => {
      const ari = b.tok({
        lemma: 'あり',
        posOneOf: ['NOUN', 'VERB'],
      }, 'ari');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(ari, da);
      const datte = b.particle('って', 'datte');
      b.headChild(da, datte, 'mark');
      b.captureSpan('あり', ari, da);
    },
    // Pattern 8: ある + 連用形-一般 + だ + だが (conjunction)
    // あの職場は近いからありだが (that workplace is a possibility because it's close, but...)
    (b) => {
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(ari, da);
      const daga = b.tok({ lemma: 'だが' }, 'daga');
      b.headChild(da, daga, 'mark');
      b.captureSpan('あり', ari, da);
    },
    // Pattern 9: ある + 連用形-一般 + だ + な (sentence-final particle)
    // カツ丼や牛丼もありだな… (katsu-don or gyu-don would be a possibility...)
    (b) => {
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(ari, da);
      const na = b.particle('な', 'na');
      b.headChild(da, na, 'mark');
      b.captureSpan('あり', ari, da);
    },
    // Pattern 10: あり (standalone exclamation at end of clause)
    // Must be followed by punctuation or end of sentence
    // ベジタリアン用のメニューあり！ (we have vegetarian menu items!)
    (b) => {
      const ari = b.tok({
        lemma: 'あり',
        dep: 'root',
      }, 'ari');
      b.captureSpan('あり', ari, ari);
    }
  );
});
