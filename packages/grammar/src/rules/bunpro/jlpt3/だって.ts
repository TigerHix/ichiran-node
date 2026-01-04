import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だって', (r) => {
  // だって (datte) - casual particle with multiple meanings:
  // 1. Sentence-final: Noun+だって/なんだって = "I heard/they said" (hearsay)
  // 2. Sentence-initial: だって = "because" (casual explanation)
  // 3. After noun/pronoun: Noun+だって = "even Noun" (emphatic inclusive)
  //
  // Different from sentence-final んだって (different grammar point)
  // Different from たって (even though - JLPT2)
  //
  // GiNZA parsing notes:
  // - だって is ALWAYS parsed as TWO tokens: だ (AUX/CCONJ) + って (ADP)
  // - For hearsay: だ (AUX, cop/fixed) + って (ADP, case)
  // - For sentence-initial "because": だ (CCONJ, cc) + って (ADP, fixed)
  // - For "even": だ (AUX, cop) + って (ADP, case)
  // - The lemma for だ is always "だ"
  // - The lemma for って is always "って"

  r.either(
    // Pattern 1: Sentence-initial だって = "because" (casual explanation)
    // だって、全然知らないんだもん。
    // だって、サメとか怖いもん。
    // だって、家の手伝いをしなきゃいけなかったんだ。
    // だって、俺の元カノも誘ったんでしょう？
    // え〜 行きたくないよ。だって、俺の元カノも誘ったんでしょう？
    // めがねちゃん：... ヤンキーくん：「だって、 家の手伝いをしなきゃいけなかったんだ。」
    // 私達は出かけなかった。だって悪い天気だったから。
    // GiNZA: だ (CCONJ/AUX, dep=cc/dep) + って (ADP, dep=fixed)
    (b) => {
      const da = b.tok({
        text: 'だ',
        lemma: 'だ',
        posOneOf: ['CCONJ', 'AUX'],
        depOneOf: ['cc', 'dep'],
      }, 'da');
      const tte = b.tok({
        text: 'って',
        lemma: 'って',
        pos: 'ADP',
        dep: 'fixed',
      }, 'tte');
      b.inOrder(da, tte, 1);
      b.captureSpan('だって', da, tte);
    },

    // Pattern 2: Noun (+ particle) + だ (copula) + って = "even (noun)" or hearsay question
    // 俺だって行きたくないよ (Even I don't want to go)
    // 誰だって傷つくよ (Anyone would get hurt)
    // 父親だって (Even my father)
    // 彼だって落ち込むこともあるよ (Even he gets depressed)
    // 月にだって行けるかもしれない (You can even go to the moon)
    // アイスクリームだって薬になる (Even ice cream can be medicine)
    // 「競馬だって？またお金を全部失うよ。」 (Horse racing, you say?)
    // 「たけしさん、海外旅行だって？」 (Is it true Takeshi is on vacation abroad?)
    // それは恋だって。表情を見れば分かります。 (They say it's love.)
    // 全員合格だってよ。 (I heard everyone passed.)
    // GiNZA: noun (+ particle) + だ (AUX, dep=cop) + って (ADP, dep=case)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const da = b.aux({
        text: 'だ',
        lemma: 'だ',
        dep: 'cop',
      }, 'da');
      const tte = b.tok({
        text: 'って',
        lemma: 'って',
        pos: 'ADP',
        dep: 'case',
      }, 'tte');
      b.inOrder(noun, da, 2);  // Allow optional particle between noun and だ
      b.inOrder(da, tte, 1);
      b.captureSpan('だって', noun, tte);
    },

    // Pattern 3: Noun + な + ん + だ (fixed) + って = "I heard (Noun)..."
    // レムの実家は農家なんだって。 (I heard Rem's family owns a farm.)
    // キリコは明日から一週間休みなんだって。 (I hear Kiriko will be on break.)
    // 今から体育なんだって。 (I heard we have P.E. from now.)
    // GiNZA: noun + な (AUX, cop) + ん (SCONJ, mark) + だ (AUX, fixed) + って (ADP, case)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      const na = b.aux({
        text: 'な',
        lemma: 'だ',
        dep: 'cop',
      }, 'na');
      const nn = b.tok({
        text: 'ん',
        lemma: 'ん',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'nn');
      const da = b.aux({
        text: 'だ',
        lemma: 'だ',
        dep: 'fixed',
      }, 'da');
      const tte = b.tok({
        text: 'って',
        lemma: 'って',
        pos: 'ADP',
        dep: 'case',
      }, 'tte');
      b.inOrder(noun, na, 2);
      b.inOrder(na, nn, 1);
      b.inOrder(nn, da, 1);
      b.inOrder(da, tte, 1);
      b.captureSpan('だって', noun, tte);
    }
  );
});
