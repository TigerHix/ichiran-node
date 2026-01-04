import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おかげで', (r) => {
  // Pattern: noun/verb/adj + おかげで (thanks to / because of)
  // おかげで expresses gratitude for a cause that led to a positive result
  // (can also be used sarcastically for negative results)
  //
  // GiNZA parses おかげで as:
  // - おかげ (NOUN) with various dependencies
  // - で as:
  //   - AUX with lemma=だ, dep=cop (after adj)
  //   - AUX with lemma=だ, dep=case (after verb/noun+の - GiNZA inconsistency!)
  //   - ADP with lemma=だ, dep=case (after verb - GiNZA inconsistency!)
  //   - ADP with lemma=で, dep=case (after noun+の)

  const okage = r.noun({ lemma: 'おかげ' }, 'okage');

  r.either(
    // Pattern 1: おかげで after i-adjective (連体形)
    // 風が強いおかげで、ヨットのスピードが上がった。
    (b) => {
      const de = b.aux({ lemma: 'だ', dep: 'cop' }, 'de');
      b.headChild(okage, de, 'cop');
      b.captureSpan('おかげで', okage, de);
    },
    // Pattern 2: おかげで after na-adjective + な
    // 友達が有名なおかげで、どんな高級レストランでも予約なしで入れる。
    (b) => {
      const na = b.aux({ lemma: 'だ', inflectionForm: '連体形-一般' }, 'na');
      b.headChild(okage, na, 'aux');
      const de = b.aux({ lemma: 'だ', dep: 'cop' }, 'de');
      b.headChild(okage, de, 'cop');
      b.captureSpan('おかげで', okage, de);
    },
    // Pattern 3: おかげで after verb + た (GiNZA: ADP + lemma=だ + dep=case)
    // 勉強をしたおかげで、試験に合格した。
    (b) => {
      const de = b.tok({ pos: 'ADP', lemma: 'だ', dep: 'case' }, 'de');
      b.headChild(okage, de, 'case');
      b.captureSpan('おかげで', okage, de);
    },
    // Pattern 4: おかげで after verb/noun (GiNZA: AUX + lemma=だ + dep=case)
    // スマホのおかげで、目的地にスムーズに着きました。
    // オニヅカ先生に日本語を教えてもらったおかげで、ぺらぺらと話せるようになった。
    (b) => {
      const de = b.aux({ lemma: 'だ', dep: 'case' }, 'de');
      b.headChild(okage, de, 'case');
      b.captureSpan('おかげで', okage, de);
    },
    // Pattern 5: おかげで after noun + の (GiNZA: ADP + text=で + dep=case)
    // 薬のおかげでだいぶよくなりました。
    // あなたのおかげで不自由のない生活ができている。
    (b) => {
      const de = b.tok({ pos: 'ADP', text: 'で', dep: 'case' }, 'de');
      b.headChild(okage, de, 'case');
      b.captureSpan('おかげで', okage, de);
    }
  );
});
