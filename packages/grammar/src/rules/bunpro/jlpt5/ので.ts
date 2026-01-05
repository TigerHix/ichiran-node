import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ので', (r) => {
  // ので (node): conjunction particle meaning "because, so, since"
  // Semi-formal expression indicating A caused or instigated B
  // Similar to から but more polite/formal
  //
  // GiNZA parsing:
  // - ので is parsed as TWO tokens: の (SCONJ) + で (AUX)
  // - の: pos=SCONJ, dep=mark, lemma=の
  // - で: pos=AUX, dep=fixed, lemma=だ
  //
  // Pattern 1: Noun/Na-adj + な + ので (e.g., 先生なので, 綺麗なので)
  //   - な: pos=AUX, dep=cop, lemma=だ
  // Pattern 2: い-Adjective + ので (e.g., 寒いので, 弱いので) - NO な
  // Pattern 3: Verb + ので (e.g., 行くので, 来るので) - NO な

  r.either(
    // Pattern 1: Noun/Na-adj + な + ので
    // This handles both nouns and na-adjectives
    // Note: GiNZA assigns dep=aux for な with adjectives, dep=cop for nouns
    (r1) => {
      const nounOrNaAdj = r1.tok({
        posOneOf: ['NOUN', 'PROPN', 'ADJ'],
      }, 'nounOrNaAdj');
      const na = r1.aux({ lemma: 'だ', text: 'な', depOneOf: ['cop', 'aux'] }, 'na');
      const no = r1.tok({ text: 'の', pos: 'SCONJ', dep: 'mark', lemma: 'の' }, 'no');
      const de = r1.tok({ text: 'で', pos: 'AUX', dep: 'fixed', lemma: 'だ' }, 'de');

      r1.inOrder(nounOrNaAdj, na, 1);
      r1.inOrder(na, no, 1);
      r1.inOrder(no, de, 1);
      r1.headChild(nounOrNaAdj, no);

      r1.captureSpan('なので', nounOrNaAdj, de);
    },
    // Pattern 2: い-Adjective + ので (no な between)
    (r2) => {
      const iAdj = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdj');
      const no = r2.tok({ text: 'の', pos: 'SCONJ', dep: 'mark', lemma: 'の' }, 'no');
      const de = r2.tok({ text: 'で', pos: 'AUX', dep: 'fixed', lemma: 'だ' }, 'de');

      r2.inOrder(iAdj, no, 5);  // Allow for auxiliary chains (e.g., 好きではないので)
      r2.inOrder(no, de, 1);
      r2.headChild(iAdj, no);

      r2.captureSpan('なので', iAdj, de);
    },
    // Pattern 3: Verb + (た)? + ので (no な between)
    // Optional た for past tense verbs
    (r3) => {
      const verb = r3.verb({}, 'verb');
      const no = r3.tok({ text: 'の', pos: 'SCONJ', dep: 'mark', lemma: 'の' }, 'no');
      const de = r3.tok({ text: 'で', pos: 'AUX', dep: 'fixed', lemma: 'だ' }, 'de');

      r3.inOrder(verb, no, 5);  // Allow for verb chains (e.g., 早く起きるので)
      r3.inOrder(no, de, 1);
      r3.headChild(verb, no);

      r3.captureSpan('なので', verb, de);
    },
    // Pattern 4: Na-adj + ではない/じゃない + ので (negative form)
    // e.g., 好きではないので, 綺麗じゃないので
    // GiNZA parses では as: で (AUX/aux) + は (ADP/fixed)
    // GiNZA parses じゃ as single token (ADP/fixed)
    (r4) => {
      const naAdj = r4.adj({}, 'naAdj');
      const de = r4.aux({ lemma: 'だ', text: 'で', dep: 'aux' }, 'de');
      const wa = r4.particle('は', 'wa');
      const nai = r4.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');
      const no = r4.tok({ text: 'の', pos: 'SCONJ', dep: 'mark', lemma: 'の' }, 'no');
      const nodeDe = r4.tok({ text: 'で', pos: 'AUX', dep: 'fixed', lemma: 'だ' }, 'nodeDe');

      r4.inOrder(naAdj, de, 1);
      r4.inOrder(de, wa, 1);
      r4.inOrder(wa, nai, 1);
      r4.inOrder(nai, no, 1);
      r4.inOrder(no, nodeDe, 1);
      r4.headChild(naAdj, no);

      r4.captureSpan('なので', naAdj, nodeDe);
    }
  );
});
