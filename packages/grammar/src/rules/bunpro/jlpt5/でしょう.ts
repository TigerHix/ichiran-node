import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('でしょう', (r) => {
  r.either(
    // Pattern 1: Verb/Adj/Adv + でしょう (conjecture/probability)
    // e.g., 踊るでしょう, 寒いでしょう, いいでしょう, 無理でしょう, 初めてでしょう
    // GiNZA parses as: VERB/ADJ/ADV (root) <- でしょう (aux)
    (r1) => {
      const pred = r1.tok({ posOneOf: ['VERB', 'ADJ', 'ADV'] }, 'predicate');
      const deshou = r1.aux({
        textOneOf: ['でしょう', 'でしょ'],
        lemma: 'です',
        dep: 'aux',
        conjugationClass: '助動詞-デス',
      }, 'deshou');
      r1.auxOf(pred, deshou);
      r1.capture(deshou);
    },
    // Pattern 2: Noun/na-adj + でしょう (conjecture/probability)
    // e.g., 本でしょう, ハンバーガーでしょう, あなたはトムでしょう
    // GiNZA parses as: NOUN (root) <- でしょう (cop)
    // Note: Some proper nouns like トム are tagged ADJ, so we include ADJ here too
    (r2) => {
      const noun = r2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'noun');
      const deshou = r2.aux({
        textOneOf: ['でしょう', 'でしょ'],
        lemma: 'です',
        dep: 'cop',
        conjugationClass: '助動詞-デス',
      }, 'deshou');
      r2.copulaOf(noun, deshou);
      r2.capture(deshou);
    }
  );
});
