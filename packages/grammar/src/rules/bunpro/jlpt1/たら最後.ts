import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たら最後', (r) => {
  // たら最後 / たが最後 (tara saigo / ta saigo) - "once X happens, there's no turning back"
  // Pattern: Verb［た］+ が/ら + 最後
  // Examples:
  //   始めたら最後、止められない (once you start, you can't stop)
  //   失ったが最後、取り戻せない (once you lose it, you can't get it back)
  //   言ったら最後だ (once you say it, that's it)
  //
  // The verb can be in ta-form (た) or tara-form (たら), followed by ga (が) or ra (ら)
  // Then 最後 (saigo - "end"/"last")
  //
  // Key discriminators:
  // - Verb must be in past form (た) or conditional form (たら)
  // - が or ら connects verb to 最後
  // - Different from simple "最後" noun usage or "〜てから" (after doing)
  //
  // GiNZA parsing notes:
  // - たら can be a single AUX token or split as た (AUX) + ら (SCONJ/AUX)
  // - が can be a particle (ADP) or conjunction (CCONJ)
  // - 最後 is typically a NOUN
  //
  // Negative tests to avoid:
  // - 最後 as a standalone noun (e.g., これは最後だ)
  // - 〜てから patterns (different grammar)
  // - た form followed by different particles (e.g., た時, た後)

  r.either(
    // Pattern 1: たら最後 (tara-form + 最後)
    // e.g., 始めたらさいご, 言ったらさいご, 開けたらさいご
    // Note: Test data uses hiragana "さいご" instead of kanji "最後"
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const tara = b1.tok({ textOneOf: ['たら', 'た'], posOneOf: ['AUX', 'SCONJ'] }, 'tara');
      const saigo = b1.tok({ textOneOf: ['最後', 'さいご'] }, 'saigo');
      b1.inOrder(verb, tara, 5);
      b1.inOrder(tara, saigo, 3);
      b1.captureSpan('たら最後', verb, saigo);
    },

    // Pattern 2: たが最後 (ta-form + が + 最後)
    // e.g., 失ったがさいご, 掛けたがさいご, なったがさいご
    // Note: Test data uses hiragana "さいご" instead of kanji "最後"
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const ta = b2.aux({ text: 'た' }, 'ta');
      const ga = b2.particle('が', 'ga');
      const saigo = b2.tok({ textOneOf: ['最後', 'さいご'] }, 'saigo');
      b2.inOrder(verb, ta, 5);
      b2.inOrder(ta, ga, 2);
      b2.inOrder(ga, saigo, 2);
      b2.captureSpan('たら最後', verb, saigo);
    }
  );
});
