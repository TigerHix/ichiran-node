import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てごらん', (r) => {
  // てごらん (te-form verb + ごらん): "try doing X" / "please try X"
  // An honorific expression used to ask someone to attempt to do something,
  // or to look at something at their leisure.
  //
  // Structure: Verb[て-form] + ごらん
  // Variants: Verb[て-form] + ごらんなさい (slightly stronger, more like a command)
  //
  // GiNZA parsing:
  //   - Verb in て-form: VERB with various inflection forms + SCONJ (て/で) with dep=mark
  //   - For suru-verbs (勉強して): VERB(勉強) + AUX(し) + SCONJ(て), where te.head points to VERB
  //   - For regular verbs (食べて): VERB(食べ) + SCONJ(て), where te.head points to VERB
  //   - ごらん alone: NOUN with lemma=ごらん, dep=ROOT
  //   - ごらんなさい: VERB (ごらん) + AUX (なさい with lemma=なさる)
  //
  // Examples:
  //   読んでごらん → 読ん[VERB,acl] + で[SCONJ,mark] + ごらん[NOUN,ROOT]
  //   勉強してごらん → 勉強[VERB,acl] + し[AUX,aux] + て[SCONJ,mark] + ごらん[NOUN,ROOT]
  //   捕まえてごらんなさい → 捕まえ[VERB,advcl] + て[SCONJ,mark] + ごらん[VERB,ROOT] + なさい[AUX,aux]
  //
  // We use r.either() to handle the two variants:
  //   1. Verb-て + ごらん (NOUN)
  //   2. Verb-て + ごらん (VERB) + なさい (AUX)
  //
  // Key insight: In all cases, te.head points to the VERB (not the AUX), so we
  // match VERB specifically, not VERB|AUX.

  r.either(
    // Variant 1: Verb-て + ごらん (basic form)
    (b1) => {
      const verbTe = b1.verb({}, 'verbTe');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const goran = b1.tok({ lemma: 'ごらん', posOneOf: ['NOUN', 'VERB'] }, 'goran');

      // te must point to verbTe
      b1.headChild(verbTe, te, 'mark');

      // Order constraints
      // Allow maxDistance=2 to handle suru-verbs: VERB + AUX(し) + SCONJ(て)
      // For regular verbs: VERB + SCONJ(て), distance=1
      b1.inOrder(verbTe, te, 2);
      b1.inOrder(te, goran, 1);

      // Capture the full pattern
      b1.captureSpan('てごらん', verbTe, goran);
    },

    // Variant 2: Verb-て + ごらんなさい (stronger form)
    (b2) => {
      const verbTe = b2.verb({}, 'verbTe');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const goran = b2.tok({ lemma: 'ごらん', pos: 'VERB' }, 'goran');
      const nasai = b2.aux({ lemma: 'なさる' }, 'nasai');

      // te must point to verbTe
      b2.headChild(verbTe, te, 'mark');

      // Order constraints
      // Allow maxDistance=2 to handle suru-verbs: VERB + AUX(し) + SCONJ(て)
      b2.inOrder(verbTe, te, 2);
      b2.inOrder(te, goran, 1);
      b2.auxOf(goran, nasai);

      // Capture the full pattern
      b2.captureSpan('てごらんなさい', verbTe, nasai);
    }
  );
});
