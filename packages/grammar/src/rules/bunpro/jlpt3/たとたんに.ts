import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: たとたん(に) - "the moment that", "just as", "as soon as"
 *
 * Matches verb ta-form + とたん(に) to indicate an immediate consequence.
 *
 * Structure:
 * - Verb［た］+ とたん(に)
 *
 * This grammar point expresses that something happens the instant or moment
 * that something else occurs. It emphasizes suddenness or unexpectedness,
 * and is typically used for events beyond the speaker's control.
 *
 * Examples:
 * - 布団に入ったとたんに先輩から電話がかかってきた (The moment I got into bed, I got a call)
 * - 窓を開けたとたん、鳥が部屋に入ってきた (As soon as I opened the window, a bird flew in)
 * - 彼女を見たとたんに恋をしてしまった (I fell in love the moment I saw her)
 *
 * GiNZA parse structure:
 * - Verb (VERB) - the main verb
 * - た (AUX) - past tense auxiliary attached to verb
 * - とたん (NOUN) - parsed as a SINGLE noun token (key discovery!)
 * - に (ADP) - optional case particle attached to とたん
 *
 * The pattern is: verb + た + とたん + (に?)
 *
 * Key discriminators:
 * - Must follow verb ta-form (cannot follow noun or adjective)
 * - "とたん" is parsed as a single NOUN token with lemma=とたん
 * - "とたん" has dep=obl (oblique nominal) indicating temporal function
 */
export default linguisticRule('たとたんに', (r) => {
  // Any verb in any form
  const verb = r.verb({}, 'verb');

  // The た auxiliary (past tense) - can have various dep labels
  // GiNZA parses it inconsistently: sometimes dep=aux, sometimes dep=fixed
  const ta = r.tok({ lemma: 'た', posOneOf: ['AUX', 'SCONJ'] }, 'ta');

  // ta must be attached to verb (either as aux or fixed relation)
  r.either(
    // Standard: ta as aux of verb
    (b) => { b.auxOf(verb, ta); },
    // GiNZA quirk: ta with dep=fixed pointing to verb
    (b) => { b.headChild(verb, ta, 'fixed'); }
  );

  // The とたん noun - GiNZA parses this as a SINGLE NOUN token!
  // Can have various dep labels: obl (standard), root (sentence-final), etc.
  const totan = r.noun({ lemma: 'とたん' }, 'totan');

  // とたん must immediately follow た
  r.inOrder(ta, totan, 1);

  // Optional に particle (can be omitted: "とたん" vs "とたんに")
  r.either(
    // With に particle
    (b) => {
      const ni = b.particle('に', 'ni');
      // に is a case marker attached to とたん
      b.caseMarker(totan, ni);
      // Capture from verb to に
      b.captureSpan('たとたんに', verb, ni);
    },
    // Without に particle
    (b) => {
      // Capture from verb to とたん
      b.captureSpan('たとたんに', verb, totan);
    }
  );
});
