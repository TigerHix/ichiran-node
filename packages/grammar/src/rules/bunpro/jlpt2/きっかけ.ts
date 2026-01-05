import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: きっかけ (kikkake) - opportunity, chance, trigger, turning point
 *
 * A noun meaning "opportunity, trigger, chance" that indicates (A) was the
 * trigger/cause that led to (B). Often describes an opportunity or process
 * that led to a result.
 *
 * Kanji: 切っ掛け
 *
 * Structures:
 * - Noun + をきっかけに (をきっかけにして) + (B)
 * - Verb［た］+ のをきっかけに (をきっかけにして) + (B)
 * - Noun + がきっかけで (がきっかけになって) + (B)
 * - Verb［た］+ のがきっかけで (がきっかけになって) + (B)
 *
 * Examples:
 * - 彼女とは、パーティーで会ったのをきっかけに付き合い始めた。
 *   (Meeting her at the party led us to start dating.)
 * - 私は入院をきっかけにタバコを止めることにしました。
 *   (Getting hospitalized led me to stop smoking.)
 * - この音楽は有名な歌手がカバーしたのがきっかけで再び有名になった。
 *   (This song became famous again because that famous singer made a cover of it.)
 * - このゲームがきっかけで、プログラマーになる事ができた。
 *   (This game led me to become a programmer.)
 *
 * Key discriminators:
 * - きっかけ is a noun (adverbial noun) meaning "opportunity, trigger"
 * - Usually preceded by noun+を/が or verb+のを/のが
 * - Followed by に/で (optionally with して/なって/として)
 * - GiNZA parses きっかけ as NOUN
 *
 * GiNZA parse structure:
 * - 会ったのをきっかけに: 会っ(VERB) + た(AUX) + の(NOUN) + を(ADP) + きっかけ(NOUN) + に(ADP)
 * - 入院をきっかけに: 入院(NOUN) + を(ADP) + きっかけ(NOUN) + に(ADP)
 * - ゲームがきっかけで: ゲーム(NOUN) + が(ADP) + きっかけ(NOUN) + で(ADP)
 */
export default linguisticRule('きっかけ', (r) => {
  // Match both hiragana and kanji forms
  const kikkake = r.tok({
    lemmaOneOf: ['きっかけ', '切っ掛け'],
  }, 'kikkake');

  r.either(
    // Pattern 1: Noun + をきっかけに (をきっかけにして, をきっかけとして)
    (b) => {
      const noun = b.noun({}, 'noun');
      const wo = b.particle('を', 'wo');
      b.caseMarker(noun, wo);

      b.inOrder(wo, kikkake, 3);

      // Followed by に (with optional して), として, or としてして
      b.either(
        // に alone
        (eb) => {
          const ni = eb.particle('に', 'ni');
          eb.inOrder(kikkake, ni, 2);
          eb.captureSpan('きっかけに', noun, ni);
        },
        // にして (expansion of "に")
        (eb) => {
          const ni = eb.particle('に', 'ni');
          const shite = eb.tok({ text: 'して', lemma: 'する' }, 'shite');
          eb.inOrder(kikkake, ni, 2);
          eb.inOrder(ni, shite, 3);
          eb.captureSpan('きっかけにして', noun, shite);
        },
        // として (alternative to に)
        // May be parsed as single token or split into として or と+し+て
        (eb) => {
          const to = eb.tok({ text: 'と', pos: 'ADP' }, 'to');
          eb.either(
            // して as single token (VERB or AUX)
            (sb) => {
              const shite = sb.tok({ text: 'して', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shite');
              sb.inOrder(to, shite, 1);
              sb.captureSpan('きっかけとして', noun, shite);
            },
            // して split into し + て (most common)
            (sb) => {
              const shi = sb.tok({ text: 'し', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shi');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(to, shi, 1);
              sb.inOrder(shi, te, 1);
              sb.captureSpan('きっかけとして', noun, te);
            }
          );
        }
      );
    },

    // Pattern 2: Verb［た］+ のをきっかけに (をきっかけにして)
    (b) => {
      const verb = b.verb({}, 'verb');

      // Match ta-form auxiliary (た)
      const ta = b.tok({
        lemma: 'た',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'ta');

      b.auxOf(verb, ta);

      // Nominalizer の
      const no = b.tok({ text: 'の', pos: 'NOUN' }, 'no');
      b.inOrder(ta, no, 3);

      // Object particle を
      const wo = b.particle('を', 'wo');
      b.inOrder(no, wo, 2);

      b.inOrder(wo, kikkake, 3);

      // Followed by に (with optional して) or として
      b.either(
        // に alone
        (eb) => {
          const ni = eb.particle('に', 'ni');
          eb.inOrder(kikkake, ni, 2);
          eb.captureSpan('きっかけに', verb, ni);
        },
        // にして (expansion of "に")
        (eb) => {
          const ni = eb.particle('に', 'ni');
          const shite = eb.tok({ text: 'して', lemma: 'する' }, 'shite');
          eb.inOrder(kikkake, ni, 2);
          eb.inOrder(ni, shite, 3);
          eb.captureSpan('きっかけにして', verb, shite);
        },
        // として (alternative to に)
        // May be parsed as single token or split into として or と+し+て
        (eb) => {
          const to = eb.tok({ text: 'と', pos: 'ADP' }, 'to');
          eb.either(
            // して as single token (VERB or AUX)
            (sb) => {
              const shite = sb.tok({ text: 'して', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shite');
              sb.inOrder(to, shite, 1);
              sb.captureSpan('きっかけとして', verb, shite);
            },
            // して split into し + て (most common)
            (sb) => {
              const shi = sb.tok({ text: 'し', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shi');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(to, shi, 1);
              sb.inOrder(shi, te, 1);
              sb.captureSpan('きっかけとして', verb, te);
            }
          );
        }
      );
    },

    // Pattern 3: Noun + がきっかけで (がきっかけになって, がきっかけとなって)
    (b) => {
      const noun = b.noun({}, 'noun');
      const ga = b.particle('が', 'ga');

      // が marks the noun (subject or topic)
      b.headChild(noun, ga, 'case');
      b.inOrder(noun, ga, 3);

      b.inOrder(ga, kikkake, 3);

      // Followed by で (with optional なって), となって, or として
      b.either(
        // で alone
        (eb) => {
          const de = eb.particle('で', 'de');
          eb.inOrder(kikkake, de, 2);
          eb.captureSpan('きっかけで', noun, de);
        },
        // になって (becoming - "で" + "なって")
        (eb) => {
          const de = eb.particle('で', 'de');
          const natte = eb.tok({ text: 'になって', lemma: 'なる' }, 'natte');
          eb.inOrder(kikkake, de, 2);
          eb.inOrder(de, natte, 3);
          eb.captureSpan('きっかけになって', noun, natte);
        },
        // となって (becoming - single token or split)
        (eb) => {
          eb.either(
            // Single token "となって" (without lemma constraint)
            (sb) => {
              const tonatte = sb.tok({ textOneOf: ['となって', 'と成って'] }, 'tonatte');
              sb.inOrder(kikkake, tonatte, 2);
              sb.captureSpan('きっかけとなって', noun, tonatte);
            },
            // Split as "と" + "なっ" + "て"
            (sb) => {
              const to = sb.tok({ text: 'と' }, 'to');
              const nat = sb.tok({ text: 'なっ', lemma: 'なる' }, 'nat');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(kikkake, to, 3);
              sb.inOrder(to, nat, 1);
              sb.inOrder(nat, te, 1);
              sb.captureSpan('きっかけとなって', noun, te);
            }
          );
        },
        // として (alternative to で)
        // May be parsed as single token or split into として or と+し+て
        (eb) => {
          const to = eb.tok({ text: 'と', pos: 'ADP' }, 'to');
          eb.either(
            // して as single token (VERB or AUX)
            (sb) => {
              const shite = sb.tok({ text: 'して', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shite');
              sb.inOrder(to, shite, 1);
              sb.captureSpan('きっかけとして', noun, shite);
            },
            // して split into し + て (most common)
            (sb) => {
              const shi = sb.tok({ text: 'し', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shi');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(to, shi, 1);
              sb.inOrder(shi, te, 1);
              sb.captureSpan('きっかけとして', noun, te);
            }
          );
        }
      );
    },

    // Pattern 4: Verb［た］+ のがきっかけで (がきっかけになって)
    (b) => {
      const verb = b.verb({}, 'verb');

      // Match ta-form auxiliary (た)
      const ta = b.tok({
        lemma: 'た',
        posOneOf: ['AUX', 'SCONJ'],
      }, 'ta');

      b.auxOf(verb, ta);

      // Nominalizer の
      const no = b.tok({ text: 'の', pos: 'NOUN' }, 'no');
      b.inOrder(ta, no, 3);

      // Subject particle が
      const ga = b.particle('が', 'ga');
      b.inOrder(no, ga, 2);

      b.inOrder(ga, kikkake, 3);

      // Followed by で (with optional なって), となって, or として
      b.either(
        // で alone
        (eb) => {
          const de = eb.particle('で', 'de');
          eb.inOrder(kikkake, de, 2);
          eb.captureSpan('きっかけで', verb, de);
        },
        // になって (becoming - "で" + "なって")
        (eb) => {
          const de = eb.particle('で', 'de');
          const natte = eb.tok({ text: 'になって', lemma: 'なる' }, 'natte');
          eb.inOrder(kikkake, de, 2);
          eb.inOrder(de, natte, 3);
          eb.captureSpan('きっかけになって', verb, natte);
        },
        // となって (becoming - single token or split)
        (eb) => {
          eb.either(
            // Single token "となって" (without lemma constraint)
            (sb) => {
              const tonatte = sb.tok({ textOneOf: ['となって', 'と成って'] }, 'tonatte');
              sb.inOrder(kikkake, tonatte, 2);
              sb.captureSpan('きっかけとなって', verb, tonatte);
            },
            // Split as "と" + "なっ" + "て"
            (sb) => {
              const to = sb.tok({ text: 'と' }, 'to');
              const nat = sb.tok({ text: 'なっ', lemma: 'なる' }, 'nat');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(kikkake, to, 3);
              sb.inOrder(to, nat, 1);
              sb.inOrder(nat, te, 1);
              sb.captureSpan('きっかけとなって', verb, te);
            }
          );
        },
        // として (alternative to で)
        // May be parsed as single token or split into として or と+し+て
        (eb) => {
          const to = eb.tok({ text: 'と', pos: 'ADP' }, 'to');
          eb.either(
            // して as single token (VERB or AUX)
            (sb) => {
              const shite = sb.tok({ text: 'して', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shite');
              sb.inOrder(to, shite, 1);
              sb.captureSpan('きっかけとして', verb, shite);
            },
            // して split into し + て (most common)
            (sb) => {
              const shi = sb.tok({ text: 'し', lemma: 'する', posOneOf: ['VERB', 'AUX'] }, 'shi');
              const te = sb.tok({ text: 'て', lemma: 'て', posOneOf: ['SCONJ', 'AUX'] }, 'te');
              sb.inOrder(to, shi, 1);
              sb.inOrder(shi, te, 1);
              sb.captureSpan('きっかけとして', verb, te);
            }
          );
        }
      );
    }
  );
});
