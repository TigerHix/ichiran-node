import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かと思ったら・かと思うと (ka to omottara - ka to omou to)
 * - Just when I thought.../No sooner than...
 *
 * A hypothetical structure showing that when an uncertain thought arose,
 * something conflicting or unexpected occurred immediately after.
 *
 * Structures:
 * - Verb［た］+ かと思ったら + (result)
 * - Verb［る］+ かと思うと + (result)
 * - いAdj + かと思ったら/かと思うと + (result)
 * - なAdj + (だ) + かと思ったら/かと思うと + (result)
 * - Noun + (だ) + かと思ったら/かと思うと + (result)
 *
 * The particle か may also be omitted in softer expressions:
 * - Verb［た/る］+ と思ったら/と思うと
 *
 * Examples:
 * - 赤ちゃんが泣き止んだかと思ったら、また大声で泣き始めた。
 *   (Just when I thought the baby had stopped crying, he started crying loudly again.)
 * - 何を言い出すかと思ったら、みんなの前で俺の悪口を言い出した。
 *   (Just when I thought of something to say, he started bad-mouthing me in front of everyone.)
 * - 子供が転んだかと思うと、立ち上がって走り出した。
 *   (No sooner than my kid fell down, she got up and started running.)
 *
 * Key discriminators:
 * - Pattern: [pred] + (か) + と + 思う + たら/と
 * - The particle か is optional (softer without it)
 * - 思う may be in various forms: 思う (dictionary), 思った (past), 思え (conditional stem)
 * - Endings: たら (tara conditional) or と (temporal/conditional)
 * - GiNZA parses 思う as a verb, と as particle, たら as SCONJ/AUX
 *
 * GiNZA parse structure:
 * - 言い出すかと思ったら:
 *   - 言い出す(VERB) + か(ADP) + と(ADP) + 思っ(VERB) + たら(SCONJ)
 * - 転んだかと思うと:
 *   - 転ん(VERB) + だ(AUX) + か(ADP) + と(ADP) + 思う(VERB) + と(ADP)
 */
export default bunproLinguisticRule('かと思ったら-かと思うと', (r) => {
  // The preceding element can be:
  // 1. Verb/Aux (in ta-form or dictionary form)
  // 2. Adjective (i-adjective or na-adjective + da)
  // 3. Noun (+ da)

  r.either(
    // Pattern 1: Verb/Aux + かと思ったら
    (b) => {
      const pred = b.tok({
        posOneOf: ['VERB', 'AUX']
      }, 'pred');

      // Optional particle か
      b.optional((ob) => {
        const ka = ob.particle('か', 'ka');
        ob.inOrder(pred, ka, 3);
      });

      // Particle と (quotation marker)
      const to1 = b.particle('と', 'to1');
      b.inOrder(pred, to1, 10);

      // Verb 思う (may be inflected: 思う, 思った, 思え, etc.)
      // Can appear as kanji (思う) or hiragana (おもう)
      // GiNZA may parse it as VERB or other POS in different contexts
      const omou = b.tok({
        lemmaOneOf: ['思う', 'おもう']
      }, 'omou');

      b.inOrder(to1, omou, 3);

      // Final particle: たら or と
      b.either(
        (eb) => {
          const tara = eb.tok({
            text: 'たら',
            posOneOf: ['SCONJ', 'AUX']
          }, 'tara');
          eb.inOrder(omou, tara, 2);
          eb.captureSpan('かと思ったら-かと思うと', pred, tara);
        },
        (eb) => {
          const to2 = eb.particle('と', 'to2');
          eb.inOrder(omou, to2, 2);
          eb.captureSpan('かと思ったら-かと思うと', pred, to2);
        }
      );
    },

    // Pattern 2: Adjective (い-adjective) + かと思ったら
    (b) => {
      const adj = b.adj({}, 'adj');

      // Optional particle か
      b.optional((ob) => {
        const ka = ob.particle('か', 'ka');
        ob.inOrder(adj, ka, 2);
      });

      // Particle と
      const to1 = b.particle('と', 'to1');
      b.inOrder(adj, to1, 3);

      // Verb 思う (can appear as kanji or hiragana)
      const omou = b.verb({
        lemmaOneOf: ['思う', 'おもう']
      }, 'omou');

      b.inOrder(to1, omou, 2);

      // Final particle: たら or と
      b.either(
        (eb) => {
          const tara = eb.tok({
            text: 'たら',
            posOneOf: ['SCONJ', 'AUX']
          }, 'tara');
          eb.inOrder(omou, tara, 2);
          eb.captureSpan('かと思ったら-かと思うと', adj, tara);
        },
        (eb) => {
          const to2 = eb.particle('と', 'to2');
          eb.inOrder(omou, to2, 2);
          eb.captureSpan('かと思ったら-かと思うと', adj, to2);
        }
      );
    },

    // Pattern 3: Noun (or な adjective stem) + だ + かと思ったら
    (b) => {
      // Match nouns, but exclude "おもう"/"思う" which should be matched by verb patterns
      // GiNZA may parse these as nouns in some contexts
      const noun = b.tok({
        pos: 'NOUN',
        textRe: /^(?!(おもう|思う)$).+$/,
      }, 'noun');

      // Copula だ (may be implicit in noun+no patterns)
      b.optional((ob) => {
        const da = ob.aux({ lemma: 'だ' }, 'da');
        ob.auxOf(noun, da);
      });

      // Optional particle か
      b.optional((ob) => {
        const ka = ob.particle('か', 'ka');
        ob.inOrder(noun, ka, 3);
      });

      // Particle と
      const to1 = b.particle('と', 'to1');
      b.inOrder(noun, to1, 5);

      // Verb 思う (can appear as kanji or hiragana)
      const omou = b.verb({
        lemmaOneOf: ['思う', 'おもう']
      }, 'omou');

      b.inOrder(to1, omou, 2);

      // Final particle: たら or と
      b.either(
        (eb) => {
          const tara = eb.tok({
            text: 'たら',
            posOneOf: ['SCONJ', 'AUX']
          }, 'tara');
          eb.inOrder(omou, tara, 2);
          eb.captureSpan('かと思ったら-かと思うと', noun, tara);
        },
        (eb) => {
          const to2 = eb.particle('と', 'to2');
          eb.inOrder(omou, to2, 2);
          eb.captureSpan('かと思ったら-かと思うと', noun, to2);
        }
      );
    }
  );
});
