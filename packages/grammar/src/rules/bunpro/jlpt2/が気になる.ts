import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: が気になる (ga ki ni naru) - "to be concerned about, to be interested in"
 *
 * An expression meaning "(something) is on my mind" or "(something) concerns me".
 * It indicates that something has naturally drawn the speaker's attention or interest,
 * often without conscious effort.
 *
 * Structures:
 * - Noun + が + 気に + なる (casual)
 * - Noun + が + 気に + なります (polite)
 * - Noun + が + 気に + なっている (progressive/state)
 * - Noun + が + 気に + なっています (polite progressive/state)
 *
 * Examples:
 * - この足跡が気になる。
 *   (I am concerned about these tracks.)
 * - 友達の日本語のアクセントが気になる。
 *   (I am interested in my friend's Japanese accent.)
 * - 値段が気になります。
 *   (I am concerned about the price.)
 * - あの映画が気になっている。
 *   (I am interested in that movie.)
 * - 最近、芸能界が気になる。
 *   (Recently, I've become interested in the entertainment world.)
 *
 * Key discriminators:
 * - Subject marker が marks the thing that draws concern/interest
 * - 気 (ki) is a NOUN meaning "mind/spirit/attention"
 * - に (ni) is a case particle marking the target/state
 * - なる (naru) is an intransitive verb meaning "to become"
 * - Different from 気にする (ki ni suru) - active worrying (volitional)
 * - 気になる (ki ni naru) is involuntary concern/interest
 *
 * GiNZA parse structure:
 * - Noun phrase (nsubj) --nsubj--> naru
 * - が (ADP) --case--> noun
 * - 気 (NOUN) --obl--> naru
 * - に (ADP) --case--> ki
 * - なる (VERB) - root verb
 *
 * The polite form (なります) and progressive forms (なっている/なっています)
 * use auxiliaries attached to なる.
 */
export default linguisticRule('が気になる', (r) => {
  r.either(
    // Pattern 1: Noun + が + 気 + に + なる (casual form)
    (b1) => {
      const noun = b1.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const ga = b1.particle('が', 'ga');
      const ki = b1.noun({ lemma: '気' }, 'ki');
      const ni = b1.particle('に', 'ni');
      const naru = b1.verb({ lemma: 'なる' }, 'naru');

      // Structural constraints
      b1.inOrder(noun, ga, 1);
      b1.inOrder(ga, ki);
      b1.inOrder(ki, ni, 1);
      b1.inOrder(ni, naru, 1);

      // Dependency constraints
      b1.caseMarker(noun, ga);  // noun --case--> ga
      b1.caseMarker(ki, ni);    // ki --case--> ni
      b1.auxOf(naru, ni);       // ni --obl/aux--> naru

      b1.captureSpan('が気になる', noun, naru);
    },

    // Pattern 2: Noun + が + 気 + に + なります (polite form)
    (b2) => {
      const noun = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const ga = b2.particle('が', 'ga');
      const ki = b2.noun({ lemma: '気' }, 'ki');
      const ni = b2.particle('に', 'ni');
      const naru = b2.verb({ lemma: 'なる' }, 'naru');
      const masu = b2.aux({ lemma: 'ます' }, 'masu');

      // Structural constraints
      b2.inOrder(noun, ga, 1);
      b2.inOrder(ga, ki);
      b2.inOrder(ki, ni, 1);
      b2.inOrder(ni, naru, 1);
      b2.inOrder(naru, masu, 1);

      // Dependency constraints
      b2.caseMarker(noun, ga);  // noun --case--> ga
      b2.caseMarker(ki, ni);    // ki --case--> ni
      b2.auxOf(naru, ni);       // ni --obl/aux--> naru
      b2.auxOf(masu, naru);     // masu --aux--> naru

      b2.captureSpan('が気になります', noun, masu);
    },

    // Pattern 3: Noun + が + 気 + に + なっている (progressive/state, casual)
    (b3) => {
      const noun = b3.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const ga = b3.particle('が', 'ga');
      const ki = b3.noun({ lemma: '気' }, 'ki');
      const ni = b3.particle('に', 'ni');
      const natte = b3.aux({ lemma: 'なる' }, 'natte');
      const iru = b3.aux({ lemma: 'いる' }, 'iru');

      // Structural constraints
      b3.inOrder(noun, ga, 1);
      b3.inOrder(ga, ki);
      b3.inOrder(ki, ni, 1);
      b3.inOrder(ni, natte, 1);
      b3.inOrder(natte, iru, 1);

      // Dependency constraints
      b3.caseMarker(noun, ga);  // noun --case--> ga
      b3.caseMarker(ki, ni);    // ki --case--> ni
      b3.auxOf(natte, ni);      // ni --obl/aux--> natte
      b3.auxOf(iru, natte);     // iru --aux--> natte

      b3.captureSpan('が気になっている', noun, iru);
    },

    // Pattern 4: Noun + が + 気 + に + なっています (progressive/state, polite)
    (b4) => {
      const noun = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun');
      const ga = b4.particle('が', 'ga');
      const ki = b4.noun({ lemma: '気' }, 'ki');
      const ni = b4.particle('に', 'ni');
      const natte = b4.aux({ lemma: 'なる' }, 'natte');
      const iru = b4.aux({ lemma: 'いる' }, 'iru');
      const masu = b4.aux({ lemma: 'ます' }, 'masu');

      // Structural constraints
      b4.inOrder(noun, ga, 1);
      b4.inOrder(ga, ki);
      b4.inOrder(ki, ni, 1);
      b4.inOrder(ni, natte, 1);
      b4.inOrder(natte, iru, 1);
      b4.inOrder(iru, masu, 1);

      // Dependency constraints
      b4.caseMarker(noun, ga);  // noun --case--> ga
      b4.caseMarker(ki, ni);    // ki --case--> ni
      b4.auxOf(natte, ni);      // ni --obl/aux--> natte
      b4.auxOf(iru, natte);     // iru --aux--> natte
      b4.auxOf(masu, iru);      // masu --aux--> iru

      b4.captureSpan('が気になっています', noun, masu);
    }
  );
});
