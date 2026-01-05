import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: からする (karasuru) - worth at least X, starting at X price
 *
 * A formal expression indicating a minimum amount where a price or value starts.
 * Used primarily for money/prices to indicate something costs "X or more".
 *
 * Structure: Number + Counter + からする
 *
 * Examples:
 * - 家賃は１０万円からする。 (Rent costs 100,000 yen or more.)
 * - 三百万円からする時計をもらった。 (I received a watch worth 3,000,000 yen or more.)
 * - 普通は３０万円からするのに… (Normally it's 300,000 yen or more, but...)
 * - ２００万ドルからする車 (a car worth 2 million dollars or more)
 *
 * Key discriminators:
 * - から (particle): indicates starting point "from"
 * - する (verb): indicates valuation/cost in this context
 * - Used mainly for prices and monetary values
 * - Different from からの (for people/quantities) and からある (for weight/size)
 *
 * GiNZA parse structure:
 * - Number (NUM) + Counter (NOUN) + から (ADP/SCONJ) + する (VERB)
 * - から and する typically form compound/fixed dependencies
 *
 * Different from:
 * - からの (karano): for people, other non-price quantities
 * - からある (karaaru): for weight, size, distance
 * - からして (karashite): "judging from", "even"
 * - Number + もする (mo suru): colloquial emphasis
 */
export default bunproLinguisticRule('からする', (r) => {
  r.either(
    // Pattern 1: NUM + NOUN(counter) + から(ADP) + する(VERB) with compound
    (b1) => {
      const num = b1.tok({ pos: 'NUM' }, 'num');
      const counter = b1.noun({}, 'counter');
      const kara = b1.particle('から', 'kara', { pos: 'ADP' });
      const suru = b1.verb({ lemma: 'する' }, 'suru');

      b1.inOrder(num, counter, 1);
      b1.inOrder(counter, kara, 1);
      b1.inOrder(kara, suru, 1);
      b1.headChild(counter, kara, 'compound');
      b1.headChild(counter, suru, 'compound');

      b1.captureSpan('からする', num, suru);
    },

    // Pattern 2: NUM + NOUN(counter) + から(SCONJ) + する with fixed
    (b2) => {
      const num = b2.tok({ pos: 'NUM' }, 'num');
      const counter = b2.noun({}, 'counter');
      const kara = b2.particle('から', 'kara', { pos: 'SCONJ' });
      const suru = b2.verb({ lemma: 'する' }, 'suru');

      b2.inOrder(num, counter, 1);
      b2.inOrder(counter, kara, 1);
      b2.inOrder(kara, suru, 1);
      b2.headChild(counter, kara, 'fixed');
      b2.headChild(counter, suru, 'fixed');

      b2.captureSpan('からする', num, suru);
    },

    // Pattern 3: NOUN + から + する for currency nouns (e.g., "ドルからする")
    (b3) => {
      const amountNoun = b3.noun({}, 'amountNoun');
      const kara = b3.particle('から', 'kara');
      const suru = b3.verb({ lemma: 'する' }, 'suru');

      b3.inOrder(amountNoun, kara, 1);
      b3.inOrder(kara, suru, 1);
      b3.headChild(amountNoun, kara, 'compound');
      b3.headChild(amountNoun, suru, 'compound');

      b3.captureSpan('からする', amountNoun, suru);
    },

    // Pattern 4: Catch-all for loose dependencies with NUM + counter
    (b4) => {
      const num = b4.tok({ pos: 'NUM' }, 'num');
      const counter = b4.noun({}, 'counter');
      const kara = b4.particle('から', 'kara');
      const suru = b4.verb({ lemma: 'する' }, 'suru');

      b4.inOrder(num, counter, 3);  // Allow more distance between num and counter
      b4.inOrder(counter, kara, 3);
      b4.inOrder(kara, suru, 3);

      b4.captureSpan('からする', num, suru);
    },

    // Pattern 5: NUM + から + する (counter may be parsed as part of NUM)
    (b5) => {
      const num = b5.tok({ pos: 'NUM' }, 'num');
      const kara = b5.tok({ text: 'から' }, 'kara');  // Don't require particle POS
      const suru = b5.verb({ lemma: 'する' }, 'suru');

      b5.inOrder(num, kara, 3);
      b5.inOrder(kara, suru, 3);

      b5.captureSpan('からする', num, suru);
    }
  );
});
