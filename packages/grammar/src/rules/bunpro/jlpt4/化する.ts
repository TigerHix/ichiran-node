import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: 化する (to become, to -ize)
 *
 * Noun + 化 + する = to transform into, to become
 * English equivalent: "-ify", "-ize" (e.g., digitize, automate, simplify)
 *
 * Patterns:
 * 1. Noun + 化/か + する (dictionary form): 商品化する (to commercialize)
 * 2. Noun + 化/か + した (past form): 単純化した (simplified)
 * 3. Noun + 化/か + され (passive): 安定化され (stabilized)
 * 4. Noun + 化/か + の (noun form): 高齢化の (aging)
 * 5. Noun + 化/か + が/は (standalone as noun): 映画化が (film adaptation)
 *
 * Test sentences use hiragana "か" (from fill-in-the-blank exercises).
 * In actual Japanese text, this is written as kanji "化".
 *
 * NOTE: Pattern 5 intentionally only matches specific case markers (が, は)
 * to avoid false positives with:
 * - Question particles: "雨か？" (is it rain?)
 * - Alternative particles: "本か雑誌" (book or magazine)
 */
export default linguisticRule('化する', (r) => {
  r.either(
    // Pattern 1: Noun/Adj + 化/か + する (dictionary form)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const suru = b.verb({ lemma: 'する' }, 'suru');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, suru, 1);
      b.captureSpan('化する', base, suru);
    },

    // Pattern 2: Noun/Adj + 化/か + した (past form)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'] }, 'ta');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, suru, 1);
      b.auxOf(suru, ta);
      b.captureSpan('化する', base, ta);
    },

    // Pattern 3: Noun/Adj + 化/か + され (passive)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      const rare = b.aux({ lemma: 'れ' }, 'rare');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, suru, 1);
      b.auxOf(suru, rare);
      b.captureSpan('化する', base, rare);
    },

    // Pattern 4: Noun/Adj + 化/か + の (noun form with particle)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const no = b.particle('の', 'no');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, no, 1);
      b.captureSpan('化する', base, no);
    },

    // Pattern 5: Noun/Adj + 化/か + が/は (standalone noun with case marker)
    // Only specific case markers to avoid matching question/alternative particles
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const particle = b.particle('が', 'ga');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, particle, 1);
      b.captureSpan('化する', base, ka);
    },

    // Pattern 5b: Same with は (topic marker)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const particle = b.particle('は', 'wa');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, particle, 1);
      b.captureSpan('化する', base, ka);
    },

    // Pattern 5c: Noun + か + だ (copula)
    // Matches "自動かだ" (it is automation)
    (b) => {
      const base = b.tok({ posOneOf: ['NOUN', 'ADJ', 'VERB'] }, 'base');
      const ka = b.tok({ textOneOf: ['化', 'か'] }, 'ka');
      const da = b.aux({ lemma: 'だ' }, 'da');

      b.inOrder(base, ka, 1);
      b.inOrder(ka, da, 1);
      b.captureSpan('化する', base, ka);
    }
  );
});
