import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: と言える - "it can be said that", "one could say that"
 *
 * Matches phrase + と (quotational particle) + 言える (potential form of 言う)
 *
 * This pattern expresses that something can be said or is fair to say.
 * It's used to make a relatively confident but not absolute statement.
 * Often followed by conjectural auxiliaries like だろう/でしょう to soften the tone.
 *
 * Structure variations:
 * - Phrase + と言える - "it can be said that"
 * - Phrase + と言えるだろう - "one could probably say that" (casual conjecture)
 * - Phrase + と言えるでしょう - "one could probably say that" (polite conjecture)
 * - Phrase + と言えよう - "let it be said that" (archaic/literary volitional)
 * - Phrase + とも言える - "it can also be said" (with emphasis particle も)
 *
 * Examples:
 * - この儀式は日本の文化の一つと言える。
 *   (It is fair to say that this ceremony is part of Japan's culture.)
 * - 彼らは同時にゴールインしたと言えるだろう。
 *   (One could say that they reached the finish line at the same time.)
 * - 日本は独自の文化で国際的に有名だと言えるでしょう。
 *   (It would be fair to say that Japan is internationally famous for its unique culture.)
 *
 * This is different from:
 * - と言われている (JLPT2) - "it is said that" (passive/reported hearsay)
 * - といえば (JLPT3) - "speaking of" (topic marker)
 * - といってもいい (JLPT4) - "you could say" (more tentative)
 * - ということだ (JLPT3) - "it means that / I hear that" (reported information)
 *
 * GiNZA parse structure:
 * - と: ADP/particle (quotational/citation marker, dep=case or dep=mark)
 * - 言える: VERB (potential form of 言う, lemma=言える or 言う)
 */
export default linguisticRule('と言える', (r) => {
  // Quotational particle と (marks what is being said)
  // Note: GiNZA may tag と as ADP or PART
  const to = r.tok({
    text: 'と',
    posOneOf: ['ADP', 'PART'],
  }, 'to');

  r.either(
    // Pattern 1: と言える (basic form - "it can be said that")
    (b) => {
      const ieru = b.verb({
        textOneOf: ['言える', 'いえる'],
        lemmaOneOf: ['言える', 'いえる'],
      }, 'ieru');
      b.inOrder(to, ieru, 1);
      b.captureSpan('と言える', to, ieru);
    },

    // Pattern 2: と言えるだろう (casual conjecture - "one could probably say")
    (b) => {
      const ieru = b.verb({
        textOneOf: ['言える', 'いえる'],
        lemmaOneOf: ['言える', 'いえる'],
      }, 'ieru');
      const darou = b.aux({
        lemmaOneOf: ['だろう', 'であろう'],
      }, 'darou');
      b.inOrder(to, ieru, 1);
      b.auxOf(ieru, darou);
      b.captureSpan('と言えるだろう', to, darou);
    },

    // Pattern 3: と言えるでしょう (polite conjecture)
    (b) => {
      const ieru = b.verb({
        textOneOf: ['言える', 'いえる'],
        lemmaOneOf: ['言える', 'いえる'],
      }, 'ieru');
      const deshou = b.aux({
        lemmaOneOf: ['でしょう', 'でしょ'],
      }, 'deshou');
      b.inOrder(to, ieru, 1);
      b.auxOf(ieru, deshou);
      b.captureSpan('と言えるでしょう', to, deshou);
    },

    // Pattern 4: と言えよう (volitional form - "let it be said that")
    // This is a more archaic/literary form expressing "anyone would say this"
    // GiNZA may parse this in different ways:
    // 1. と + 言え/いえ (VERB stem) + よう (AUX, volitional)
    // 2. と + いえよう (single VERB token with volitional inflection)
    (b) => {
      const ieruStem = b.verb({
        textOneOf: ['言え', 'いえ'],
      }, 'ieruStem');
      const you = b.aux({
        lemma: 'よう',
        inflectionForm: '意志推量形',
      }, 'you');
      b.inOrder(to, ieruStem, 1);
      b.auxOf(ieruStem, you);
      b.captureSpan('と言えよう', to, you);
    },

    // Pattern 4b: と言えよう as single token (GiNZA sometimes parses volitional as single verb)
    (b) => {
      const ieruyou = b.tok({
        textOneOf: ['言えよう', 'いえよう'],
        inflectionForm: '意志推量形',
      }, 'ieruyou');
      b.inOrder(to, ieruyou, 1);
      b.captureSpan('と言えよう', to, ieruyou);
    },

    // Pattern 5: とも言える (with emphasis particle も - "it can ALSO be said")
    (b) => {
      const mo = b.particle('も', 'mo');
      const ieru = b.verb({
        textOneOf: ['言える', 'いえる'],
        lemmaOneOf: ['言える', 'いえる'],
      }, 'ieru');
      b.inOrder(to, mo, 1);
      b.inOrder(mo, ieru, 1);
      b.captureSpan('と言える', to, ieru);
    },

    // Pattern 6: とも言えるだろう (emphasized + conjecture)
    (b) => {
      const mo = b.particle('も', 'mo');
      const ieru = b.verb({
        textOneOf: ['言える', 'いえる'],
        lemmaOneOf: ['言える', 'いえる'],
      }, 'ieru');
      const darou = b.aux({
        lemmaOneOf: ['だろう', 'であろう'],
      }, 'darou');
      b.inOrder(to, mo, 1);
      b.inOrder(mo, ieru, 1);
      b.auxOf(ieru, darou);
      b.captureSpan('と言えるだろう', to, darou);
    }
  );
});
