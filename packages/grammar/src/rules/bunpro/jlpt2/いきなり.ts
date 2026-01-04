import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いきなり', (r) => {
  // いきなり (ikinari) - adverb meaning "suddenly, all of a sudden, abruptly"
  // Used when something happens without warning or unexpectedly
  //
  // From the Bunpro examples, いきなり appears in these contexts:
  // - At sentence beginning: いきなり電話してごめん (Sorry for calling suddenly)
  // - Before verbs: いきなり雨が降ってきた (Suddenly started raining)
  // - Before noun phrases: いきなりの展開 (sudden development)
  // - In compound forms: 出会っていきなり (met and suddenly)
  //
  // GiNZA parsing notes:
  // - いきなり is tagged as ADV (adverb)
  // - Can appear at various positions in the sentence
  // - Sometimes modifies verbs directly (advmod dep)
  // - Sometimes modifies nouns (through adnominal use with の)
  //
  // Key examples from test data:
  // 1. Sentence-initial: いきなり電話してごめん
  // 2. Before verb phrase: いきなり雨が降ってきた
  // 3. Before noun with の: いきなりの展開
  // 4. After te-form: 出会っていきなり結婚を申し込む

  const ikinari = r.adv({ text: 'いきなり' }, 'ikinari');

  r.capture(ikinari);
});
