import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('せっかく', (r) => {
  // せっかく - adverb meaning "with trouble, at great pains, take the trouble, might as well"
  // Indicates doing something with much effort or a rare/long-awaited occasion
  //
  // Patterns:
  // - せっかく + verb: せっかく休みを取ったのに, せっかく運動するなら
  // - せっかく + verb phrase: せっかく大学を卒業したんだから, せっかく買ったんだから
  // - せっかくの + noun: せっかくの誕生日会, せっかくのデート, せっかくの休暇

  // Note: GiNZA may tag せっかく as ADV, ADJ, or other tags depending on context
  // Accept both hiragana and kanji forms
  const sekkaku = r.tok({
    textOneOf: ['せっかく', '折角'],
  }, 'sekkaku');

  r.either(
    // Pattern 1: せっかく + の + noun
    // せっかくの誕生日会, せっかくのデート, せっかくの休暇
    // せっかくの花見シーズン, せっかくの友達との遠足
    (b) => {
      const no = b.particle('の', 'no');
      const noun = b.noun({}, 'noun');
      b.inOrder(sekkaku, no, 1);
      b.inOrder(no, noun, 2);
      b.captureSpan('せっかく', sekkaku, noun);
    },

    // Pattern 2: せっかく + noun/pronoun/det + particle + verb
    // せっかく休みを取ったのに, せっかく大学を卒業したんだから
    // せっかく君が欲しかった靴を買ってあげたのに
    // せっかく異世界に転移したのに (object particle に in "異世界に")
    // せっかくこの旅行の準備してきた (particle の in "旅行の")
    // Note: Requires noun/pronoun/det immediately after せっかく (not particle に)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'DET'] }, 'noun');
      const particle = b.tok({ textOneOf: ['を', 'に', 'の', 'が'], pos: 'ADP' }, 'particle');
      const verb = b.verb({}, 'verb');
      b.inOrder(sekkaku, noun, 1);
      b.inOrder(sekkaku, particle, 8);
      b.inOrder(particle, verb, 5);
      b.captureSpan('せっかく', sekkaku, verb);
    },

    // Pattern 3: せっかく + verb (close range, no particle に immediately after)
    // せっかく運動するなら, せっかく勉強したのに
    // Note: Must NOT have particle に immediately after せっかく
    (b) => {
      const notNi = b.tok({ text: 'に', pos: 'ADP' }, 'notNi');
      const verb = b.verb({}, 'verb');
      b.not(() => {
        b.inOrder(sekkaku, notNi, 1);
      });
      b.inOrder(sekkaku, verb, 5);
      b.captureSpan('せっかく', sekkaku, verb);
    },

    // Pattern 4: せっかく + non-に token + verb (catch-all for remaining cases)
    // Must NOT have particle に immediately after せっかく
    // This handles cases where the structure doesn't match above patterns
    (b) => {
      const notNi = b.tok({ text: 'に', pos: 'ADP' }, 'notNi');
      const verb = b.verb({}, 'verb');
      b.not(() => {
        b.inOrder(sekkaku, notNi, 1);
      });
      b.inOrder(sekkaku, verb, 10);
      b.captureSpan('せっかく', sekkaku, verb);
    }
  );
});
