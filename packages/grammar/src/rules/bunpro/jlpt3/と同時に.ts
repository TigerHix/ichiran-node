import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: と同時に (at the same time as)
 *
 * Noun/Verb + と同時に = simultaneously with / at the same time as / as well as
 *
 * Connects two events or states happening at the same time.
 *
 * Examples:
 * - 目覚めると同時に着替えて出かけた (The moment I woke up, I got dressed and went out)
 * - サイレンが鳴ったと同時に、犯人は逃げた (As the siren went off, the criminal ran away)
 * - 野菜と同時に肉を鍋に入れてください (Please throw the vegetables and meat into the pot at the same time)
 * - その人は博士であると同時に宇宙飛行士でもある (That person is a scientist and also an astronaut)
 *
 * GiNZA parsing notes:
 * - 同時/どうじ (NOUN or ADV depending on form)
 * - に (ADP, lemma=に, dep=case/obl)
 * - と (ADP, lemma=と, dep=case/mark)
 * - である (AUX, lemma=だ) - can be omitted with nouns
 *
 * Note: Test sentences use hiragana "どうじに" from answer field, but actual Japanese uses kanji "同時に"
 */
export default linguisticRule('と同時に', (r) => {
  r.either(
    // Pattern 1: Verb/Aux (dictionary form/past) + と + 同時/どうじ + に
    // 目覚める + と + 同時 + に
    // 鳴った + と + どうじ + に
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const to = b.particle('と', 'to');
      const douji = b.tok({
        textOneOf: ['同時', 'どうじ'],
      }, 'douji');
      const ni = b.tok({
        text: 'に',
        depOneOf: ['case', 'obl'],
      }, 'ni');
      b.inOrder(verb, to);
      b.inOrder(to, douji, 1);
      b.inOrder(douji, ni, 1);
      b.captureSpan('と同時に', verb, ni);
    },

    // Pattern 2: Noun + と + 同時/どうじ + に
    // 自宅 + と + 同時 + に
    // 野菜 + と + どうじ + に
    (b) => {
      const noun = b.noun({}, 'noun');
      const to = b.particle('と', 'to');
      const douji = b.tok({
        textOneOf: ['同時', 'どうじ'],
      }, 'douji');
      const ni = b.tok({
        text: 'に',
        depOneOf: ['case', 'obl'],
      }, 'ni');
      b.inOrder(noun, to, 1);
      b.inOrder(to, douji, 1);
      b.inOrder(douji, ni, 1);
      b.captureSpan('と同時に', noun, ni);
    },

    // Pattern 3: Noun + である + と + 同時/どうじ + に
    // 博士 + である + と + 同時 + に
    // 便利 + である + と + どうじ + に
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'だ', text: 'である' }, 'dearu');
      const to = b.particle('と', 'to');
      const douji = b.tok({
        textOneOf: ['同時', 'どうじ'],
      }, 'douji');
      const ni = b.tok({
        text: 'に',
        depOneOf: ['case', 'obl'],
      }, 'ni');
      b.inOrder(noun, dearu, 1);
      b.inOrder(dearu, to, 1);
      b.inOrder(to, douji, 1);
      b.inOrder(douji, ni, 1);
      b.captureSpan('と同時に', noun, ni);
    }
  );
});
