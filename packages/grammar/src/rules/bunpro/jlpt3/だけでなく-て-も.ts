import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だけでなく-て-も', (r) => {
  // だけでなく(て)～も - "not only X but also Y"
  // This rule extends JLPT4's だけでなく by requiring も particle after the second element
  //
  // Patterns:
  // 1. Formal: だけでなく、Noun+も / だけでなくて、Noun+も
  // 2. Written: だけではなく、Noun+も / だけではなくて、Noun+も
  // 3. Casual: だけじゃなく、Noun+も / だけじゃなくて、Noun+も
  //
  // The key difference from JLPT4 だけでなく is the REQUIRED も particle
  // following the second noun/element, emphasizing inclusion of both items.

  const dake = r.tok({ lemma: 'だけ' }, 'dake');

  r.either(
    // Pattern 1: だけでなく (formal, no て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, de, 1).inOrder(de, nai, 2);
      // Require も to appear somewhere later in the sentence
      b.inOrder(nai, mo, 10);
      b.captureSpan('だけでなく', dake, nai);
    },

    // Pattern 1b: ではなく (formal with は, no て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const wa = b.tok({ text: 'は' }, 'wa');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, de, 1);
      b.inOrder(de, wa, 1);
      b.inOrder(wa, nai, 1);
      // Require も to appear somewhere later in the sentence
      b.inOrder(nai, mo, 10);
      b.captureSpan('ではなく', dake, nai);
    },

    // Pattern 2: だけでなくて (formal with て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const te = b.aux({ lemma: 'て' }, 'te');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, de, 1).inOrder(de, nai, 2).inOrder(nai, te, 1);
      // Require も to appear somewhere later in the sentence
      b.inOrder(te, mo, 10);
      b.captureSpan('だけでなくて', dake, te);
    },

    // Pattern 2b: ではなくて (formal with は and て)
    (b) => {
      const de = b.tok({
        text: 'で',
        lemmaOneOf: ['で', 'だ'],
        posOneOf: ['AUX', 'ADP'],
        depOneOf: ['cop', 'aux', 'case', 'fixed']
      }, 'de');
      const wa = b.tok({ text: 'は', dep: 'fixed' }, 'wa');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, de, 1);
      b.inOrder(de, wa, 1);
      b.inOrder(wa, nai, 1);
      b.inOrder(nai, te, 1);
      // Require も to appear somewhere later in the sentence
      b.inOrder(te, mo, 10);
      b.captureSpan('ではなくて', dake, te);
    },

    // Pattern 3: だけじゃなく (casual, no て)
    (b) => {
      const ja = b.tok({
        text: 'じゃ',
        lemma: 'だ'
      }, 'ja');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2);
      // Require も to appear somewhere later in the sentence
      b.inOrder(nai, mo, 10);
      b.captureSpan('だけじゃなく', dake, nai);
    },

    // Pattern 4: だけじゃなくて (casual with て)
    (b) => {
      const ja = b.tok({
        text: 'じゃ',
        lemma: 'だ'
      }, 'ja');
      const nai = b.aux({
        text: 'なく',
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');
      const te = b.aux({ lemma: 'て' }, 'te');
      const mo = b.tok({ text: 'も' }, 'mo');
      b.inOrder(dake, ja, 1).inOrder(ja, nai, 2).inOrder(nai, te, 1);
      // Require も to appear somewhere later in the sentence
      b.inOrder(te, mo, 10);
      b.captureSpan('だけじゃなくて', dake, te);
    }
  );
});
