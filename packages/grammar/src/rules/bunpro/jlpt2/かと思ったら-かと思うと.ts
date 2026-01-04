import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('かと思ったら-かと思うと', (r) => {
  // JLPT2: かと思ったら・かと思うと (the moment that, just when I thought)
  // Pattern: Verb[Ta/Ru] + かと思ったら/かと思うと
  // Also: Noun/Adj + かと思ったら/かと思うと
  // Variant: Without か (と思ったら/思うと)
  // Meaning: "Just when I thought X, Y happened" / "No sooner than X than Y"

  r.either(
    // Pattern 1: Any predicate + かと思ったら (split: おもっ+たら)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'NOUN', 'ADJ'] }, 'pred');
      const ka = b.tok({ text: 'か', pos: 'PART', dep: 'mark' }, 'ka');
      const toQuote = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toQuote');
      const omou = b.tok({ pos: 'VERB', textOneOf: ['おもっ', '思っ'], lemma: '思う' }, 'omou');
      const tara = b.aux({ text: 'たら', lemma: 'た' }, 'tara');
      b.inOrder(pred, ka, 10);
      b.inOrder(ka, toQuote, 2);
      b.inOrder(toQuote, omou, 2);
      b.inOrder(omou, tara, 2);
      b.auxOf(omou, tara);
      b.captureAs('predicate', pred);
      b.captureSpan('かと思ったら-かと思うと', pred, tara);
    },
    // Pattern 2: Any predicate + かと思うと (おもう + と)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'NOUN', 'ADJ'] }, 'pred');
      const ka = b.tok({ text: 'か', pos: 'PART', dep: 'mark' }, 'ka');
      const toQuote = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toQuote');
      const omou = b.tok({ pos: 'VERB', textOneOf: ['おもう', '思う'], lemma: '思う' }, 'omou');
      const toMark = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toMark');
      b.inOrder(pred, ka, 10);
      b.inOrder(ka, toQuote, 2);
      b.inOrder(toQuote, omou, 2);
      b.inOrder(omou, toMark, 2);
      b.headChild(omou, toMark, 'case');
      b.captureAs('predicate', pred);
      b.captureSpan('かと思ったら-かと思うと', pred, toMark);
    },
    // Pattern 3: Any predicate + と思ったら (without か, split: おもっ+たら)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'NOUN', 'ADJ'] }, 'pred');
      const toQuote = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toQuote');
      const omou = b.tok({ pos: 'VERB', textOneOf: ['おもっ', '思っ'], lemma: '思う' }, 'omou');
      const tara = b.aux({ text: 'たら', lemma: 'た' }, 'tara');
      b.inOrder(pred, toQuote, 10);
      b.inOrder(toQuote, omou, 2);
      b.inOrder(omou, tara, 2);
      b.auxOf(omou, tara);
      b.captureAs('predicate', pred);
      b.captureSpan('かと思ったら-かと思うと', pred, tara);
    },
    // Pattern 4: Any predicate + と思うと (without か)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'NOUN', 'ADJ'] }, 'pred');
      const toQuote = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toQuote');
      const omou = b.tok({ pos: 'VERB', textOneOf: ['おもう', '思う'], lemma: '思う' }, 'omou');
      const toMark = b.tok({ text: 'と', pos: 'ADP', dep: 'case' }, 'toMark');
      b.inOrder(pred, toQuote, 10);
      b.inOrder(toQuote, omou, 2);
      b.inOrder(omou, toMark, 2);
      b.headChild(omou, toMark, 'case');
      b.captureAs('predicate', pred);
      b.captureSpan('かと思ったら-かと思うと', pred, toMark);
    }
  );
});
