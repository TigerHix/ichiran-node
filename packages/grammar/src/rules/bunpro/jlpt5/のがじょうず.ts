import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のがじょうず', (r) => {
  // Pattern: Verb (dictionary form) + の + が + 上手(じょうず)
  // Meaning: "Good at, skilled at" doing something
  // Example: 歌うのが上手 (good at singing)

  // The verb in dictionary form (plain form)
  const verb = r.verb({}, 'verb');

  // Nominalizer particle の
  // When の nominalizes a verb/phrase, GiNZA tags it as SCONJ with dep=mark
  const no = r.tok({
    text: 'の',
    tag: '助詞-準体助詞',
    pos: 'SCONJ',
    dep: 'mark',
  }, 'no');

  // Subject particle が
  const ga = r.particle('が', 'ga', {
    tag: '助詞-格助詞',
    dep: 'case',
  });

  // 上手 (じょうず) - skilled/good at
  // GiNZA may parse as ADJ or NOUN, and may be hiragana or kanji
  const jouzu = r.tok({
    lemmaOneOf: ['上手', 'じょうず'],
    posOneOf: ['NOUN', 'ADJ'],
  }, 'jouzu');

  // Structural constraints
  r.inOrder(verb, no, 2);       // verb immediately followed by の
  r.inOrder(no, ga, 1);         // の immediately followed by が
  r.inOrder(ga, jouzu, 1);      // が immediately followed by 上手

  // Capture the entire pattern
  r.captureSpan('のがじょうず', verb, jouzu);
});
