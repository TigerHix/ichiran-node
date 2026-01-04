import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ながらも', (r) => {
  // ながらも (nagara mo) - "although", "even while", "despite"
  // Expresses two contradictory states that coexist, often with surprise
  //
  // Patterns:
  // 1. Verb[stem] + ながらも: 思いながらも, 持ちながらも, 知っていながらも, 緊張しながらも
  // 2. I-adjective + ながらも: 狭いながらも
  // 3. Na-adjective + ながらも: 貧乏ながらも, 不本意ながらも, 微力ながらも, 豪快ながらも
  // 4. Noun + でありながらも: 敵でありながらも (copula である + で + ながらも)
  //
  // Note: Unlike regular ながら (while doing action), ながらも is used
  // primarily with state-expressing verbs and adjectives, not action verbs.

  r.either(
    // Pattern 1: Verb[stem] + ながらも
    // 思いながらも, 持ちながらも, 知っていながらも, 緊張しながらも, 感じながらも
    // Verb stem is in 連用形-一般 (conjunctive form/masu-stem)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'verb');
      const nagara = b.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ'],
      }, 'nagara');
      const mo = b.particle('も', 'mo');
      b.inOrder(verb, nagara, 1);
      b.inOrder(nagara, mo, 1);
      b.captureSpan('ながらも', verb, mo);
    },

    // Pattern 2: I-adjective + ながらも
    // 狭いながらも
    (b) => {
      const iAdj = b.adj({
        tag: '形容詞-一般',
      }, 'iAdj');
      const nagara = b.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ'],
      }, 'nagara');
      const mo = b.particle('も', 'mo');
      b.inOrder(iAdj, nagara, 1);
      b.inOrder(nagara, mo, 1);
      b.captureSpan('ながらも', iAdj, mo);
    },

    // Pattern 3: Na-adjective/Noun + ながらも
    // 貧乏ながらも, 不本意ながらも, 微力ながらも, 豪快ながらも
    // These are NOUN/ADJ/VERB with tags 名詞-普通名詞-* or 形状詞-一般
    // Note: GiNZA parses many na-adjectives and nouns as VERB with noun-like tags
    (b) => {
      const naAdj = b.tok({
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
        tagOneOf: [
          '名詞-普通名詞-一般',
          '名詞-普通名詞-サ変可能',
          '名詞-普通名詞-サ変形状詞可能',
          '名詞-普通名詞-形状詞可能',
          '形状詞-一般',
        ],
      }, 'naAdj');
      const nagara = b.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ'],
      }, 'nagara');
      const mo = b.particle('も', 'mo');
      b.inOrder(naAdj, nagara, 1);
      b.inOrder(nagara, mo, 1);
      b.captureSpan('ながらも', naAdj, mo);
    },

    // Pattern 4: Noun + でありながらも
    // 敵でありながらも (copula である + で + ながら + も)
    // "である" is AUX (copula in 連体形), "で" is AUX (conjunctive form)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
        tagOneOf: [
          '名詞-普通名詞-一般',
          '名詞-普通名詞-サ変可能',
          '名詞-普通名詞-サ変形状詞可能',
          '名詞-普通名詞-形状詞可能',
          '形状詞-一般',
        ],
      }, 'noun');
      const dearu = b.aux({
        lemma: 'だ',
        text: 'である',
        pos: 'AUX',
      }, 'dearu');
      const de = b.aux({
        lemma: 'だ',
        text: 'で',
        pos: 'AUX',
      }, 'de');
      const nagara = b.tok({
        text: 'ながら',
        posOneOf: ['PART', 'SCONJ'],
      }, 'nagara');
      const mo = b.particle('も', 'mo');
      b.inOrder(noun, dearu, 1);
      b.inOrder(dearu, de, 1);
      b.inOrder(de, nagara, 1);
      b.inOrder(nagara, mo, 1);
      b.captureSpan('ながらも', noun, mo);
    }
  );
});
