import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('し-し', (r) => {
  // し-し (listing reasons)
  // A conjunction particle (接続助詞) used for listing multiple reasons
  // Patterns:
  // - Verb + し: 食べるし, 行ったし, できるし, 捨てたし
  // - い-adjective + し: 高いし, 可愛いし, 美味しいし, 良いし
  // - な-adjective + だ + し: 親切だし, 綺麗だし, 静かだし
  // - Noun + だ + し: 休みだし, 晴れだし, 真面目だし
  //
  // The particle し is typically tagged as SCONJ (接続助詞 - conjunction particle)
  // that connects clauses, but GiNZA sometimes tags it as AUX with dep=aux.
  // It indicates multiple reasons of equal importance.

  r.either(
    // Pattern 1: Verb (with or without auxiliaries) + し
    // 食べるし, 行ったし, できるし, 捨てたし, 降ってたし
    // Match any VERB or AUX before the し particle
    (b) => {
      const verbOrAux = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verbOrAux');
      const shi = b.tok({
        text: 'し',
        posOneOf: ['SCONJ', 'AUX'],
        depOneOf: ['mark', 'aux'],
      }, 'shi');
      b.inOrder(verbOrAux, shi, 1);
      b.captureSpan('し', verbOrAux, shi);
    },

    // Pattern 2: い-adjective + し
    // 高いし, 可愛いし, 美味しいし, 良いし, 賢いし
    (b) => {
      const iAdj = b.tok({
        pos: 'ADJ',
        tagOneOf: ['形容詞-一般', '形容詞-非自立可能'],
      }, 'iAdj');
      const shi = b.tok({
        text: 'し',
        posOneOf: ['SCONJ', 'AUX'],
        depOneOf: ['mark', 'aux'],
      }, 'shi');
      b.inOrder(iAdj, shi, 1);
      b.captureSpan('し', iAdj, shi);
    },

    // Pattern 3: な-adjective + だ + し
    // 親切だし, 綺麗だし, 静かだし
    (b) => {
      const naAdj = b.adj({
        tag: '形状詞-一般',
      }, 'naAdj');
      const da = b.aux({
        lemma: 'だ',
      }, 'da');
      const shi = b.tok({
        text: 'し',
        posOneOf: ['SCONJ', 'AUX'],
        depOneOf: ['mark', 'aux'],
      }, 'shi');
      b.inOrder(naAdj, da, 1);
      b.inOrder(da, shi, 1);
      b.captureSpan('だし', naAdj, shi);
    },

    // Pattern 4: Noun + だ + し
    // 休みだし, 晴れだし, 真面目だし
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-普通名詞-サ変形状'],
      }, 'noun');
      const da = b.aux({
        lemma: 'だ',
      }, 'da');
      const shi = b.tok({
        text: 'し',
        posOneOf: ['SCONJ', 'AUX'],
        depOneOf: ['mark', 'aux'],
      }, 'shi');
      b.inOrder(noun, da, 1);
      b.inOrder(da, shi, 1);
      b.captureSpan('だし', noun, shi);
    }
  );
});
