import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いくら-でも', (r) => {
  r.either(
    // Pattern 1: いくら + Verb[て-form] + ても
    // e.g., いくら言っても, いくら急いでも, いくら泣いても
    // GiNZA: いくら(ADV,advmod) + 連用形 verb --mark--> て/で (SCONJ) --case--> も (ADP)
    // Note: both て/で and も have the verb as head
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'verb');
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(verb, te, 'mark');
      b.headChild(verb, mo, 'case');
      b.inOrder(ikura, verb, 10);
      b.captureSpan('いくら-ても', ikura, mo);
    },
    // Pattern 2: いくら + い-Adj[て-form] + ても
    // e.g., いくら新しくても, いくらかわいくても
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const adj = b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
          '終止形-一般',  // For edge cases like いくらないても
        ],
      }, 'adj');
<<<<<<< HEAD
      const _te = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      void _te;
=======
      const te = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
>>>>>>> jlpt3-ikura-demo
      b.inOrder(ikura, adj, 10);
      b.captureSpan('いくら-ても', ikura, mo);
    },
    // Pattern 3: いくら + Noun/Pron + でも
    // e.g., いくら社長でも, いくら俺でも
    // GiNZA: いくら(ADV,advmod) + noun/pron + で(ADP,case) + も(ADP,case)
    // Note: both で and も have the noun as head
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(noun, de, 'case');
      b.headChild(noun, mo, 'case');
      b.inOrder(ikura, noun, 10);
      b.captureSpan('いくら-でも', ikura, mo);
    },
    // Pattern 4: いくらでも standing alone
    // e.g., 相談ならいくらでも聞いてあげるよ, お菓子ならいくらでもある
    // GiNZA: いくら(NOUN,obl) + で(ADP,case) + も(ADP,case)
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'NOUN' }, 'ikura');
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(ikura, de, 'case');
      b.headChild(ikura, mo, 'case');
      b.captureSpan('いくらでも', ikura, mo);
    },
    // Pattern 5: いくら + サ変 verb (noun + する) + ても
    // e.g., いくら勉強しても, いくら忠告しても
    // GiNZA: noun(VERB,infl=undefined) + し(AUX,infl=連用形-一般) + て(SCONJ,dep=mark) + も
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const sahen = b.verb({}, 'sahen');
      const suru = b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      b.auxOf(sahen, suru);
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(sahen, te, 'mark');
      b.headChild(sahen, mo, 'case');
      b.inOrder(ikura, sahen, 10);
      b.captureSpan('いくら-しても', ikura, mo);
    },
    // Pattern 6: いくら + な-Adj + でも
    // e.g., いくら優秀でも, いくら嫌いでも
    // GiNZA: な-adj(ADJ) + で(AUX,lemma=だ,dep=aux) + も(ADP,dep=case)
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', inflectionForm: '連用形-一般' }, 'de');
      b.auxOf(adj, de);
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(adj, mo, 'case');
      b.inOrder(ikura, adj, 2);
      b.captureSpan('いくら-でも', ikura, mo);
    },
    // Pattern 7: いくら + noun-adj + にしても (idiomatic)
    // e.g., いくら親切にしても
    // GiNZA: adj(ADJ) + に(SCONJ,lemma=だ,dep=mark,infl=連用形-ニ) + し(SCONJ,dep=fixed) + て(SCONJ,dep=fixed) + も(SCONJ,dep=fixed)
    (b) => {
      const ikura = b.tok({ text: 'いくら', lemma: 'いくら', pos: 'ADV' }, 'ikura');
      const adj = b.adj({}, 'adj');
      const ni = b.tok({ text: 'に', lemma: 'だ', pos: 'SCONJ', inflectionForm: '連用形-ニ', dep: 'mark' }, 'ni');
      b.headChild(adj, ni, 'mark');
      const shi = b.tok({ text: 'し', lemma: 'する', pos: 'SCONJ', dep: 'fixed' }, 'shi');
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'fixed' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'SCONJ', dep: 'fixed' }, 'mo');
      b.headChild(ni, shi, 'fixed');
      b.headChild(ni, te, 'fixed');
      b.headChild(ni, mo, 'fixed');
      b.inOrder(ikura, adj, 2);
      b.captureSpan('いくら-にしても', ikura, mo);
    }
  );
});
