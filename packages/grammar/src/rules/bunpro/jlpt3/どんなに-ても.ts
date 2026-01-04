import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('どんなに-ても', (r) => {
  r.either(
    // Pattern 1: どんな + に + Verb[て-form] + ても
    // e.g., どんなに頑張っても, どんなに勉強しても, どんなに笑われても
    // GiNZA: どんな(ADJ) + に(AUX,lemma=だ,infl=連用形-ニ) + verb --mark--> て/で (SCONJ) --case--> も (ADP)
    // Note: Passive verbs like 笑われる have stem (笑わ) in 未然形-一般
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      b.auxOf(donna, ni);
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
          '未然形-一般',  // For passive/potential forms like 笑われても
        ],
      }, 'verb');
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(verb, te, 'mark');
      b.headChild(verb, mo, 'case');
      b.inOrder(ni, verb, 10);
      b.captureSpan('どんなに-ても', donna, mo);
    },

    // Pattern 2: どんな + に + い-Adj[て-form] + ても
    // e.g., どんなにつまらなくても, どんなに苦しくても, どんなに悲しくても
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      b.auxOf(donna, ni);
      const adj = b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'adj');
      const te = b.tok({ textOneOf: ['て', 'で'], posOneOf: ['SCONJ', 'AUX'] }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.inOrder(ni, adj, 10);
      b.captureSpan('どんなに-ても', donna, mo);
    },

    // Pattern 3: どんな + に + Noun/Adj + でも
    // e.g., どんなに暇でも, どんなにハンサムでも, どんなにお金持ちでも
    // GiNZA: どんな(ADJ) + に(AUX,lemma=だ,infl=連用形-ニ) + noun + で(ADP,case) + も(ADP,case)
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      b.auxOf(donna, ni);
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'noun');
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(noun, de, 'case');
      b.headChild(noun, mo, 'case');
      b.inOrder(ni, noun, 10);
      b.captureSpan('どんなに-でも', donna, mo);
    },

    // Pattern 4: どんな + Noun + でも (without に)
    // e.g., どんな困難でも, どんなことでも
    // GiNZA: どんな(PRON/ADJ) + noun + で(AUX/cop or ADP/case) + も(ADP,case)
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const de = b.tok({
        text: 'で',
        posOneOf: ['ADP', 'AUX'],
        depOneOf: ['case', 'cop']
      }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(noun, mo, 'case');
      b.inOrder(donna, noun, 1);
      b.inOrder(noun, de, 1);
      b.inOrder(de, mo, 1);
      b.captureSpan('どんな-でも', donna, mo);
    },

    // Pattern 5: どんな + に + サ変 verb (noun + する) + ても
    // e.g., どんなに勉強しても, どんなに忠告しても
    // GiNZA: どんな(ADJ) + に(AUX) + noun(VERB) + し(AUX,infl=連用形-一般) + て(SCONJ,dep=mark) + も
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      b.auxOf(donna, ni);
      const sahen = b.verb({}, 'sahen');
      const suru = b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      b.auxOf(sahen, suru);
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(sahen, te, 'mark');
      b.headChild(sahen, mo, 'case');
      b.inOrder(ni, sahen, 10);
      b.captureSpan('どんなに-しても', donna, mo);
    },

    // Pattern 6: どんな + に + な-Adj + でも
    // e.g., どんなに綺麗でも, どんなに上手でも
    // GiNZA: どんな(ADJ) + に(AUX) + な-adj(ADJ) + で(AUX,lemma=だ,dep=aux) + も(ADP,dep=case)
    (b) => {
      const donna = b.tok({ text: 'どんな', posOneOf: ['ADJ', 'PRON'] }, 'donna');
      const ni = b.aux({ lemma: 'だ', inflectionForm: '連用形-ニ' }, 'ni');
      b.auxOf(donna, ni);
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', inflectionForm: '連用形-一般' }, 'de');
      b.auxOf(adj, de);
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(adj, mo, 'case');
      b.inOrder(ni, adj, 2);
      b.captureSpan('どんなに-でも', donna, mo);
    }
  );
});
