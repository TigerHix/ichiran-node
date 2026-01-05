import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('たとえ-ても', (r) => {
  r.either(
    // Pattern 1: たとえ + Verb[て-form] + ても
    // e.g., たとえ試合で勝っても, たとえ雪が降らなくても, たとえ笑われても
    // GiNZA: たとえ(ADV) + verb[renyou] --mark--> て(SCONJ) + も(ADP,case)
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const verb = b.verb({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
          '未然形-一般',  // For verb+aux patterns like 降らない+ても, 笑われる+ても
        ],
      }, 'verb');
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(verb, te, 'mark');
      b.headChild(verb, mo, 'case');
      b.inOrder(tatoe, verb, 10);
      b.captureSpan('たとえ-ても', tatoe, mo);
    },
    // Pattern 2: たとえ + い-Adj[て-form] + ても
    // e.g., たとえ暑くても, たとえ嬉しくても, たとえ忙しくても
    // GiNZA: たとえ(ADV) + い-adj[renyou] + て/で(SCONJ/AUX) + も(ADP,case)
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const adj = b.adj({
        inflectionFormOneOf: [
          '連用形-イ音便',
          '連用形-ウ音便',
          '連用形-ニ',
          '連用形-一般',
          '連用形-促音便',
          '連用形-撥音便',
          '連用形-融合',
        ],
      }, 'adj');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.inOrder(tatoe, adj, 10);
      b.captureSpan('たとえ-ても', tatoe, mo);
    },
    // Pattern 3: たとえ + な-Adj + でも
    // e.g., たとえ好きでも, たとえ嫌いでも
    // GiNZA: たとえ(ADV) + な-adj(ADJ) + で(AUX,lemma=だ,dep=aux) + も(ADP,dep=case)
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const adj = b.adj({}, 'adj');
      const de = b.aux({ lemma: 'だ', inflectionForm: '連用形-一般' }, 'de');
      b.auxOf(adj, de);
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(adj, mo, 'case');
      b.inOrder(tatoe, adj, 2);
      b.captureSpan('たとえ-でも', tatoe, mo);
    },
    // Pattern 4: たとえ + Noun/Pron + でも
    // e.g., たとえ子供でも, たとえ電車でも, たとえあなたでも
    // GiNZA: たとえ(ADV,advmod) + noun/pron + で(ADP,case) + も(ADP,case)
    // Note: both で and も have the noun as head
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const noun = b.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN'] }, 'noun');
      const de = b.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(noun, de, 'case');
      b.headChild(noun, mo, 'case');
      b.inOrder(tatoe, noun, 10);
      b.captureSpan('たとえ-でも', tatoe, mo);
    },
    // Pattern 5: たとえ + サ変 verb (noun + する) + ても
    // e.g., たとえ短い時間でも (special case: time + de + mo)
    // Also handles サ変 compound patterns
    // GiNZA: noun(VERB,infl=undefined) + し(AUX,infl=連用形-一般) + て(SCONJ,dep=mark) + も
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const sahen = b.verb({}, 'sahen');
      const suru = b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'suru');
      b.auxOf(sahen, suru);
      const te = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      const mo = b.tok({ text: 'も', pos: 'ADP', dep: 'case' }, 'mo');
      b.headChild(sahen, te, 'mark');
      b.headChild(sahen, mo, 'case');
      b.inOrder(tatoe, sahen, 10);
      b.captureSpan('たとえ-しても', tatoe, mo);
    },
    // Pattern 6: たとえ + としても (complex volitional)
    // e.g., たとえ太ったとしても, たとえ完成したとしても
    // GiNZA: verb/adj + と(SCONJ/ADP) + し(AUX/SCONJ) + て(SCONJ) + も
    (b) => {
      const tatoe = b.tok({ text: 'たとえ', lemma: 'たとえ', pos: 'ADV' }, 'tatoe');
      const pred = b.tok({ posOneOf: ['VERB', 'ADJ'] }, 'pred');
      const mo = b.tok({ text: 'も', pos: 'SCONJ' }, 'mo');
      b.inOrder(tatoe, pred, 10);
      b.captureSpan('たとえ-としても', tatoe, mo);
    }
  );
});
