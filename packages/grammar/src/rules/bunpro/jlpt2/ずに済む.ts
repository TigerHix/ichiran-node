import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ずに済む', (r) => {
  r.either(
    (b1) => {
      const zuni = b1.tok({ text: 'ずに', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'zuni');
      const sumu = b1.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'], posOneOf: ['VERB', 'AUX'] }, 'sumu');
      b1.inOrder(zuni, sumu, 3);
      b1.captureSpan('ずに済む', zuni, sumu);
    },
    (b2) => {
      const zu = b2.tok({ text: 'ず', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'zu');
      const ni = b2.particle('に', 'ni');
      const sumu = b2.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'], posOneOf: ['VERB', 'AUX'] }, 'sumu');
      b2.inOrder(zu, ni, 1);
      b2.inOrder(ni, sumu, 2);
      b2.captureSpan('ずに済む', zu, sumu);
    },
    (b3) => {
      const zuni = b3.tok({ text: 'ずに' }, 'zuni');
      const sumu = b3.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'] }, 'sumu');
      b3.inOrder(zuni, sumu, 3);
      b3.headChild(sumu, zuni, 'aux');
      b3.captureSpan('ずに済む', zuni, sumu);
    },
    (b4) => {
      const zu = b4.tok({ text: 'ず' }, 'zu');
      const ni = b4.particle('に', 'ni');
      const sumu = b4.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'] }, 'sumu');
      b4.inOrder(zu, ni, 1);
      b4.inOrder(ni, sumu, 2);
      b4.headChild(sumu, zu, 'fixed');
      b4.headChild(sumu, ni, 'fixed');
      b4.captureSpan('ずに済む', zu, sumu);
    },
    (b5) => {
      const zuni = b5.tok({ text: 'ずに' }, 'zuni');
      const sumu = b5.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'] }, 'sumu');
      b5.inOrder(zuni, sumu, 3);
      b5.captureSpan('ずに済む', zuni, sumu);
    },
    (b6) => {
      const zu = b6.tok({ text: 'ず' }, 'zu');
      const ni = b6.particle('に', 'ni');
      const sumu = b6.tok({ textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'] }, 'sumu');
      b6.inOrder(zu, ni, 1);
      b6.inOrder(ni, sumu, 2);
      b6.captureSpan('ずに済む', zu, sumu);
    }
  );
});
