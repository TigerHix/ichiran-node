// ichiran/dict-errata - Errata, corrections, and special word lists
// Port of dict-errata.lisp

import type { ConjProp, ConjData } from './types.js';

// Line 1092-1115: defparameter *skip-words*
export const SKIP_WORDS = [
  2458040,  // てもいい
  2822120,  // ても良い
  2013800,  // ちゃう
  2108590,  // とく
  2029040,  // ば
  2428180,  // い
  2654250,  // た
  2561100,  // うまいな
  2210270,  // ませんか
  2210710,  // ましょうか
  2257550,  // ない
  2210320,  // ません
  2017560,  // たい
  2394890,  // とる
  2194000,  // であ
  2568000,  // れる/られる
  2537250,  // しようとする
  2760890,  // 三箱
  2831062,  // てる
  2831063,  // てく
  2029030,  // ものの
  2568020,  // せる
  900000,   // たそう
];

// Line 1119-1130: defparameter *final-prt*
export const FINAL_PRT = [
  2017770,  // かい
  2425930,  // なの
  2130430,  // け っけ
  2029130,  // ぞ
  2834812,  // ぜ
  2718360,  // がな
  2201380,  // わい
  2722170,  // のう
  2751630,  // かいな
];

// Line 1133-1139: defparameter *semi-final-prt*
export const SEMI_FINAL_PRT = [
  ...FINAL_PRT,
  2029120,  // さ
  2086640,  // し
  2029110,  // な
  2029080,  // ね
  2029100,  // わ
];

// Line 1142-1144: defparameter *copulae*
export const COPULAE = [
  2089020,  // だ
];

// Line 1146-1149: defparameter *non-final-prt*
export const NON_FINAL_PRT = [
  2139720,  // ん
];

// Line 1151-1161: defparameter *no-kanji-break-penalty*
export const NO_KANJI_BREAK_PENALTY = [
  1169870,  // 飲む
  1198360,  // 会議
  1277450,  // 好き
  2028980,  // で
  1423000,  // 着る
  1164690,  // 一段
  1587040,  // 言う
  2827864,  // なので
];

// Line 1163-1164: defparameter *force-kanji-break*
export const FORCE_KANJI_BREAK = ['です'];

// Line 1168-1172: Conjugation type constants
export const CONJ_ADVERBIAL = 50;
export const CONJ_ADJECTIVE_STEM = 51;
export const CONJ_NEGATIVE_STEM = 52;
export const CONJ_CAUSATIVE_SU = 53;
export const CONJ_ADJECTIVE_LITERARY = 54;

// Line 1248-1253: defparameter *weak-conj-forms*
// Format: [conj-type, neg, fml] or [pos, conj-type, neg, fml]
export const WEAK_CONJ_FORMS: Array<[number, any, any] | [string, number, any, any]> = [
  [CONJ_ADJECTIVE_STEM, ':any', ':any'],
  [CONJ_NEGATIVE_STEM, ':any', ':any'],
  [CONJ_CAUSATIVE_SU, ':any', ':any'],
  [CONJ_ADJECTIVE_LITERARY, ':any', ':any'],
  [9, true, ':any'],
];

// Line 1242-1246: defparameter *skip-conj-forms*
// Format: [conj-type, neg, fml] or [pos, conj-type, neg, fml]
export const SKIP_CONJ_FORMS: Array<[number, any, any] | [string, number, any, any]> = [
  [10, true, ':any'],
  [3, true, true],
  ['vs-s', 5, ':any', ':any'],
];

// Line 1255-1262: defun test-conj-prop
export function testConjProp(prop: ConjProp, forms: Array<[number, any, any] | [string, number, any, any]>): boolean {
  const propList = [prop.pos, prop.conjType, prop.neg, prop.fml];

  return forms.some(sk => {
    if (sk.length === 3) {
      // Match [conj-type, neg, fml]
      return propList.slice(1).every((l, i) => sk[i] === ':any' || l === sk[i]);
    } else if (sk.length === 4) {
      // Match [pos, conj-type, neg, fml]
      return propList[0] === sk[0] &&
             propList.slice(1).every((l, i) => sk[i + 1] === ':any' || l === sk[i + 1]);
    }
    return false;
  });
}

// Line 1264-1267: defun skip-by-conj-data
export function skipByConjData(conjData: ConjData[] | null): boolean {
  if (!conjData || conjData.length === 0) return false;
  return conjData.every(cd => testConjProp(cd.prop, SKIP_CONJ_FORMS));
}