// Counter tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { simpleSegment } from '../src/dict/presentation.js';
import { setupTests, extractTexts } from './test-setup.js';

setupTests();

describe('Counter Tests', () => {
  const counters = [
    "倍", "晩", "秒", "着", "挺", "丁", "台", "段", "度", "円", "服", "幅", "分", "杯",
    "発", "遍", "篇", "匹", "本", "時", "畳", "帖", "条", "課", "日", "回", "ヵ月", "階",
    "軒", "機", "個", "脚", "間", "枚", "巻", "名", "年", "人", "列", "輪", "輌", "才",
    "歳", "棹", "冊", "隻", "章", "首", "足", "艘", "反", "滴", "点", "頭", "つ", "通",
    "対", "羽", "把", "割", "膳", "時間", "週間", "人中", "番目", "期目", "巻目"
  ];

  const numbers = ["1", "三", "十一"];

  for (const ctr of counters) {
    for (const n of numbers) {
      // Skip "十一つ" - not valid
      if (ctr === "つ" && n === "十一") continue;

      const s = `${n}${ctr}`;
      test(`segments "${s}" as single unit`, async () => {
        const texts = extractTexts(await simpleSegment(s));
        expect(texts).toEqual([s]);
      });
    }
  }
});