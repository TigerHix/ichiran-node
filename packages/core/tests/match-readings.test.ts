// Match Readings tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { matchReadings } from '@ichiran/core';
import { setupTests } from '@ichiran/testing';

setupTests();

describe('Match Readings Tests', () => {
  test('取次店 (とりつぎてん)', async () => {
    const result = await matchReadings("取次店", "とりつぎてん");
    expect(result).toEqual([
      ["取", "とり", "ja_kun", null],
      ["次", "つぎ", "ja_kun", null],
      ["店", "てん", "ja_on", null]
    ]);
  });

  test('助っ人 (すけっと)', async () => {
    const result = await matchReadings("助っ人", "すけっと");
    expect(result).toEqual([
      ["助", "すけ", "ja_kun", null],
      "っ",
      ["人", "と", "ja_kun", null]
    ]);
  });

  test('頑張って (がんばって)', async () => {
    const result = await matchReadings("頑張って", "がんばって");
    expect(result).toEqual([
      ["頑", "がん", "ja_on", null],
      ["張", "ば", "ja_kun", null],
      "って"
    ]);
  });

  test('肝心 (かんじん) - rendaku', async () => {
    const result = await matchReadings("肝心", "かんじん");
    expect(result).toEqual([
      ["肝", "かん", "ja_on", null],
      ["心", "じん", "ja_on", "rendaku", null]
    ]);
  });

  test('決心 (けっしん) - gemination', async () => {
    const result = await matchReadings("決心", "けっしん");
    expect(result).toEqual([
      ["決", "けっ", "ja_on", null, "つ"],
      ["心", "しん", "ja_on", null]
    ]);
  });

  test('行方 (ゆくえ) - irregular', async () => {
    const result = await matchReadings("行方", "ゆくえ");
    expect(result).toEqual([
      ["行", "ゆ", "ja_kun", null],
      ["方", "くえ", "irr"]
    ]);
  });

  test('時計 (とけい) - irregular', async () => {
    const result = await matchReadings("時計", "とけい");
    expect(result).toEqual([
      ["時", "と", "irr"],
      ["計", "けい", "ja_on", null]
    ]);
  });

  test('指図 (さしず)', async () => {
    const result = await matchReadings("指図", "さしず");
    expect(result).toEqual([
      ["指", "さし", "irr"],
      ["図", "ず", "ja_on", null]
    ]);
  });

  test('竹刀 (しない) - irregular', async () => {
    const result = await matchReadings("竹刀", "しない");
    expect(result).toEqual([
      ["竹", "しな", "irr"],
      ["刀", "い", "irr"]
    ]);
  });

  test('小売店 (こうりてん)', async () => {
    const result = await matchReadings("小売店", "こうりてん");
    expect(result).toEqual([
      ["小", "こ", "ja_kun", null],
      ["売", "うり", "irr"],
      ["店", "てん", "ja_on", null]
    ]);
  });

  test('売上げ (うりあげ)', async () => {
    const result = await matchReadings("売上げ", "うりあげ");
    expect(result).toEqual([
      ["売", "うり", "irr"],
      ["上", "あ", "ja_kun", null],
      "げ"
    ]);
  });

  test('北方 (ほっぽう) - rendaku with gemination', async () => {
    const result = await matchReadings("北方", "ほっぽう");
    expect(result).toEqual([
      ["北", "ほっ", "ja_on", null, "く"],
      ["方", "ぽう", "ja_on", "rendaku", null]
    ]);
  });

  test('明後日 (あさって) - multiple irregular readings', async () => {
    const result = await matchReadings("明後日", "あさって");
    expect(result).toEqual([
      ["明", "あ", "ja_kun", null],
      ["後", "さっ", "irr"],
      ["日", "て", "irr"]
    ]);
  });
});