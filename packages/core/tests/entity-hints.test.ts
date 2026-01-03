// Entity hint tests - testing Named Entity Recognition hints for segmentation
import { describe, test, expect } from 'bun:test';
import { dictSegment, wordInfoGlossJson } from '@ichiran/core';
import { setupTests } from '@ichiran/testing';

setupTests();

// Helper to get all segment texts
const getTexts = (wordInfos: any[]) => wordInfos.map(wi => wi.text);
const joinTexts = (wordInfos: any[]) => wordInfos.map(wi => wi.text).join('');

describe('Entity Hints - Basic Functionality', () => {
  test('entity hint creates synthetic segment for unknown name', async () => {
    const text = '田中太郎です';
    const entities = [{ start: 0, end: 4, boost: 50 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos.length).toBeGreaterThanOrEqual(2);
    expect(wordInfos[0].text).toBe('田中太郎');
    expect(wordInfos[0].isEntity).toBe(true);
    
    const json = await wordInfoGlossJson(wordInfos[0]);
    expect(json.gloss?.[0].pos).toBe('[n-pr]');
  });
  
  test('entity hint boosts existing dictionary entries', async () => {
    const text = '東京に行く';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('entity without boost uses default boost (50)', async () => {
    const text = '田中太郎';
    const entities = [{ start: 0, end: 4 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('田中太郎');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('synthetic entity has no seq (dictionary ID)', async () => {
    const text = '田中太郎';
    const entities = [{ start: 0, end: 4 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const json = await wordInfoGlossJson(wordInfos[0]);
    
    expect(json.seq).toBeUndefined();
    expect(json.gloss?.[0].pos).toBe('[n-pr]');
  });
  
  test('no entities parameter works normally', async () => {
    const text = '東京に行く';
    
    const results = await dictSegment(text, { limit: 1 });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京');
    expect(wordInfos[0].isEntity).toBeUndefined();
  });
  
  test('empty entities array works normally', async () => {
    const text = '東京に行く';
    const entities: any[] = [];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京');
  });
});

describe('Entity Hints - Multiple Entities', () => {
  test('two entities with high boost both get matched', async () => {
    const text = '田中太郎と山田花子';
    const entities = [
      { start: 0, end: 4, boost: 200 },
      { start: 5, end: 9, boost: 200 }
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(2);
    expect(entitySegments[0].text).toBe('田中太郎');
    expect(entitySegments[1].text).toBe('山田花子');
  });
  
  test('three entities in sentence', async () => {
    const text = '田中太郎と山田花子が東京に行く';
    const entities = [
      { start: 0, end: 4, boost: 200 },   // 田中太郎
      { start: 5, end: 9, boost: 200 },   // 山田花子
      { start: 10, end: 12, boost: 200 }  // 東京
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(3);
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('adjacent entities (no gap)', async () => {
    const text = '田中太郎山田花子';
    const entities = [
      { start: 0, end: 4, boost: 200 },
      { start: 4, end: 8, boost: 200 }
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos.length).toBe(2);
    expect(wordInfos[0].text).toBe('田中太郎');
    expect(wordInfos[1].text).toBe('山田花子');
  });
  
  test('entities at start, middle, and end', async () => {
    const text = '田中は東京で山田に会う';
    const entities = [
      { start: 0, end: 2, boost: 150 },   // 田中
      { start: 3, end: 5, boost: 150 },   // 東京
      { start: 6, end: 8, boost: 150 }    // 山田
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(3);
    expect(joinTexts(wordInfos)).toBe(text);
  });
});

describe('Entity Hints - Boost Values', () => {
  test('high boost (500) wins over any dictionary entry', async () => {
    const text = '日本語を勉強する';
    const entities = [{ start: 0, end: 3, boost: 500 }]; // 日本語 as entity
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('日本語');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('low boost (10) may lose to dictionary', async () => {
    // 山田花子 with low boost - 山田 is in dictionary
    const text = '山田花子';
    const entities = [{ start: 0, end: 4, boost: 10 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    // Either way, full text should be preserved
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('boost 0 still creates entity segment', async () => {
    const text = '田中太郎';
    const entities = [{ start: 0, end: 4, boost: 0 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    // With 0 boost, entity may or may not win, but text preserved
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('very high boost (1000) for compound word', async () => {
    const text = '東京大学医学部';
    const entities = [{ start: 0, end: 7, boost: 1000 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京大学医学部');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('different boost values for different entities', async () => {
    const text = '田中と山田';
    const entities = [
      { start: 0, end: 2, boost: 100 },
      { start: 3, end: 5, boost: 200 }
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    expect(entitySegments.length).toBe(2);
  });
});

describe('Entity Hints - Japanese Names', () => {
  test('common surname only', async () => {
    const text = '田中さん';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('田中');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('full name with surname + given name', async () => {
    const text = '鈴木一郎です';
    const entities = [{ start: 0, end: 4, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('鈴木一郎');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('given name only', async () => {
    const text = '太郎が来た';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('太郎');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('name with honorific suffix', async () => {
    const text = '田中太郎様';
    const entities = [{ start: 0, end: 4, boost: 100 }]; // Just the name
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('田中太郎');
    expect(wordInfos[0].isEntity).toBe(true);
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('unusual name characters', async () => {
    const text = '龍之介が書いた';
    const entities = [{ start: 0, end: 3, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('龍之介');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('name with の particle inside', async () => {
    const text = '井上の介です';
    const entities = [{ start: 0, end: 4, boost: 150 }]; // 井上の介
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('井上の介');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Katakana Names', () => {
  test('foreign name in katakana', async () => {
    const text = 'マイケルが来た';
    const entities = [{ start: 0, end: 4, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('マイケル');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('full foreign name', async () => {
    const text = 'マイケル・ジャクソン';
    const entities = [{ start: 0, end: 10, boost: 200 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('マイケル・ジャクソン');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('japanese name in katakana', async () => {
    const text = 'タナカタロウです';
    // タナカタロウ = 6 chars (positions 0-5), end = 6
    const entities = [{ start: 0, end: 6, boost: 300 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('タナカタロウ');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('company name in katakana', async () => {
    const text = 'ソニーで働く';
    const entities = [{ start: 0, end: 3, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('ソニー');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Place Names', () => {
  test('city name', async () => {
    const text = '大阪に行く';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('大阪');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('compound place name', async () => {
    const text = '東京都渋谷区';
    const entities = [{ start: 0, end: 6, boost: 500 }]; // High boost for compound
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京都渋谷区');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('station name', async () => {
    const text = '新宿駅で待つ';
    const entities = [{ start: 0, end: 3, boost: 150 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('新宿駅');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('foreign place in katakana', async () => {
    const text = 'パリに住む';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('パリ');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('country name', async () => {
    const text = 'アメリカから来た';
    const entities = [{ start: 0, end: 4, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('アメリカ');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Organization Names', () => {
  test('company with 株式会社', async () => {
    const text = '株式会社山田で働く';
    // 株式会社山田 = 6 chars (positions 0-5), end = 6 - needs very high boost
    const entities = [{ start: 0, end: 6, boost: 800 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('株式会社山田');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('university name', async () => {
    const text = '東京大学に入る';
    const entities = [{ start: 0, end: 4, boost: 150 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京大学');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('government organization', async () => {
    const text = '外務省が発表した';
    const entities = [{ start: 0, end: 3, boost: 150 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('外務省');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Product and Brand Names', () => {
  test('product name in katakana', async () => {
    const text = 'アイフォンを買う';
    const entities = [{ start: 0, end: 5, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('アイフォン');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('game title', async () => {
    const text = 'ポケモンをする';
    const entities = [{ start: 0, end: 4, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('ポケモン');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Edge Cases', () => {
  test('single character entity', async () => {
    const text = '李さん';
    const entities = [{ start: 0, end: 1, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('李');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('entity at very end of sentence', async () => {
    const text = 'これは田中';
    const entities = [{ start: 3, end: 5, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    const lastWord = wordInfos[wordInfos.length - 1];
    expect(lastWord.text).toBe('田中');
    expect(lastWord.isEntity).toBe(true);
  });
  
  test('entity is entire text', async () => {
    const text = '田中太郎';
    const entities = [{ start: 0, end: 4, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos.length).toBe(1);
    expect(wordInfos[0].text).toBe('田中太郎');
  });
  
  test('very long entity name', async () => {
    const text = '東京都千代田区永田町一丁目';
    // 13 chars total (positions 0-12), end = 13
    const entities = [{ start: 0, end: 13, boost: 3000 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('東京都千代田区永田町一丁目');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('entity followed by particle', async () => {
    const text = '田中が走る';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('田中');
    expect(wordInfos[0].isEntity).toBe(true);
    expect(wordInfos[1].text).toBe('が');
  });
  
  test('entity after particle', async () => {
    const text = 'と田中が';
    const entities = [{ start: 1, end: 3, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[1].text).toBe('田中');
    expect(wordInfos[1].isEntity).toBe(true);
  });
  
  test('hiragana name entity', async () => {
    const text = 'さくらが咲く';
    const entities = [{ start: 0, end: 3, boost: 150 }]; // さくら as name
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('さくら');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});

describe('Entity Hints - Complex Sentences', () => {
  test('person name, action, and place', async () => {
    const text = '田中太郎は東京で働いている';
    const entities = [
      { start: 0, end: 4, boost: 200 },   // 田中太郎
      { start: 5, end: 7, boost: 200 }    // 東京
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(2);
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('dialogue with names', async () => {
    const text = '山田「田中さんはどこですか」';
    const entities = [
      { start: 0, end: 2, boost: 150 },   // 山田
      { start: 3, end: 5, boost: 150 }    // 田中
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(2);
  });
  
  test('multiple names with same surname', async () => {
    const text = '田中太郎と田中花子';
    const entities = [
      { start: 0, end: 4, boost: 200 },
      { start: 5, end: 9, boost: 200 }
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(2);
    expect(entitySegments[0].text).toBe('田中太郎');
    expect(entitySegments[1].text).toBe('田中花子');
  });
  
  test('sentence with organization, person, and place', async () => {
    const text = '東京大学の田中教授が京都で講演した';
    const entities = [
      { start: 0, end: 4, boost: 200 },   // 東京大学
      { start: 5, end: 7, boost: 200 },   // 田中
      { start: 10, end: 12, boost: 200 }  // 京都
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const entitySegments = wordInfos.filter((wi: any) => wi.isEntity);
    
    expect(entitySegments.length).toBe(3);
    expect(joinTexts(wordInfos)).toBe(text);
  });
});

describe('Entity Hints - Proper Noun PoS Assignment', () => {
  test('synthetic entity has n-pr gloss', async () => {
    const text = '田中太郎';
    const entities = [{ start: 0, end: 4 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    const json = await wordInfoGlossJson(wordInfos[0]);
    
    expect(json.gloss).toBeDefined();
    expect(json.gloss?.[0].pos).toBe('[n-pr]');
    expect(json.gloss?.[0].gloss).toContain('proper noun');
  });
  
  test('dictionary entry marked as entity has isEntity flag', async () => {
    const text = '東京';
    const entities = [{ start: 0, end: 2, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    // Dictionary entries marked as entities should have isEntity flag
    expect(wordInfos[0].isEntity).toBe(true);
    
    // The gloss should include n-pr somewhere (may be first or prepended)
    const json = await wordInfoGlossJson(wordInfos[0]);
    const hasProperNounGloss = json.gloss?.some((g: any) => g.pos === '[n-pr]');
    expect(hasProperNounGloss).toBe(true);
  });
  
  test('multiple entities all get proper noun marking', async () => {
    const text = '田中と山田';
    const entities = [
      { start: 0, end: 2, boost: 150 },
      { start: 3, end: 5, boost: 150 }
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    for (const wi of wordInfos.filter((w: any) => w.isEntity)) {
      const json = await wordInfoGlossJson(wi);
      expect(json.gloss?.[0].pos).toBe('[n-pr]');
    }
  });
});

describe('Entity Hints - Sentence Integrity', () => {
  test('full sentence text is preserved', async () => {
    const text = '私は田中太郎と申します';
    const entities = [{ start: 2, end: 6, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(joinTexts(wordInfos)).toBe(text);
  });
  
  test('particles around entity preserved', async () => {
    const text = 'の田中が来た';
    const entities = [{ start: 1, end: 3, boost: 100 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(joinTexts(wordInfos)).toBe(text);
    expect(getTexts(wordInfos)).toContain('田中');
    expect(getTexts(wordInfos)).toContain('が');
  });
  
  test('long sentence with entities maintains structure', async () => {
    const text = '昨日田中太郎は東京駅で山田花子に会って一緒に大阪へ行った';
    const entities = [
      { start: 2, end: 6, boost: 200 },   // 田中太郎
      { start: 7, end: 10, boost: 200 },  // 東京駅
      { start: 11, end: 15, boost: 200 }, // 山田花子
      { start: 20, end: 22, boost: 200 }  // 大阪
    ];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(joinTexts(wordInfos)).toBe(text);
  });
});

describe('Entity Hints - Mixed Scripts', () => {
  test('kanji-katakana mixed name', async () => {
    const text = '山田マイケルです';
    const entities = [{ start: 0, end: 6, boost: 150 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('山田マイケル');
    expect(wordInfos[0].isEntity).toBe(true);
  });
  
  test('kanji-hiragana name', async () => {
    const text = '田中さくらです';
    const entities = [{ start: 0, end: 5, boost: 150 }];
    
    const results = await dictSegment(text, { limit: 1, entities });
    const [wordInfos] = results[0];
    
    expect(wordInfos[0].text).toBe('田中さくら');
    expect(wordInfos[0].isEntity).toBe(true);
  });
});
