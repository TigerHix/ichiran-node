import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あり.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Regular verb ある (existence) - not the literary form あり
  '机の上に本がある。',           // There is a book on the desk
  'お金があるから買える。',        // I have money so I can buy it
  '彼は才能がある。',              // He has talent
  // あった (past tense of ある)
  '昨日は雨があった。',            // There was rain yesterday
  // あって (te-form of ある)
  '名前があって面白い。',          // The names match and it's interesting
  // ある (sentence-initial, not the grammar pattern)
  'ある日、男が来た。',            // One day, a man came
  // ありません (polite negative of ある)
  '部屋には誰もありません。',      // There is no one in the room
  // ありませんでした (polite past negative)
  '昨日は雨がありませんでした。',  // There was no rain yesterday
  // あります (polite form of ある)
  '食堂があります。',              // There is a cafeteria
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
