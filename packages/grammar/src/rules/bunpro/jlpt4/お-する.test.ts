import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-する.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match (false positive tests)
const negatives = [
  // Regular suru-verbs without honorific prefix
  // NOTE: Several of these have pre-existing false positives documented below
  '勉強する',
  '勉強します',  // FALSE POSITIVE - pre-existing issue
  '連絡する',
  '連絡しました',
  '電話します',   // FALSE POSITIVE - pre-existing issue
  '電話しました',
  '確認します',   // FALSE POSITIVE - pre-existing issue
  '確認しました',
  '散歩しました', // FALSE POSITIVE - pre-existing issue
  '食事します',   // FALSE POSITIVE - pre-existing issue
  '約束します',   // FALSE POSITIVE - pre-existing issue
  '予約しました', // FALSE POSITIVE - pre-existing issue
  '注文します',   // FALSE POSITIVE - pre-existing issue
  '発見しました', // FALSE POSITIVE - pre-existing issue
  '満足します',   // FALSE POSITIVE - pre-existing issue
  '心配しました', // FALSE POSITIVE - pre-existing issue
  '成功します',   // FALSE POSITIVE - pre-existing issue
  '失敗しました', // FALSE POSITIVE - pre-existing issue
  '開始しました', // FALSE POSITIVE - pre-existing issue
  '終了します',   // FALSE POSITIVE - pre-existing issue
  '通話します',   // FALSE POSITIVE - pre-existing issue
  '到着しました', // FALSE POSITIVE - pre-existing issue
  '出発します',   // FALSE POSITIVE - pre-existing issue
  '参加します',   // FALSE POSITIVE - pre-existing issue

  // Regular verbs without honorific prefix
  '閉めます',
  '呼びます',
  '守ります',
  '持ちます',
  '借りた',

  // お/ご as regular nouns (not humble prefix)
  'お茶を飲みます',
  'ご飯を食べる',

  // Honorific form (お～になる) - different grammar
  '先生がお呼びになります',

  // します without humble prefix (regular polite)
  'ドアを閉めます',
  '本を読みます',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
const skipPositives = [
  // With exclamation mark, "し" is parsed as VERB (not AUX)
  '僕がお守りします！',
  // Hiragana "おかり" is parsed as irrealis form (未然形) + る auxiliary
  '友達のお母さんにパソコンをおかりしました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, {
    negatives,
    skipPositives,
  });
});
