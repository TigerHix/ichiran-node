import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-する.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match (false positive tests)
const negatives = [
  // Regular suru-verbs without honorific prefix
  '勉強する',
  '連絡する',
  '電話します',   // NOUN compound without humble prefix (should NOT match)
  '確認します',   // NOUN compound without humble prefix (should NOT match)
  '散歩しました', // NOUN compound without humble prefix (should NOT match)
  '食事します',   // NOUN compound without humble prefix (should NOT match)

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

  // Regular お/ご + suru-verb but NOT humble context
  // お/ご + compound where the compound is the main activity, not a humble action
  'お電話しますか？',  // Question about calling (not humble "I will call")
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
