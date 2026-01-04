import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './聞こえる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 聞く (kiku - to listen/hear actively) - transitive, different verb
  '音楽を聞く。',
  'ラジオを聞いている。',
  '先生の話を聞きました。',
  '彼女はCDを聞くのが好きです。',

  // 聞ける (kikeru - potential form of 聞く, "able to listen")
  'このラジオはCDが聞けます。',
  '私はラジオを聞けます。',

  // Other verbs that look similar but are different
  '消える',  // kieru - to disappear
  '見える',  // mieru - to be visible

  // Sentences with 聞こえる but in forms we shouldn't match (if any)
  // (None - all forms of 聞こえる should match)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
