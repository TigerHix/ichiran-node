import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないで.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negation without で (plain negative form)
  // These are statements, not "without doing"
  '行かない。',
  '食べない。',
  'しない。',

  // Negative conjunction なくて (connects clauses with reason/causality)
  // This has a different meaning: "because X didn't happen, Y..."
  // Structurally: text=なく (not text=ない) + て (not で)
  'インターネットが繋がらなくて困ってる。',
  '高くなくて便利です。',
  '行けなくて残念でした。',

  // Positive te-form て (different grammar)
  '行って、買ってきた。',
  '食べて、寝た。',

  // ては (different grammar)
  '食べてはいけない。',
  '行ってはだめ。',

  // Independent ない (negation without で)
  '彼は来ない。',
  'お金がない。',

  // なくて-conjunction (reasons/causes, not "without doing")
  // These use なくて to give a reason for something
  '時間がなくて、行けませんでした。',
  '雨が降らなくて、よかった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
