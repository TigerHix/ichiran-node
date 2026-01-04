import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てある.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - these should NOT match てある
const negatives = [
  // ている (progressive/resultative) - different auxiliary verb
  // ている can be used with both transitive AND intransitive verbs
  // while てある is ONLY for transitive verbs
  'ドアが開いている。',
  '彼が本を読んでいる。',
  '電気がついている。',
  '窓が閉まっている。',
  '猫が寝ている。',

  // Intransitive verbs + てある - INVALID (てある only works with transitive verbs)
  // We can't directly test transitivity, but these examples use intransitive verbs
  // and should not match because the semantic meaning is wrong
  '行動が続いてある。',  // 続く is intransitive - should be 続いている
  '問題が起きてある。',  // 起きる is intransitive - should be 起きている

  // ておく (do something in advance) - different grammar
  // Focuses on the action/preparation, not the resulting state
  'ドアを開けておく。',
  '晩ご飯を作っておく。',

  // てしまう (completely/unfortunately) - different grammar
  '食べてしまう。',
  '忘れてしまう。',

  // っぱなし (leave something as is) - different grammar
  'ドアを開けっぱなしにする。',
  'テレビをつけっぱなしにする。',

  // Standalone ある (exist/inanimate) - not te-form + aru grammar
  '本がある。',
  '時間がある。',

  // Simple te-form without auxiliary
  'ドアを開けて。',
  '本を読んで。'
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
