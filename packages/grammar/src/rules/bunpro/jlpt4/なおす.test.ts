import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なおす.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 直す or なおす as a standalone main verb (to fix/repair), not as auxiliary
  '壊れた椅子を直す。',
  'テレビを修理して直します。',
  '道を直す。',
  '間違いを直す。',
  'このパソコンを直してください。',
  // 治す (to heal/cure) - different kanji, different meaning
  '病気を治す。',
  '怪我を治す。',
  // Different compound verbs that don't use なおす
  '勉強を続ける。',
  '本を読み終わる。',
  '走り続ける。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
