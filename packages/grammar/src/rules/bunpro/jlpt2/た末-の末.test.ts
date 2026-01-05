import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './た末-の末.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar "after" expressions that are NOT た末・の末
  // あと - "after" (general temporal marker, lacks the emphasis on completion)
  '映画を見たあと、食事をしました。',
  '仕事のあとで、飲みに行きましょう。',

  // あげく - "after" with negative connotation (different nuance)
  'さんざん泣いたあげく、彼女は眠ってしまった。',
  '口論のあげく、喧嘩になってしまい、警察のお世話になった。',

  // うえで - "after doing" (focuses on process/sequence)
  '詳しく調べたうえで、決めます。',
  'よく相談したうえで、返事します。',

  // てから - "after doing" (simple sequence)
  'ご飯を食べてから、勉強します。',
  '家に帰ってから、シャワーを浴びました。',

  // 以来 - "since" (temporal marker, different meaning)
  '卒業以来、彼に会っていない。',
  'あの日以来、ずっと待っています。',

  // 後 - "after" (simple suffix, different emphasis)
  '数年後、彼は成功した。',
  '戦後、日本は急速に復興した。',

  // Sentences with 末 but in different contexts
  // e.g., 末 as "end" in spatial sense (月末, 期末)
  '月末までにレポートを出してください。',
  '期末テストが終わりました。',

  // 末 as "tip" or "extremity" (枝末, 毛末)
  '枝末なこと気にしないで。',
  '細部に至るまで注意を払う。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
