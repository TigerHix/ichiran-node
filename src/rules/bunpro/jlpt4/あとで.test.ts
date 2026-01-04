import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あとで.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // あと without で (just "after" as a noun, not the grammar pattern)
  '食事のあとに電話します。',
  '彼のあとに続いて走った。',
  '三時間のあとに雨が止んだ。',
  // 後 as "behind" (spatial, not temporal)
  '私の後ろに誰かいる。',
  '後ろのドアから入ってください。',
  // あとが (different grammar - "the rest/remainder")
  '全部食べて、あとが少ししか残っていない。',
  '仕事のあとが大変だ。',
  // Noun + compound 後 (not temporal "after")
  '午後の授業',  // gogo = "afternoon" (different word)
  // で without proper case marking (different usage)
  '家で勉強する。',  // de = locative "at", not temporal "after"
  '鉛筆で書く。',    // de = instrumental "with", not temporal "after"
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
