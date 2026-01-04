import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけで.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // だけでなく (not only...but also - different grammar)
  '日本語だけでなく英語も話せる。',
  '彼は頭がいいだけでなく、性格もいい。',

  // だけ alone (without で)
  'これだけあれば十分です。',
  'それだけ知りたい。',

  // で alone (without だけ)
  '電車で行きます。',
  '日本語で話してください。',

  // だ as copula + で (te-form of copula)
  '彼は学生で、私は会社員です。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
