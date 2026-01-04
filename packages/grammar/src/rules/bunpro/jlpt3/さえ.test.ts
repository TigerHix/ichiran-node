import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さえ.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // さえ〜ば (different grammar - conditional "as long as")
  // This is a separate JLPT3 grammar point
  // 'お金さえあれば幸せだ。',  // Would match さえ〜ば instead

  // Simple で (locative/instrumental) - not でさえ
  '東京で働きます。',
  '鉛筆で書きます。',

  // Simple さえ at end of sentence (noun meaning "alone")
  // This is rare but exists as a different grammatical use

  // ても (even if) - different particle
  '雨でも行きます。',
  '子供でもできる。',

  // も (also) - different particle
  '私も行きます。',
  '子供も知っている。',

  // まで (even/to) - different particle
  '最後まで頑張りました。',
  '子供まで参加した。',

  // こそ (emphasis) - different particle
  'これこそが正解だ。',
  'あなたこそ適任者だ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
