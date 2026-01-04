import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おまけに.js';
import { BUNPRO_JLPT2 } from './index.js';

const positives = [
  // Examples from test data
  '今日は仕事に遅刻して部長に怒られたし、おまけに取引先の人も怒らせちゃったから、今日は最悪の日だったよ。',
  '昨日は彼氏に美味しいご飯をご馳走してもらって、おまけにプレゼントまでもらった。',
  'パソコンが全然立ち上がらない。おまけにスマホの充電がないから仕事が全然できない。',
  '家を掃除した。おまけにランチを作っといた。',
  '宝石を買ったら、おまけに香水をサービスしてくれた。',
  '競馬で負け、おまけに帰りに財布を落とし、最悪だ。',
  '自転車が盗まれただけではなく、おまけに雨が降りそうだ。',
];

const negatives = [
  // Similar conjunctions that should NOT match
  'そのうえ、彼も来る。',
  'それに、私も行く。',
  'さらに、詳しく説明します。',
  'なお、明日の午後にお知らせします。',
  'にくわえて、雨も降ってきた。',
  'また、来週も会議があります。',
  'うえに、彼女は優秀だ。',
  'かたわら、勉強もしている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { positives, negatives });
});
