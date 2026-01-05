import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つつ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the つつ grammar rule
const negatives = [
  // ながら (nagara) - "while doing" (more casual, can be used with physical actions)
  // This is the similar but more common grammar for simultaneous actions
  '走りながら携帯でゲームをしていたら、電柱にぶつかった。',
  '音楽を聴きながら勉強する。',
  'テレビを見ながら食事をする。',
  '歩きながら話す。',
  '泣きながら謝った。',
  '笑いながら話していた。',

  // ながらも (nagara mo) - "even though, despite" (concessive)
  // Different grammar point with concessive meaning
  '古いながらも中は綺麗だ。',
  '狭いながらも楽しい家だ。',
  '貧しいながらも幸せに暮らしている。',

  // あいだ (aida) / あいだに (aidani) - "during, while" (time expression)
  '寝ているあいだに泥棒が入った。',
  '勉強しているあいだ、静かにして。',

  // てから (tekara) - "after doing" (sequential, not simultaneous)
  '食事をしてから出かけた。',
  '帰宅してからシャワーを浴びた。',

  // たまま (tamama) - "in a state of" (resultative state, not simultaneous action)
  '靴を履いたまま部屋に入った。',
  'メガネをかけたまま寝た。',

  // ては (tewa) - "doing and then" (repeated action or negative consequence)
  '食べては飲んで、遊んで暮らす。',
  'そんなに飲んではいけない。',

  // て (te) - gerund/connective form (not specifically simultaneous)
  '起きて顔を洗う。',
  '本を読んで勉強する。',

  // たり-たり (tari-tari) - "doing things like" (examples, not simultaneous)
  '映画を見たり、音楽を聞いたりする。',

  // Set expressions where つつ doesn't apply
  // 生まれながら (umarenagara) - "from birth" (fixed expression with ながら)
  '生まれながらの音楽家だ。',
  '生まれながらの天才。',

  // つつ used as counter (rare but exists)
  '二つつ三つつ数える。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
