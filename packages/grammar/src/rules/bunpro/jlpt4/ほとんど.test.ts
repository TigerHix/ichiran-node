import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ほとんど.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // だいたい (daitai) - generally/approximately (different word)
  'だいたいの人は知っている。',
  'だいたい５時に起きています。',

  // たいてい (taitei) - usually (different word)
  'たいてい家で食べています。',

  // ぜんぜん (zenzen) - not at all / completely (different word)
  'ぜんぜんわからない。',
  'ぜんぜん食べない。',

  // あまり (amari) - not very / not much (different word)
  'あまり食べない。',
  'あまり好きじゃない。',

  // おおよそ (oyoyoso) - approximately (more formal)
  'おおよそ３時間かかる。',

  // めったに (mettani) - rarely (requires negative)
  'めったに行かない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
