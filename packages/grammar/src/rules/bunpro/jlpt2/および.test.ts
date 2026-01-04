import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './および.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Verb 及ぶ (oyobu - to reach/extend) - different grammar
  // Note: These sentences use ONLY the verb, not the conjunction および
  '影響が全国に及ぶ。',
  '被害が数千人に及ぶ。',
  'その話は私には及ばない。',
  '混乱が極みに及ぶ。',
  '迷惑が及ぶ。',

  // Casual "and" using と - different register
  'りんごとバナナを買う。',
  '東京と大阪に行く。',

  // や (ya) - partial listing "and things like that"
  'りんごやバナナを買う。',
  '本や雑誌を読む。',

  // そして (soshite) - connective "and then"
  '本を読んで、そして寝た。',
  '雨が降り、そして風が吹いた。',

  // その他 (sonohoka) - "etc" or "and other things"
  '本その他の荷物',
  'その他の問題',

  // ならびに (narabini) - another formal "and" (different grammar)
  '東京ならびに大阪', // Different particle
  '氏名ならびに住所', // Different particle
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
