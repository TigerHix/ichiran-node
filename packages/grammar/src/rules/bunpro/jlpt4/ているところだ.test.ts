import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ているところだ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // たところだ (just finished - different grammar)
  '今帰ったところです。',
  '仕事は今終わったところ。',

  // ところだ (about to do - different grammar)
  '今から行くところです。',
  '食べるところです。',

  // Simple ている (without ところ - different grammar)
  '今、映画を見ている。',
  'ご飯を食べている。',

  // ているあいだに (while doing - different grammar)
  '勉強しているあいだに電話がかかってきた。',
  '寝ているあいだに地震があった。',

  // ている最中に (in the middle of - different but similar grammar)
  '食べている最中に電話が鳴った。',

  // ところ as physical place (not grammatical "situation")
  'あの場所に行ったことがある。',
  'ここは良い場所だ。',

  // Verb + て form without いる (te-form connection - different grammar)
  '本を読んで寝る。',
  'パンを食べて学校に行く。',

  // てしまう (completion - different grammar)
  '食べてしまった。',
  '忘れてしまった。',

  // てある (transitive resultative - different grammar)
  '黑板に字が書いてある。',
  'ポスターが貼ってある。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
