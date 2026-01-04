import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // しか～ない with noun (different grammar - "only X", not "have no choice")
  '１００円しかない。',
  'バナナしか食べなかった。',
  'それしか知りません。',
  // Regular negative verbs without しか
  '行かない。',
  '食べない。',
  'しない。',
  // しか alone without negative
  'これしか。',
  // し verb (different meaning)
  '仕事をする。',
  '勉強をする。',
  // か as question particle
  '行くか？',
  '食べるか？',
  // ない as standalone negative
  'ないです。',
  'ありません。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
