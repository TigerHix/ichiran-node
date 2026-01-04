import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てくれない-てもらえない.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: sentences that should NOT match てくれない-てもらえない
const negatives = [
  // て form without くれない/もらえない (different grammar)
  '本を読んでいる。',
  'ご飯を作って食べた。',

  // てくれる (positive form, different grammar - someone did something for you)
  '彼が私を手伝ってくれた。',
  '友達が貸してくれた。',
  '母が弁当を作ってくれた。',

  // てあげる (different grammar - I do something for someone)
  '私は彼に本を買ってあげた。',
  '手伝ってあげましょう。',

  // てもらう (positive form, different grammar - had someone do something)
  '彼に英語を教えてもらった。',
  '兄に手伝ってもらった。',
  '友達に借りてもらった。',

  // てください (polite request, different grammar)
  '見てください。',
  '買ってください。',
  '座ってください。',

  // ていただけませんか (more polite request, different grammar)
  '見ていただけませんか。',
  'やっていただけませんか。',

  // Simple negative verb without request meaning
  '彼は来ない。',
  '行かないでください。',

  // くれる without te-form
  '彼がくれる。',
  '彼女が何かくれるでしょう。',

  // くれてありがとう (gratitude, not request)
  '手伝ってくれてありがとう。',
  '作ってくれてありがとう。',

  // Negative verb + で + something else
  '行かないで帰った。', // went home without going
  '食べないで寝た。', // slept without eating
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
