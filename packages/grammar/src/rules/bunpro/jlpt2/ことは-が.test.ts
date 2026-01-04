import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことは-が.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // こと alone without the repetition pattern
  '彼が来ることは確かだ。',

  // Simple こと nominalization without contrast
  '日本語を勉強することが大切です。',

  // ということ (different grammar - "means that...")
  '彼が来たということは、予定が変わったのだろう。',

  // かわりに (instead of - different pattern)
  '私が行くかわりに、彼を行かせる。',

  // Simple contrastive は without こと
  '行きたいけど、時間がない。',

  // にしては (considering that - different grammar)
  '彼は新人にしては、よくやる。',

  // ものの (although - different grammar)
  '練習はしたものの、上手くならなかった。',
];

// Known GiNZA parsing limitation: sentences where the second clause
// contains verbs/adjectives that can be mistakenly matched instead of
// the repeated form after ことは
const skipPositives = [
  '締切を延ばせたことは延ばせたが、余裕ないですよ。',  // "ない" in "余裕ない" is matched instead of "延ばせた"
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
