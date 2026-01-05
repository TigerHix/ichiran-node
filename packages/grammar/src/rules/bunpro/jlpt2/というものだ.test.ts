import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というものだ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the というものだ grammar rule
const negatives = [
  // という alone (quoting particle without ものだ)
  '彼は来るという.',
  'これは山田という人です.',
  '東京という都市は大きい.',

  // ものだ alone (general truths, without という)
  '水は低いところに流れるものだ.',
  '若者は未来を担うものだ.',

  // というわけだ (conclusion/reasoning, different grammar)
  '彼が来ないというわけだ.',
  'つまり、彼は知らなかったというわけだ.',

  // というはずだ (expectation, different grammar)
  '彼は来るというはずだ.',
  '成功するというはずだった.',

  // というそうだ (hearsay, different grammar)
  '彼は来るというそうだ.',
  '明日は雨が降るというそうだ.',

  // というらしいだ (incorrect form - should be というらしい)
  // Note: This is grammatically incorrect Japanese

  // Simple copula だ (without というもの)
  'これは本だ.',
  '彼は学生だ.',
  '今日は良い天気だ.',

  // もの as object (physical thing, not abstract concept)
  'これはとても重いものだ.',
  'これは大切なものです.',
  'そのものは美しい.',

  // という + noun + で (quoting + instrumental で)
  'これはペンというもので書く.',
  'バスというもので行く.',

  // という + noun + に (quoting + locative に)
  '京都という所に行きたい.',
  '寿司というものに興味がある.',

  // て + ものだ (te-form + ものだ, different meaning)
  '食べてものだ.',
  '読んでものだ.',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
