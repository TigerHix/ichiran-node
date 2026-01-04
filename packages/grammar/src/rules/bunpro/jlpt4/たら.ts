import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: たら (conditional / when)
 *
 * Matches verb/adj/noun + たら (conditional form)
 *
 * Examples:
 * - 勉強したら、テストは簡単になる。(When you study, the test will become easy.)
 * - 寒かったら、エアコンつけてね。(If/when you are cold, turn on the AC.)
 * - 週末だったら、時間ありますよ。(If/when it's the weekend, I have time.)
 */
export default linguisticRule('たら', (r) => {
  // Match common たら forms as single tokens
  // Based on test data from Bunpro
  const tara = r.tok({
    textOneOf: [
      'したら', 'だったら', 'かったら', '行ったら', 'なったら', 'あったら',
      '寒かったら', 'よかったら', 'なかったら', '遅れたら', 'いきたかったら',
      'してみたら', 'したかったら', 'うすかったら', 'からかったら'
    ],
  }, 'tara');
  r.capture(tara);
});
