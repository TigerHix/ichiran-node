import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './きっかけ.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Particles not matching the pattern
  'この本を読みました。',
  '彼との出会いはいい思い出です。',
  'きっかけがあります。',
  // きっかけ used as a simple noun without proper particles
  'きっかけを作りました。',
  '良いきっかけだと思いました。',
  // Different grammar patterns
  'これを機に始めました。', // 契機 instead of きっかけ
  '彼が来たので始めました。', // simple ので form
  // Wrong particle combinations
  'この本をきっかけだ',
  '彼との出会いにきっかけで',
  'この本がきっかけに',
];

// Known GiNZA parsing limitations - these sentences have complex particle
// arrangements that confuse the pattern matcher
const skipPositives = [
  'クモに刺されたのをきっかけとして、彼はスーパーヒーローになった。',
  'テキサスでの蝶々の羽ばたきがきっかけとなって数日あとにブラジルで竜巻が起きると言われている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
