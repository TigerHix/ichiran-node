import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さすが.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the さすが grammar rule
const negatives = [
  // さす (sasu) - different verb (to point out, to move)
  '指でさすな。',
  '彼の背中をさした。',

  // が (ga) - subject particle alone
  '私が行きます。',
  '彼が好きです。',

  // Similar sounding but different words
  // さすがい (sasugai) - not a real word

  // やっぱり (yappari) / やはり (yahari) - similar meaning but different word
  'やっぱり雨が降ってきた。',
  'やはり彼は来なかった。',

  // はたして (hatashite) - expresses doubt, not affirmation of expectation
  'はたして成功するだろうか。',
  '彼が来るかはたして分からない。',

  // 流石 as individual characters (not the compound word)
  // Unlikely to occur in natural text, but keeping as edge case

  // Noun phrases with さすが but not as the adverb
  // Example: "さすが" as a name or title (very rare)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
