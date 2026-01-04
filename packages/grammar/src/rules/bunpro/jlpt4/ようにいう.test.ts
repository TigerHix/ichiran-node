import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようにいう.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match ようにいう
const negatives = [
  // Plain verb + 言う (without ように) - direct quotation or simple "say"
  '言うことは簡単だ。',  // Saying is easy (noun phrase, not ようにいう)
  '彼が言うことを聞け。',  // Listen to what he says (direct object, not ようにいう)

  // Noun + のように (similarity/manner = "like X", not indirect speech)
  '子供のように遊ぶ。',  // Play like a child (manner, not speech)
  '彼のように日本語を話す。',  // Speak Japanese like him (similarity)
  '夢のように美しい。',  // Beautiful like a dream (similarity)

  // ように without speech verb (purpose/aim "so that", not "tell to")
  '風が入るように窓を開けた。',  // Opened window so wind would enter (purpose, not speech)
  '間に合うように早く行こう。',  // Let's go early so we can make it (purpose, not speech)
  'わかるように説明してください。',  // Please explain so I understand (purpose, not speech)

  // Other related grammar points
  'ようにする',  // try to / make sure to (different grammar)
  'ようになる',  // comes to be that (change of state)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
