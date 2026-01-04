import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でできる-からできる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - sentences that should NOT match this rule
const negatives = [
  // Regular potential できる (can do) without material/source particle
  // "I can do it"
  'できる。',
  'できます。',
  // Instrumental で (doing something WITH something) - verb is NOT できる
  // "Write with a pencil" - instrumental use of で, but not about "being made of"
  '鉛筆で書く。',
  // Locative で (doing something AT a place)
  // "Work at home" - locative use of で
  '家で働く。',
  // から as source (from somewhere, not made from)
  // "Come from Tokyo" - source, not material
  '東京から来る。',
  // Potential verb with が particle (not material marker)
  // "Can speak Japanese" - regular potential with が
  '日本語ができる。',
  '日本語ができます。',
  // Verb + で + different verb (not できる)
  // "Eat with chopsticks"
  '箸で食べる。',
  // Source/origin から without できる
  // "From morning to night"
  '朝から夜まで働く。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
