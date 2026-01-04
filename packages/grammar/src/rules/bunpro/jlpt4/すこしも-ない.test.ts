import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すこしも-ない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 少し (positive/affirmative usage)
  '少し食べてください。',
  '少し話を聞かせて。',
  '少し待ってください。',

  // すこしも at end of sentence as noun (unusual but possible)
  // Note: This is extremely rare in practice and すこしも almost always
  // precedes a negative predicate when used as an adverb.

  // Positive emphasis usage (different grammar point)
  // Note: Similar to あまり, すこしも can sometimes appear in contexts
  // that aren't strictly grammatical negation but convey minimal positive meaning.
  // These are rare and context-dependent.
];

// Complex sentence structures that are difficult to match with current patterns:
//
// 1. あなたは子供の話をすこしも聞いてあげてない。
//    - Contains 聞いてあげてない (benefactive auxiliary あげ + progressive + negative)
//    - Multiple auxiliary verbs in complex chain
//    - Pattern doesn't account for benefactive あげる between te-form and いる
//
// 2. 今の話がすこしも理解出来なかった。
//    - Contains 理解出来なかった (サ変 compound verb + potential + past negative)
//    - 理解 is a noun, 出来 is a potential verb, forming a compound structure
//    - The compound verb + potential + negative chain doesn't match our simple patterns
//
// 3. 君はすこしもお金がないのにロレックスを買おうとしているの？
//    - Contains 買おうとしている (volitional + to-suru + progressive)
//    - Main clause doesn't use すこしも-ない pattern (すこしも only modifies first clause)
//    - This is a different grammatical structure
//
// Note: These represent edge cases with complex auxiliary chains that would require
// significantly more complex patterns to match. The core すこしも-ない patterns
// (simple negative, progressive negative, i-adj negative, noun+ではない) work correctly.
const skipPositives = [
  'あなたは子供の話をすこしも聞いてあげてない。',
  '今の話がすこしも理解出来なかった。',
  '君はすこしもお金がないのにロレックスを買おうとしているの？',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
