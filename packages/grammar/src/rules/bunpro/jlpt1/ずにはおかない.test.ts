import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずにはおかない.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Similar patterns that are NOT ずにはおかない:
  // ずに (without the はおかない part)
  '彼は一言も発さずに部屋を出て行った。',
  '彼女は何も言わずに泣いていた。',

  // ないでは (but different continuation)
  'そんなことはしないではいられない。',
  '彼を怒らせないではすまない。',

  // には + different verbs (not おかない)
  'この本は読んでみなければ分からないことはない。',
  '彼に聞かないでは分からない。',

  // ずには + different auxiliary
  '彼女は感動せずにはいられなかった。',
  'この本は読まずにはいられない。',

  // おかない but without the prefix pattern
  '部屋を片付けないでおいた。',

  // Separate uses of particles
  '彼には言わないでおこう。',
  '私は東京には行かない。',

  // Verb + ず + different continuation
  '彼は何も言わずに立ち去った。',

  // ないでは but different structure
  'やらないではいられない気分だ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
