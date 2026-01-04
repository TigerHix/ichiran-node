import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とおり.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ように・ような (JLPT4) - "like, similar to" (less emphatic)
  // This is a different grammar point with similar meaning but different nuance
  '言われたようにした。',
  'レシピのように作った。',

  // まま(に) (JLPT1) - "as one pleases, spontaneous"
  // Different grammar: emphasizes leaving things as they are
  '思うままに話す。',
  '言われるがままにやった。',

  // Noun + とおり without の (wrong construction)
  // When using とおり form with nouns, must use の or special voiced form
  // Note: Some nouns allow direct attachment (契約どおり), so not all are false positives

  // とおる (verb "to pass through") - different word
  'この道を通る。',

  // に particle used incorrectly (should be part of the grammar pattern)
  // Note: Many valid examples use に, so this is context-dependent
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
