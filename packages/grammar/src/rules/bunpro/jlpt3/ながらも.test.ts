import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ながらも.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple ながら (while doing action) - not ながらも (contrast)
  // While eating, watching TV - action verbs, not state
  '食事をしながらテレビを見る。',
  '歩きながら話す。',

  // にしては (considering/for) - different grammar
  '彼にしては珍しく早く来た。',

  // くせに (despite, with blame) - different nuance
  '知っているくせに教えてくれない。',

  // のに (although) - more general contrast
  '雨が降っているのに出かけた。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
