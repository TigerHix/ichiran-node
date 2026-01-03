import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
import iAdjectives from './い-adjectives.js';
import ndesu from './-んです-のです.js';
import tsumorida from './つもりだ.js';
import ka from './か.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [adjectivete, iAdjectives, ndesu, tsumorida, ka],
};
