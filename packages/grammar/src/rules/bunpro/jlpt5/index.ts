import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
import iAdjectives from './い-adjectives.js';
import ndesu from './-んです-のです.js';
import tsumorida from './つもりだ.js';
import kuru from './くる.js';
import ageru from './あげる.js';
import shite from './して.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [adjectivete, iAdjectives, ndesu, tsumorida, kuru, ageru, shite],
};
