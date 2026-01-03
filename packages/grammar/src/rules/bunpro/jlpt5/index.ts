import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
import iAdjectives from './い-adjectives.js';
import ndesu from './-んです-のです.js';
import tsumorida from './つもりだ.js';
import ka from './か.js';
import karaBecause from './から-because.js';
import ga from './が.js';
import gairu from './がいる.js';
import kuru from './くる.js';
import shite from './して.js';
import tekara from './てから.js';
import wo from './を.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [adjectivete, iAdjectives, ndesu, tsumorida, ka, karaBecause, ga, gairu, kuru, shite, tekara, wo],
};
