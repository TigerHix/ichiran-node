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
import da from './だ.js';
import desu from './です.js';
import wa from './は.js';
import wo from './を.js';
import de from './で.js';
import ni from './に.js';
import he from './へ.js';
import to from './と.js';
import yo from './よ.js';
import ne from './ね.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [
    adjectivete,
    iAdjectives,
    ndesu,
    tsumorida,
    ka,
    karaBecause,
    ga,
    gairu,
    kuru,
    shite,
    tekara,
    da,
    desu,
    wa,
    wo,
    de,
    ni,
    he,
    to,
    yo,
    ne,
  ],
};
