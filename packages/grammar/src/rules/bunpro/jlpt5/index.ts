import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
import iAdjectives from './い-adjectives.js';
import ndesu from './-んです-のです.js';
import tsumorida from './つもりだ.js';
import ka from './か.js';
import kaOr from './か-or.js';
import karaBecause from './から-because.js';
import kara from './から.js';
import ga from './が.js';
import gaBut from './が-but.js';
import gaAru from './がある.js';
import gairu from './がいる.js';
import kuru from './くる.js';
import shite from './して.js';
import tekara from './てから.js';
import da from './だ.js';
import desu from './です.js';
import janai from './じゃない.js';
import wa from './は.js';
import wo from './を.js';
import de from './で.js';
import deBy from './で-by.js';
import ni from './に.js';
import he from './へ.js';
import to from './と.js';
import yo from './よ.js';
import ne from './ね.js';
import ageru from './あげる.js';
import ii from './いい.js';
import uVerbs from './う-Verbs.js';
import uverbNai from './うverb--ない.js';
import uVerbPast from './う-verb-past.js';
import uVerbNegPast from './う-verb-neg-past.js';
import kirai from './きらい.js';
import kurai1 from './くらい1.js';
import kureru from './くれる.js';
import kekkou from './けっこう.js';
import kedoDakedo from './けど-だけど.js';
import keredomo from './けれども.js';
import koko from './ここ.js';
import kono from './この.js';
import kore from './これ.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [
    adjectivete,
    ageru,
    gaAru,
    gaBut,
    ga,
    gairu,
    ii,
    iAdjectives,
    ka,
    kaOr,
    kara,
    karaBecause,
    kedoDakedo,
    keredomo,
    kekkou,
    kirai,
    koko,
    kono,
    kore,
    kuru,
    kurai1,
    kureru,
    da,
    deBy,
    desu,
    janai,
    de,
    ni,
    he,
    to,
    wo,
    wa,
    yo,
    ne,
    ndesu,
    shite,
    tekara,
    tsumorida,
    uVerbs,
    uVerbNegPast,
    uVerbPast,
    uverbNai,
  ],
};
