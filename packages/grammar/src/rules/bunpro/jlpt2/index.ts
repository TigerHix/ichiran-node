import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
import kaneru from './かねる.js';
import karashite from './からして.js';
import karasurutokarasureba from './からすると-からすれば.js';
import kotoDakara from './ことだから.js';
import kotoNiNatteiru from './ことになっている.js';
import osoraku from './おそらく.js';
import sasuga from './さすが.js';
import souieba from './そういえば.js';
import souninai from './そうにない.js';
import sonoue from './その上.js';
import soretomo from './それとも.js';
import sorenanoni from './それなのに.js';
import sorenara from './それなら.js';
import sorenishitemo from './それにしても.js';
import tachimachi from './たちまち.js';
import tattano from './たった-の.js';
import tatte from './たって.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [
    tekoso,
    nouchide,
    ageku,
    iwayuru,
    kanaikanouniuchi,
    kaneru,
    karashite,
    karasurutokarasureba,
    kotoDakara,
    kotoNiNatteiru,
    osoraku,
    sasuga,
    souieba,
    souninai,
    sonoue,
    soretomo,
    sorenanoni,
    sorenara,
    sorenishitemo,
    tachimachi,
    tattano,
    tatte,
  ],
};
