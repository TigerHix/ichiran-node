import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import gakeni from './がけに.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
import kaneru from './かねる.js';
import kanoyouda from './かのようだ.js';
import karashite from './からして.js';
import karasurutokarasureba from './からすると-からすれば.js';
import karatoitte from './からといって.js';
import ge from './げ.js';
import kotoDakara from './ことだから.js';
import kotoNiNatteiru from './ことになっている.js';
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
    gakeni,
    kanaikanouniuchi,
    kaneru,
    kanoyouda,
    karashite,
    karasurutokarasureba,
    karatoitte,
    ge,
    kotoDakara,
    kotoNiNatteiru,
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
