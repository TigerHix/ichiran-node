import type { Ruleset } from '../../../ruleset.js';

import amari from './あまり.js';
import amariNi from './あまりに.js';
import ari from './あり.js';
import aruiwa from './あるいは.js';
import ikuraDemo from './いくら-でも.js';
import uchiNi from './うちに.js';
import okageDe from './おかげで.js';
import okiNi from './おきに.js';
import kake from './かけ.js';
import katoiuto1 from './かというと1.js';
import kanari from './かなり.js';
import karaKoso from './からこそ.js';
import karaIuTo from './から言うと.js';
import gatai from './がたい.js';
import gachi from './がち.js';
import kiri from './きり.js';
import gimi from './ぎみ.js';
import kuseni from './くせに.js';
import kurai2 from './くらい2.js';
import koso from './こそ.js';
import kotoKa from './ことか.js';
import kotoKara from './ことから.js';
import kotoGaAru from './ことがある.js';
import kotoDa from './ことだ.js';
import kotoNano from './ことなの.js';
import kotoNi from './ことに.js';
import kotoNiSuru from './ことにする.js';
import kotoNiNaru from './ことになる.js';
import kotoWaNai from './ことはない.js';
import saCasualYo from './さ-casualよ.js';
import sekkaku from './せっかく.js';
import dewaSoredewa from './では-それでは-じゃあ.js';
import youToShinai from './-ようとしない.js';

export const BUNPRO_JLPT3: Ruleset = {
  id: 'bunpro.jlpt3',
  rules: [
    amari,
    amariNi,
    ari,
    aruiwa,
    ikuraDemo,
    uchiNi,
    okageDe,
    okiNi,
    kake,
    katoiuto1,
    kanari,
    karaKoso,
    karaIuTo,
    gatai,
    gachi,
    kiri,
    gimi,
    kuseni,
    kurai2,
    koso,
    kotoKa,
    kotoKara,
    kotoGaAru,
    kotoDa,
    kotoNano,
    kotoNi,
    kotoNiSuru,
    kotoNiNaru,
    kotoWaNai,
    saCasualYo,
    sekkaku,
    dewaSoredewa,
    youToShinai,
  ],
};
