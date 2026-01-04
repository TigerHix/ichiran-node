import type { Ruleset } from '../../../ruleset.js';
import atode from './あとで.js';
import amarinai from './あまり-ない.js';
import dakedenaku from './だけでなく.js';
import teshimau from './てしまう-ちゃう.js';
import ika from './いか.js';
import igai from './いがい.js';
import itasu from './いたす.js';
import irassharu from './いらっしゃる.js';
import okudasai from './お-ください.js';
import osuru from './お-する.js';
import oninaru from './お-になる.js';
import owaru from './おわる.js';
import kai from './かい.js';
import kashira from './かしら.js';
import kata from './かた.js';
import kadouka from './かどうか.js';
import kana from './かな.js';
import kamoshirenai from './かもしれない.js';
import gasuru from './がする.js';
import gahitsuyou from './がひつよう.js';
import gahoshii from './がほしい.js';
import gamirareru from './がみられる.js';
import gotoni from './ごとに.js';

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [
    atode,
    amarinai,
    dakedenaku,
    teshimau,
    ika,
    igai,
    itasu,
    irassharu,
    okudasai,
    osuru,
    oninaru,
    owaru,
    kai,
    kashira,
    kata,
    kadouka,
    kana,
    kamoshirenai,
    gasuru,
    gahitsuyou,
    gahoshii,
    gamirareru,
    gotoni,
  ],
};
