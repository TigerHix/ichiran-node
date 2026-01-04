import type { Ruleset } from '../../../ruleset.js';
import atode from './あとで.js';
import amarinai from './あまり-ない.js';
import dandan from './だんだん.js';
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
import causative from './causative.js';
import questionPhraseKa from './question-phrase-か.js';
import gasuru from './がする.js';
import gahitsuyou from './がひつよう.js';
import gahoshii from './がほしい.js';
import gamirareru from './がみられる.js';
import kotogadekiru from './ことができる.js';
import numberMo from './number-も.js';
import numberAmountWa from './number-amount-は.js';
import sou from './そう.js';
import soreni from './それに.js';
import tagaru from './たがる.js';
import tsuzukeru from './つづける.js';
import verbPassive from './Verb[passive].js';

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [
    atode,
    amarinai,
    dandan,
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
    causative,
    questionPhraseKa,
    kotogadekiru,
    numberMo,
    sou,
    soreni,
    tagaru,
    tsuzukeru,
    gasuru,
    gahitsuyou,
    gahoshii,
    gamirareru,
    numberAmountWa,
    verbPassive,
  ],
};
