import type { Ruleset } from '../../../ruleset.js';
import atode from './あとで.js';
import amarinai from './あまり-ない.js';
import dandan from './だんだん.js';
import dakedenaku from './だけでなく.js';
import teshimau from './てしまう-ちゃう.js';
import verbTeB from './verbて-b.js';
import verbTeB2 from './verbて-b2.js';
import verbTeNounDeB from './verb-て-noun-で-b.js';
import ika from './いか.js';
import igai from './いがい.js';
import itasu from './いたす.js';
import irassharu from './いらっしゃる.js';
import shikanai from './しか-ない.js';
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
import dasu from './だす.js';
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
    verbTeB,
    verbTeB2,
    verbTeNounDeB,
    ika,
    igai,
    itasu,
    irassharu,
    shikanai,
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
    dasu,
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
