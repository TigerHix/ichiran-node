import type { Ruleset } from '../../../ruleset.js';
import atode from './あとで.js';
import amarinai from './あまり-ない.js';
import verbpassive from './Verb[passive].js';
import verbpotential from './Verb[potential].js';
import causativepassive from './causative-passive.js';
import causative from './causative.js';
import describingverbs from './describing-verbs.js';
import numberamountwa from './number-amount-は.js';
import numbershikanai from './number-しか-ない.js';
import numbermo from './number-も.js';
import questionphraseka from './question-phrase-か.js';
import transitiveintransitive from './transitive-intransitive-verbs.js';
import verbtenoundeb from './verb-て-noun-で-b.js';
import verbnaide from './verb-ないで.js';
import verbteb from './verbて-b.js';
import verbteb2 from './verbて-b2.js';
import verbtereq from './verbて-request.js';
import dakedenaku from './だけでなく.js';
import dakede from './だけで.js';
import dasu from './だす.js';
import dandan from './だんだん.js';
import dondon from './どんどん.js';
import demodemo from './でも-でも.js';
import tsuzukeru from './つづける.js';
import teshimau from './てしまう-ちゃう.js';
import tekuretearigatou from './てくれてありがとう.js';
import teageru from './てあげる.js';
import tearu from './てある.js';
import teiku from './ていく.js';
import teita from './ていた.js';
import teitadakemasenka from './ていただけませんか.js';
import teiruaidani from './ているあいだに.js';
import teirutokoroda from './ているところだ.js';
import teoku from './ておく.js';
import tekuru from './てくる.js';
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
import hazuganai from './はずがない.js';
import gamirareru from './がみられる.js';
import garu from './がる.js';
import koto from './こと.js';
import kotogadekiru from './ことができる.js';
import gozaimasu from './ございます.js';
import gotoni from './ごとに.js';
import goro from './ごろ.js';
import sa from './さ.js';
import shishi from './し-し.js';
import shikanai from './しか-ない.js';
import janaika from './じゃないか.js';
import sukunakunai from './すくなくない.js';
import sukoshimonai from './すこしも-ない.js';
import zutto1 from './ずっと1.js';
import zenzen from './ぜんぜん.js';
import souiu from './そういう.js';
import sounisouna from './そうに-そうな.js';
import sorede from './それで.js';
import soredemo from './それでも.js';
import soreni from './それに.js';
import sonnakonnaanna from './そんな-こんな-あんな-どんな.js';
import sonnani from './そんなに.js';
import tagaru from './たがる.js';
import tatoba from './たとえば.js';
import takokoroda from './たところだ.js';
import tabakari from './たばかり.js';
import tara from './たら.js';
import taradou from './たらどう.js';
import daitai from './だいたい.js';
import dagadega from './だが・ですが.js';
import dedekirukaradekiru from './でできる-からできる.js';
import toiwareteiru from './といわれている.js';
import tokiita from './ときいた.js';
import teyokatta from './てよかった.js';
import toutou from './とうとう.js';
import nakerebaikenai from './なければいけない.js';
import nakerebanaranai from './なければならない.js';
import nakute from './なくて.js';
import nakuteconjunction from './なくて-conjunction.js';
import nakutemoii from './なくてもいい.js';
import naosu from './なおす.js';
import nagara from './ながら.js';
import nasai from './なさい.js';
import nasaru from './なさる.js';
import nado from './など.js';
import tekurenyaoraenai from './てくれない-てもらえない.js';
import tekureru from './てくれる.js';
import tesumimasen from './てすみません.js';
import tehoshii from './てほしい.js';
import temiru from './てみる.js';
import temo from './ても.js';
import temorau from './てもらう.js';
import toomou from './とおもう.js';
import toomieru from './とみえる.js';
import tosareteiru from './とされている.js';
import tokikatoka from './とか-とか.ts';
import tokangaeRareteiru from './とかんがえられている.js';
import toki from './とき.js';
import toii from './といい.js';
import toiuKoto from './ということ.js';
import toitemoii from './といってもいい.js';
import toconditional from './と-conditional.ts';
import totodochiraga from './と-と-どちらが.ts';
import demo from './でも.js';
import degozaimasu from './でございます.js';

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [
    atode,
    amarinai,
    verbpassive,
    verbpotential,
    causativepassive,
    causative,
    describingverbs,
    numberamountwa,
    numbershikanai,
    numbermo,
    questionphraseka,
    transitiveintransitive,
    verbtenoundeb,
    verbnaide,
    verbteb,
    verbteb2,
    verbtereq,
    dakedenaku,
    dakede,
    dasu,
    dandan,
    dondon,
    demodemo,
    tsuzukeru,
    teshimau,
    tekuretearigatou,
    teageru,
    tearu,
    teiku,
    teita,
    teitadakemasenka,
    teiruaidani,
    teirutokoroda,
    teoku,
    tekuru,
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
    hazuganai,
    gamirareru,
    garu,
    koto,
    kotogadekiru,
    gozaimasu,
    gotoni,
    goro,
    sa,
    shishi,
    shikanai,
    janaika,
    sukunakunai,
    sukoshimonai,
    zutto1,
    zenzen,
    souiu,
    sounisouna,
    sorede,
    soredemo,
    soreni,
    sonnakonnaanna,
    sonnani,
    tagaru,
    tatoba,
    takokoroda,
    tabakari,
    tara,
    taradou,
    daitai,
    dagadega,
    dedekirukaradekiru,
    toiwareteiru,
    tokiita,
    teyokatta,
    toutou,
    nakerebaikenai,
    nakerebanaranai,
    nakute,
    nakuteconjunction,
    nakutemoii,
    naosu,
    nagara,
    nasai,
    nasaru,
    nado,
    tekurenyaoraenai,
    tekureru,
    tesumimasen,
    tehoshii,
    temiru,
    temo,
    temorau,
    toomou,
    toomieru,
    tosareteiru,
    tokikatoka,
    tokangaeRareteiru,
    toki,
    toii,
    toiuKoto,
    toitemoii,
    toconditional,
    totodochiraga,
    demo,
    degozaimasu,
  ],
};
