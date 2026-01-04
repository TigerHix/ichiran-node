import type { Ruleset } from '../../../ruleset.js';
import naihanai from './ない-はない.js';
import naide from './ないで.js';
import naito from './ないと.js';
import nara from './なら.js';
import narubeku from './なるべく.js';
import nancounteka from './なん-counter-か.js';
import nifrequency from './に-frequency.js';
import nikigatsuku from './にきがつく.js';
import nikui from './にくい.js';
import nisurukusuru from './にする-くする.js';
import nimieru from './にみえる.js';
import nodaroka from './のだろうか.js';
import nonakade from './のなかで.js';
import nonidespite from './のに-despite.js';
import noyoni from './のように-のような.js';
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
import daitai from './だいたい.js';
import dagadega from './だが・ですが.js';
import demo from './でも.js';
import demodemo from './でも-でも.js';
import dedekirukaradekiru from './でできる-からできる.js';
import degozaimasu from './でございます.js';
import toconditional from './と-conditional.js';
import totodochiraga from './と-と-どちらが.js';
import toiwareteiru from './といわれている.js';
import tokiita from './ときいた.js';
import toomieru from './とみえる.js';
import toomou from './とおもう.js';
import tosareteiru from './とされている.js';
import tokikatoka from './とか-とか.js';
import tokangaeRareteiru from './とかんがえられている.js';
import toki from './とき.js';
import toii from './といい.js';
import toiuKoto from './ということ.js';
import toitemoii from './といってもいい.js';
import toutou from './とうとう.js';
import dondon from './どんどん.js';
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
import tekurenyaoraenai from './てくれない-てもらえない.js';
import tekureru from './てくれる.js';
import tesumimasen from './てすみません.js';
import tehoshii from './てほしい.js';
import temiru from './てみる.js';
import temo from './ても.js';
import temorau from './てもらう.js';
import teyokatta from './てよかった.js';
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
import wahitotsu from './は-の一つだ.js';
import hajimeru from './はじめる.js';
import hazugaai from './はずがない.js';
import hazuda from './はずだ.js';
import ba from './ば.js';
import baaiha from './ばあいは.js';
import bayokatta from './ばよかった.js';
import hitsuyougaaru from './ひつようがある.js';
import hokanimohokaniha from './ほかにも-ほかには.js';
import hotondo from './ほとんど.js';
import mazu from './まず.js';
import mata from './また.js';
import mainoyouni from './まい-のように.js';
import made from './まで.js';
import madeni from './までに.js';
import mitaini from './みたいに-みたいな.js';
import mitai from './みたい.js';
import moshi from './もし.js';
import yasui from './やすい.js';
import youou from './よう-おう.js';
import youda from './ようだ.js';
import youtoomou from './ようと思う-おうと思う.js';
import youni from './ように.js';
import yonitehoshii from './ように-てほしい.js';
import youniyouna from './ように-ような.js';
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
import kasuru from './化する.js';
import kata from './かた.js';
import kadouka from './かどうか.js';
import kana from './かな.js';
import kamoshirenai from './かもしれない.js';
import gasuru from './がする.js';
import gahitsuyou from './がひつよう.js';
import gahoshii from './がほしい.js';
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
import sou from './そう.js';
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

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [
    atode,
    amarinai,
    ba,
    baaiha,
    bayokatta,
    causative,
    causativepassive,
    daitai,
    dagadega,
    dakede,
    dakedenaku,
    dasu,
    dandan,
    dedekirukaradekiru,
    degozaimasu,
    demo,
    demodemo,
    describingverbs,
    dondon,
    gasuru,
    gahitsuyou,
    gahoshii,
    gamirareru,
    garu,
    gozaimasu,
    gotoni,
    goro,
    hajimeru,
    hazugaai,
    hazuda,
    hitsuyougaaru,
    hokanimohokaniha,
    hotondo,
    ika,
    igai,
    irassharu,
    itasu,
    janaika,
    kai,
    kana,
    kamoshirenai,
    kasuru,
    kata,
    kadouka,
    kashira,
    koto,
    kotogadekiru,
    mazu,
    mainoyouni,
    made,
    madeni,
    mata,
    mitai,
    mitaini,
    moshi,
    nado,
    naihanai,
    naide,
    naito,
    nakerebaikenai,
    nakerebanaranai,
    nakute,
    nakuteconjunction,
    nakutemoii,
    naosu,
    nagara,
    nara,
    nasai,
    nasaru,
    narubeku,
    nancounteka,
    nifrequency,
    nikigatsuku,
    nikui,
    nimieru,
    nisurukusuru,
    nodaroka,
    nonidespite,
    nonakade,
    noyoni,
    numberamountwa,
    numbershikanai,
    numbermo,
    okudasai,
    oninaru,
    owaru,
    osuru,
    questionphraseka,
    sa,
    shishi,
    shikanai,
    sonnakonnaanna,
    sonnani,
    sou,
    souiu,
    sounisouna,
    sorede,
    soredemo,
    soreni,
    sukunakunai,
    sukoshimonai,
    tagaru,
    takokoroda,
    tabakari,
    tatoba,
    tara,
    taradou,
    teageru,
    tearu,
    teiku,
    teita,
    teitadakemasenka,
    teiruaidani,
    teirutokoroda,
    teoku,
    tekuru,
    tekurenyaoraenai,
    tekureru,
    tekuretearigatou,
    temiru,
    temo,
    temorau,
    teshimau,
    tesumimasen,
    tehoshii,
    teyokatta,
    toconditional,
    toiwareteiru,
    toii,
    tokikatoka,
    tokangaeRareteiru,
    toki,
    tokiita,
    toitemoii,
    toomieru,
    toomou,
    toiuKoto,
    totodochiraga,
    tosareteiru,
    toutou,
    transitiveintransitive,
    tsuzukeru,
    verbnaide,
    verbpassive,
    verbpotential,
    verbteb,
    verbteb2,
    verbtereq,
    verbtenoundeb,
    wahitotsu,
    youda,
    youni,
    youniyouna,
    yonitehoshii,
    youou,
    yasui,
    youtoomou,
    zenzen,
    zutto1,
  ],
};
