import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
import adjectiveTeNounDe from './adjective-て-noun-で.js';
import adjectiveNoWa from './adjective-の-は.js';
import iAdjectives from './い-adjectives.js';
import iAdjectiveKuNakatta from './い-Adjective-くなかった.js';
import iAdjectiveNoun from './い-adjective-noun.js';
import iAdjectivePredicate from './い-adjective-predicate.js';
import ndesu from './-んです-のです.js';
import tsumorida from './つもりだ.js';
import ka from './か.js';
import kaOr from './か-or.js';
import karaBecause from './から-because.js';
import kara from './から.js';
import ga from './が.js';
import gaBut from './が-but.js';
import gaAru from './がある.js';
import gaAruNoun from './がある-noun.js';
import gairu from './がいる.js';
import kuru from './くる.js';
import shite from './して.js';
import suru from './する.js';
import tekara from './てから.js';
import teKudasai from './てください.js';
import teWaIkenai from './てはいけない.js';
import verbTeB from './verb-て-b.js';
import verbTe from './verb-て.js';
import verbTemoIi from './verb-てもいい.js';
import verbTaNoun from './verb-た-noun.js';
import verbNiIku from './verb-にいく.js';
import verbNonPast from './verb-non-past.js';
import da from './だ.js';
import dake from './だけ.js';
import dare from './だれ.js';
import darou from './だろう.js';
import dattadeshita from './だった-でした.js';
import desu from './です.js';
import deshou from './でしょう.js';
import tai from './たい.js';
import takotoegaaru from './たことがある.js';
import tahogaii from './たほうがいい.js';
import takusan from './たくさん.js';
import taritari from './たり-たりする.js';
import janai from './じゃない.js';
import janakatta from './じゃなかった.js';
import naAdjectiveNoun from './な-adjective-noun.js';
import naAdjectivePredicate from './な-adjective-predicate.js';
import naAdjectives from './な-adjectives.js';
import naidekudasai from './ないでください.js';
import nahogaii from './ないほうがいい.js';
import nakucha from './なくちゃ-なきゃ.js';
import nakuteisikenai from './なくてはいけない.js';
import wa from './は.js';
import wo from './を.js';
import de from './で.js';
import deBy from './で-by.js';
import ni from './に.js';
import niSuru from './にする.js';
import he from './へ.js';
import teiru1 from './ている1.js';
import teiru2 from './ている2.js';
import teiru3 from './ている3.js';
import to from './と.js';
import toAnd from './と-and.js';
import toWith from './と-with.js';
import tte from './って.js';
import yo from './よ.js';
import ne from './ね.js';
import no from './の.js';
import na from './な.js';
import ageru from './あげる.js';
import ano from './あの.js';
import are from './あれ.js';
import asoko from './あそこ.js';
import ii from './いい.js';
import uVerbs from './う-Verbs.js';
import sugiru from './すぎる.js';
import uverbNai from './うverb--ない.js';
import uVerbPast from './う-verb-past.js';
import uVerbNegPast from './う-verb-neg-past.js';
import nounMade from './noun-まで.js';
import vMade from './v-まで.js';
import nogaheta from './のがへた.js';
import negativeIAdjectives from './negative-い-adjectives.js';
import pastTenseIAdjectives from './past-tense-い-adjectives.js';
import politeVerbEndings from './polite-verb-endings.js';
import kirai from './きらい.js';
import suki from './すき.js';
import kurai1 from './くらい1.js';
import kureru from './くれる.js';
import kekkou from './けっこう.js';
import kedoDakedo from './けど-だけど.js';
import keredomo from './けれども.js';
import koko from './ここ.js';
import kono from './この.js';
import kore from './これ.js';
import soko from './そこ.js';
import sono from './その.js';
import sore from './それ.js';
import doko from './どこ.js';
import dono from './どの.js';
import dore from './どれ.js';
import mo from './も.js';

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [
    adjectivete,
    adjectiveNoWa,
    adjectiveTeNounDe,
    ageru,
    ano,
    are,
    asoko,
    dake,
    da,
    darou,
    dattadeshita,
    dare,
    desu,
    deshou,
    gaAru,
    gaAruNoun,
    gaBut,
    ga,
    gairu,
    ii,
    iAdjectiveKuNakatta,
    sugiru,
    iAdjectiveNoun,
    iAdjectivePredicate,
    iAdjectives,
    janai,
    janakatta,
    ka,
    kaOr,
    kara,
    karaBecause,
    kedoDakedo,
    keredomo,
    kekkou,
    kirai,
    suki,
    koko,
    kono,
    kore,
    kuru,
    kurai1,
    kureru,
    naAdjectiveNoun,
    naAdjectivePredicate,
    naAdjectives,
    naidekudasai,
    nahogaii,
    nakucha,
    nakuteisikenai,
    tahogaii,
    takotoegaaru,
    takusan,
    tai,
    taritari,
    de,
    deBy,
    ni,
    niSuru,
    he,
    teiru1,
    teiru2,
    teiru3,
    to,
    toAnd,
    toWith,
    tte,
    wo,
    wa,
    yo,
    ne,
    no,
    na,
    ndesu,
    negativeIAdjectives,
    nogaheta,
    nounMade,
    pastTenseIAdjectives,
    politeVerbEndings,
    shite,
    suru,
    soko,
    sono,
    sore,
    doko,
    dono,
    dore,
    mo,
    tekara,
    teKudasai,
    teWaIkenai,
    tsumorida,
    uVerbs,
    uVerbNegPast,
    uVerbPast,
    uverbNai,
    vMade,
    verbNiIku,
    verbNonPast,
    verbTaNoun,
    verbTe,
    verbTeB,
    verbTemoIi,
  ],
};
