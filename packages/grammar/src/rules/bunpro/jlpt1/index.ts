import type { Ruleset } from '../../../ruleset.js';

// Batch 1: New JLPT1 rules (40 rules)
import narinari from './-なり-なり.js';
import ninai from './-に-ない.js';
import yougamaigato from './-よう--う-まい-が-と.js';
import akumademo from './あくまでも.js';
import atteno from './あっての.js';
import ikukumonantomonai from './い-adj-く-もなんともない.js';
import ikanaru from './いかなる.js';
import ikanzu from './いかん-ず.js';
import katagata from './かたがた.js';
import katawara from './かたわら.js';
import karaaru from './からある.js';
import karasuru from './からする.js';
import karekare from './かれ-かれ.js';
import kainika from './か否か.js';
import gatera from './がてら.js';
import gantotally from './がん-totally.js';
import gahayaika from './が早いか.js';
import kiraiegaaru from './きらいがある.js';
import kurainara from './ぐらいなら.js';
import gurumide from './ぐるみで.js';
import kososurenai from './こそすれ-ない.js';
import kotodashi from './ことだし.js';
import kototote from './こととて.js';
import sazo from './さぞ.js';
import sura from './すら.js';
import zukume from './ずくめ.js';
import zujimai from './ずじまい.js';
import zutomo from './ずとも.js';
import zuniwaokanai from './ずにはおかない.js';
import zuniwasumanai from './ずにはすまない.js';
import sobakara from './そばから.js';
import soremadeda from './それまでだ.js';
import tadanomi from './ただ-のみ.js';
import tatokorode from './たところで.js';
import tanarinari from './たなり-なり.js';
import tamameda from './たまでだ.js';
import tarade from './たら-で.js';
import tarasaigo from './たら最後.js';
import taru from './たる.js';
import danishinai from './だに-しない.js';

// Previously existing rules
import jaArumaishi from './じゃあるまいし.js';
import woHete from './を経て.js';

export const BUNPRO_JLPT1: Ruleset = {
  id: 'bunpro.jlpt1',
  rules: [
    // Batch 1: New JLPT1 rules (alphabetical order)
    akumademo,
    atteno,
    danishinai,
    gahayaika,
    gantotally,
    gatera,
    gurumide,
    ikanaru,
    ikanzu,
    ikukumonantomonai,
    jaArumaishi,
    kainika,
    karasuru,
    karaaru,
    katagata,
    katawara,
    karekare,
    kiraiegaaru,
    kososurenai,
    kotodashi,
    kototote,
    kurainara,
    narinari,
    ninai,
    sazo,
    sura,
    sobakara,
    soremadeda,
    tadanomi,
    tamameda,
    tanarinari,
    tarade,
    tarasaigo,
    taru,
    tatokorode,
    woHete,
    yougamaigato,
    zukume,
    zujimai,
    zuniwaokanai,
    zuniwasumanai,
    zutomo,
  ],
};
