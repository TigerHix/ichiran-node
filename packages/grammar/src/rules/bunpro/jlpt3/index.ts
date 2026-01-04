import type { Ruleset } from '../../../ruleset.js';
import dewaSoredewa from './では-それでは-じゃあ.js';
import youToShinai from './-ようとしない.js';
import kotoGaAru from './ことがある.js';
import kotoKara from './ことから.js';
import koso from './こそ.js';
import kuseni from './くせに.js';
import kotoNiSuru from './ことにする.js';

export const BUNPRO_JLPT3: Ruleset = {
  id: 'bunpro.jlpt3',
  rules: [dewaSoredewa, youToShinai, kotoGaAru, kotoKara, koso, kuseni, kotoNiSuru],
};
