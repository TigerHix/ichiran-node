import type { Ruleset } from '../../../ruleset.js';
import dewaSoredewa from './では-それでは-じゃあ.js';
import youToShinai from './-ようとしない.js';
import kurai2 from './くらい2.js';

export const BUNPRO_JLPT3: Ruleset = {
  id: 'bunpro.jlpt3',
  rules: [dewaSoredewa, youToShinai, kurai2],
};
