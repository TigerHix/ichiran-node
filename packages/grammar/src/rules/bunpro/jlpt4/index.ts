import type { Ruleset } from '../../../ruleset.js';
import dakedenaku from './だけでなく.js';
import teshimau from './てしまう-ちゃう.js';

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [dakedenaku, teshimau],
};
