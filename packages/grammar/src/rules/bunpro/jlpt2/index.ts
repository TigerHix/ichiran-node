import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import gakininaru from './が気になる.js';
import kotodakara from './ことだから.js';
import souieba from './そういえば.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, gakininaru, kotodakara, souieba],
};
