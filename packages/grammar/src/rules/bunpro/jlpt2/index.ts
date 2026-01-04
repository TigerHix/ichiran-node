import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
// import kaneru from './かねる.js'; // Has syntax errors
import karashite from './からして.js';
import karasurukarasureba from './からすると-からすれば.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, ageku, iwayuru, kanaikanouniuchi, karashite, karasurukarasureba],
};

