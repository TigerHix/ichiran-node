import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
import kaneru from './かねる.js';
import karashite from './からして.js';
import karasurutokarasureba from './からすると-からすれば.js';
import kotoDakara from './ことだから.js';
import kotoNiNatteiru from './ことになっている.js';
import sasuga from './さすが.js';
import semete from './せめて.js';
import shitagatte from './したがって.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, ageku, iwayuru, kanaikanouniuchi, kaneru, karashite, karasurutokarasureba, kotoDakara, kotoNiNatteiru, sasuga, semete, shitagatte],
};

