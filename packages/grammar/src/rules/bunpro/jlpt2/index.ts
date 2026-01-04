import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
import kaneru from './かねる.js';
import karashite from './からして.js';
import karasurutokarasureba from './からすると-からすれば.js';
import katoomottara from './かと思ったら-かと思うと.js';
import kotoDakara from './ことだから.js';
import kotoNiNatteiru from './ことになっている.js';
import sasuga from './さすが.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, ageku, iwayuru, kanaikanouniuchi, kaneru, karashite, karasurutokarasureba, katoomottara, kotoDakara, kotoNiNatteiru, sasuga],
};

