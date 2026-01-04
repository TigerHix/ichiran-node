import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import ageku from './あげく.js';
import iwayuru from './いわゆる.js';
import gakeni from './がけに.js';
import kanaikanouniuchi from './か-ないかのうちに.js';
import kaneru from './かねる.js';
import kanoyouda from './かのようだ.js';
import karashite from './からして.js';
import karasurutokarasureba from './からすると-からすれば.js';
import karamiruto from './から見ると.js';
import ge from './げ.js';
import kotoDakara from './ことだから.js';
import kotoNaku from './ことなく.js';
import kotoNiNatteiru from './ことになっている.js';
import sasuga from './さすが.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, ageku, iwayuru, gakeni, kanaikanouniuchi, kaneru, kanoyouda, karashite, karasurutokarasureba, karamiruto, ge, kotoDakara, kotoNaku, kotoNiNatteiru, sasuga],
};
