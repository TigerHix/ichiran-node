import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import zaru from './ざる.js';
import kananika from './か何か.js';
import omakeni from './おまけに.js';
// import zaruOenai from './ざるを得ない.js'; // Temporarily disabled due to error
import kotoNiNatteiru from './ことになっている.js';
import kotohaniwanaranai from './ことにはならない.js';
import gakininaru from './が気になる.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, zaru, kananika, omakeni, kotoNiNatteiru, kotohaniwanaranai, gakininaru],
};
