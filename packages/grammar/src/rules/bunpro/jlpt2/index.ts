import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import souieba from './そういえば.js';
import kotohaniwanaranai from './ことにはならない.js';
import zaruOenai from './ざるを得ない.js';
import zaru from './zaru.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, souieba, kotohaniwanaranai, zaruOenai, zaru],
};
