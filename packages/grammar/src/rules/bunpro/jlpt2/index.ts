import type { Ruleset } from '../../../ruleset.js';
import tekoso from './-てこそ.js';
import nouchide from './-のうち-で.js';
import shikamo from './しかも.js';
import shikashinagara from './しかしながら.js';
import zaru from './ざる.js';

export const BUNPRO_JLPT2: Ruleset = {
  id: 'bunpro.jlpt2',
  rules: [tekoso, nouchide, shikamo, shikashinagara, zaru],
};
