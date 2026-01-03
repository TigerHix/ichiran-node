import type { Ruleset } from '../../ruleset.js';
import { BUNPRO_JLPT1 } from './jlpt1/index.js';
import { BUNPRO_JLPT2 } from './jlpt2/index.js';
import { BUNPRO_JLPT3 } from './jlpt3/index.js';
import { BUNPRO_JLPT4 } from './jlpt4/index.js';
import { BUNPRO_JLPT5 } from './jlpt5/index.js';
import { BUNPRO_NON_JLPT } from './nonJlpt.js';

export { BUNPRO_JLPT1 } from './jlpt1/index.js';
export { BUNPRO_JLPT2 } from './jlpt2/index.js';
export { BUNPRO_JLPT3 } from './jlpt3/index.js';
export { BUNPRO_JLPT4 } from './jlpt4/index.js';
export { BUNPRO_JLPT5 } from './jlpt5/index.js';
export { BUNPRO_NON_JLPT } from './nonJlpt.js';

/** All Bunpro rulesets */
export const BUNPRO_RULESETS: Ruleset[] = [
  BUNPRO_JLPT1,
  BUNPRO_JLPT2,
  BUNPRO_JLPT3,
  BUNPRO_JLPT4,
  BUNPRO_JLPT5,
  BUNPRO_NON_JLPT,
];

/** Ruleset id -> Ruleset map */
export const BUNPRO_RULESETS_BY_ID = new Map<string, Ruleset>(
  BUNPRO_RULESETS.map((rs) => [rs.id, rs])
);
