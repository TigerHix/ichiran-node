import type { Ruleset } from '../../../ruleset.js';
import woHete from './を経て.js';
import jaArumaishi from './じゃあるまいし.js';
import akumademo from './あくまでも.js';

export const BUNPRO_JLPT1: Ruleset = {
  id: 'bunpro.jlpt1',
  rules: [woHete, jaArumaishi, akumademo],
};
