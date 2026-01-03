import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './にする.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Resultative にする (to change into, to make something)
  // These have different semantic meaning even though form is similar
  // '静かにする。',  // Make it quiet (hard to distinguish syntactically)
  // '部屋をきれいにする。',  // Make the room clean (uses を not に)

  // Other に particle usages (not choice/decision)
  '東京に行く。',  // に as direction marker (not with する)
  '日本に住みたい。',  // に as location marker
  '３時に会う。',  // に as time marker
  '先生に聞く。',  // に as indirect object

  // Other と particle usages (not choice/decision)
  '友達と行く。',  // と as "with" companion marker
  '「こんにちは」と言う。',  // と as quotation marker

  // Simple する without particle
  '勉強する。',
  '宿題をする。',
  '掃除する。',

  // Noun + で + する (instrumental, not choice)
  '鉛筆で書く。',
  '日本語で話す。',

  // Potential forms (different meaning)
  'できます。',
  'できますか。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
