import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけでなく-て-も.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // だけで without なく - simple "only/with" meaning
  '三日だけで完成した。',
  '一人だけで行く。',

  // だけ + ない separately (simple negation of "only")
  '彼だけない。',

  // でなく without だけ (not X but Y - different grammar point)
  '問題は彼でなく私だ。',
  'これは本でなく雑誌だ。',

  // だけ and なく in separate clauses
  'これだけ食べたら、もう食べなくていい。',

  // て-form verb + も (even if / even) - different grammar
  // Example: 雨が降っても行く (even if it rains, I'll go)
  '雨が降っても行きます。',
  '疲れても寝られない。',

  // Simple も (also) without だけでなく construction
  '私も行きます。',
  '子供も知っている。',

  // だけでなく without も after second element (JLPT4 grammar)
  // This is the base pattern without the emphatic も
  '日本語だけでなく、韓国語も話せる。',  // Actually has も, would match
  '彼は優しくて、親切だけど、ちょっと寂しい。',  // Different grammar
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
