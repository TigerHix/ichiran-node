import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './したがって.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the したがって grammar rule
const negatives = [
  // だから (dagara) - casual "therefore" (different conjunction)
  '雨が降っている。だから、行きません。',
  '彼は病気だ。だから来られない。',

  // ですから (desukara) - polite "therefore" (different conjunction)
  '雨が降っています。ですから、行きません。',
  '彼は病気です。ですから来られません。',

  // なので (nanode) - formal "because/therefore" (different conjunction)
  '雨が降っている。なので、行きません。',
  '彼は病気なので、来られません。',

  // それで (sorede) - "so/then" (temporal sequence, not logical consequence)
  '朝ごはんを食べた。それから学校に行った。',
  '雨が降った。それで、試合が中止になった。',

  // そこで (sokode) - "accordingly/therefore" (action-oriented, different nuance)
  '駅に着いた。そこで友達に会った。',

  // その結果 (sono kekka) - "as a result" (noun phrase, not conjunction)
  '事故があった。その結果、遅刻しました。',

  // ゆえに (yue ni) - "due to/therefore" (more formal, different structure)
  '貧困ゆえに、教育を受けられなかった。',

  // 従う (shitagau) - verb "to follow/comply" (not conjunction)
  'ルールに従って行動してください。',
  '指示に従います。',
  '彼は従順な性格です。',

  // 従って (shitagatte) as verb stem (not conjunction)
  '上司の方針に従って、プロジェクトを進めた。',
  'マニュアルに従って操作してください。',

  // Similar-looking but different grammar patterns
  // に従って (ni shitagatte) - "in accordance with/following" (prepositional phrase)
  '彼の指示に従って仕事をした。',
  'ルールに従ってゲームを進めましょう。',
  '計画に従って進める。',

  // したが (shitaga) - different form (not te-form)
  // This would be grammatically incorrect in standard Japanese, but ensuring we don't match partial forms
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
