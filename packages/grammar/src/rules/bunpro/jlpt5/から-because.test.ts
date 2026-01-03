import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から-because.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // から as source/starting point (ADP + case dep)
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '会社から家まで歩きます。',
  '京都から来ました。',
  // から as origin/source in time/space
  '昨日から雨が降っている。',
  'ここから見える。',
  // から with particles (not reason)
  '彼からもらった。',
  '先生から聞いた。',
  // から as "from" in passive constructions
  '彼は皆から愛されています。',
  // Noun + から where から is case marker (origin/source)
  '日本から来た人。',
  '彼から電話があった。',
  // から as "from" in causative
  '彼を行かせた。',
  // から in comparisons or other uses
  'これから作る。',
  '彼より背が高い。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
