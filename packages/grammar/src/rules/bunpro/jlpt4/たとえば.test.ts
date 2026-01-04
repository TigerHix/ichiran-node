import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たとえば.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // たとえ without ば (different grammar - "even if")
  'たとえ雨が降っても、行きます。',
  'たとえ忙しくても、時間を作ってください。',
  'たとえお金があっても、買わない。',
  // といえば (different grammar - "speaking of")
  '日本語といえば、漢字が難しいですね。',
  '彼といえば、最近忙しいそうです。',
  // としたら (different grammar - "if we suppose")
  'ここに行ったとしたら、何時間かかりますか。',
  '彼が来たとしたら、パーティーはもっと楽しいだろう。',
  // として (different grammar - "as" or "assuming")
  '学生として、勉強が一番大切です。',
  '彼は医者として働いています。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
