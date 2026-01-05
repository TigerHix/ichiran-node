import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './という風に.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the という風に grammar rule
const negatives = [
  // 風 alone (without という) - literal "wind"
  '風が強い。',
  '風が吹いている。',
  '台風が来る。',
  '風邪を引く。',

  // 風に (kaze ni) - "into the wind" (literal wind + particle)
  '風に向かって走る。',
  '風に乗る。',
  '風に揺られる。',
  '風に当たる。',

  // という alone (without 風に) - quotative particle only
  '彼は来るという。',
  'これは大事なことだという。',
  '彼女は医者だという。',

  // というように (to iu you ni) - more direct comparison
  // Different grammar point (ように・ような)
  '彼は知っているというように振る舞う。',
  '彼女は怒っていないというように見える。',

  // かのようだ (ka no you da) - "as if, just like"
  // More imaginative/unreal comparison
  '彼は幻かのようだ。',
  'まるで夢かのようだ。',
  '彼女は天使かのようだ。',

  // ように・ような (you ni/you na) - direct similarity
  // More direct comparison than という風に
  '彼は日本人のように日本語を話す。',
  '彼女は母のように優しい。',
  '水のように流れる。',

  // ふうに (fuu ni) - standalone manner/mode
  // Without the いう quotation component
  'こんなふうにしてください。',
  'そんなふうに言わないで。',
  'あんなふうに書く。',

  // という風 (to iu fuu) - without に particle or with different particle
  // This is incomplete grammar or different structure
  'その状態を活躍という風で表現する。',
  '彼女はそれをあるという風だと思っている。',
  'これは私の考えという風です。',

  // Similar sounding but unrelated patterns
  // というのは (to iu no wa) - "the thing called..."
  '日本というのは国の名前だ。',
  '愛というのは難しい。',

  // とかいう (to ka iu) - "things like, or something"
  '彼とかいう人から電話があった。',
  '東京とかいうところに行く。',

  // といった (to itta) - "such as, like"
  'りんごといった果物が好きだ。',
  '東京といった都会が嫌いだ。',

  // というと (to iu to) - "speaking of, as for"
  '夏というと海を思い出す。',
  '彼というと忘れられない。',

  // 風が (kaze ga) - "wind" as subject
  '風が涼しい。',
  '風が止んだ。',

  // Different patterns with 風
  // 風情 (fuzei) - atmosphere, charm
  'この店には風情がある。',
  '古い家の风情が感じられる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
