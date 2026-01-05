import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけのことはある.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the だけのことはある grammar rule
const negatives = [
  // だけ alone (without のことはある) - "only, just"
  'これだけあれば十分です。',
  'あの人だけが好きです。',
  '今日だけ discount があります。',
  '時間だけあれば、何でもできる。',

  // だけに (dakeni) - "all the more because, as might be expected"
  // Similar meaning but appears mid-sentence and connects clauses
  '試験に合格してだけに、喜びは大きい。',
  '子供だけに、素直な反応をした。',
  '値段が高いだけに、品質も良いだろう。',

  // だけあって (dakeatte) - "as befits, worthy of, as expected of"
  // Also appears before a phrase stating the result
  '彼はプロだけあって、上手だ。',
  'この店は有名だけあって、いつも込んでいる。',
  '日本に10年いただけあって、日本語がぺらぺらだ。',

  // だけで (dakede) - "with only, just by"
  'これだけで完成です。',
  '名前だけで分かる。',
  'お金だけでは買えない。',

  // だけの (dake no) - "only, mere" (pre-nominal, without ことはある)
  '彼はただのだけの理由で来なかった。',
  'それだけのことはできない。',
  '十分だけの金を持っている。',

  // ことはあるが (koto wa aru ga) - "there are cases where, but..."
  '苦労することはあるが、楽しいです。',
  '遅れることはあるが、いつも頑張っている。',
  '失敗することはあるが、諦めない。',

  // だけのことは (dake no koto wa) - incomplete phrase (missing ある)
  // This shouldn't match as a complete grammar pattern
  '練習しただけのことは…',
  'それだけのことは分かっている。',

  // Similar patterns with だけ but different grammar
  // だけに (already covered above)

  // ことになっている (koto ni natteiru) - "it is arranged that, it's the rule"
  '会議は10時からことになっている。',
  '日曜日は休みことになっている。',

  // ことだから (koto dakara) - "because it is"
  '彼のことだから、忘れているだろう。',
  '子供のことだから、分からない。',

  // Noun + のこと (no koto) - "the thing of/about" (different usage)
  '彼のことが好きです。',
  '日本のことを勉強したい。',
  '父のことが心配です。',

  // ある (aru) - existential verb "there is" (without だけのことは)
  '部屋に人がある。',
  '便利なものがある。',

  // だけではない (dakedewa nai) - "not only, not just"
  'それだけではない。',
  'お金だけではない。',
  '日本語だけではない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
