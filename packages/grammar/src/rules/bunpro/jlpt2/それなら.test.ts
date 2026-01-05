import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それなら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それなら grammar rule
const negatives = [
  // なら (nara) alone - general conditional without "that" reference
  '行くなら、私も行きたい。',
  '雨なら行かない。',
  'できるならやってみて。',
  'そんなに嫌なら、やめればいい。',

  // それ (sore) alone - demonstrative pronoun
  'それは私の本です。',
  'それを見せてください。',
  'それを食べました。',

  // で + は as separate particles (locative では, not conjunction)
  '東京では雨が降っている。',
  '日本では桜が有名です。',
  'ここでは喫煙禁止です。',

  // Similar conjunctions with different meanings
  // では・それでは (dewa/soredewa) - "well then, in that case" (broader usage)
  'それでは、始めましょう。',
  'では、また明日。',

  // その場合なら (sono baai nara) - "in that specific case" (more formal)
  'その場合なら、別の方法を考えましょう。',

  // そうすれば (sureba) - "if (you) do that" (conditional verb form)
  'そうすればうまくいくと思います。',
  'そうすれば問題は解決するだろう。',

  // それなのに (sore noni) - "even though that, despite that" (concessive)
  'それなのに、彼は来なかった。',
  '頑張ったのに、それなのだ。',

  // それにしても (sore ni shitemo) - "even so, be that as it may" (adverbial)
  'それにしても、彼は遅いな。',

  // それから (sorekara) - "and then, after that" (temporal sequence)
  '朝ごはんを食べた。それから学校に行った。',

  // それでも (soredemo) - "even so, nevertheless" (concessive)
  '雨が降っている。それでも出かけます。',

  // それとも (soretomo) - "or, or else" (alternative question)
  'コーヒーにしますか？それとも紅茶にしますか？',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
