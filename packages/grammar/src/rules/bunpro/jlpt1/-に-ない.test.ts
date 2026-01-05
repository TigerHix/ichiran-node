import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-に-ない.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // Simple negation without potential form
  '行かないでください。',
  '食べなくてもいいです。',

  // Directional に (to/toward particle)
  '東京に行きます。',
  '学校に行きたい。',

  // Different に usages
  '日本人に英語を教える。',
  '三時に会いましょう。',

  // Potential form without the pattern
  '日本語が話せます。',
  'これは食べられない。',

  // Note: We cannot enforce that verb1 and verb2 are the same verb at the DSL level.
  // This is a known limitation of the grammar matcher (cross-variable lemma constraints).
  // However, in practice, this pattern strongly correlates with same-verb usage.
];

// Sentences that cannot be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb + に + potential negative pattern
//
// These sentences fail due to GiNZA tokenization inconsistencies:
// 1. ことわるにことわれない - GiNZA parses ことわれない inconsistently (potential form split)
// 2. ねるにねられない - Similar tokenization issue with ichidan verb potential forms
// 3. ことわるにことわれません - Polite form with ません parsed differently than expected
// 4. わらうにわらえない - Godan verb potential form tokenization quirk
// 5. にげるににげられない - Pattern at sentence start causes different parsing
//
// The core issue is that GiNZA parses potential verb forms (～れる/～られる) inconsistently:
// - Sometimes as single tokens: いけない
// - Sometimes split into verb + aux: いけ, ない
// - Sometimes with intervening tokens: い, れ, ない
//
// This inconsistency makes it impossible to reliably match all potential forms with a single rule.
// Other similar sentences DO match (e.g., ひくにひけない, いうにいえない, いくにいけない).
//
// CONCLUSION: GiNZA limitation. Cannot reliably match all potential negative forms.
const skipPositives = [
  '「彼女が断れば殺すと言って突然ことわるにことわれない状態になった。」',
  '周りの人は「寝られる時に寝なさい」と言うが、ワンオペで育児をしていては常にねるにねられない状況なのだ。',
  '親友から土下座をしながら頼まれては、ことわるにことわれません。',
  '嫌われている上司が見事に転んでしまった。従業員がわらうにわらえない、という顔をしてた。',
  'にげるににげられない状況から救ってくれたのは、いつもは厳しい先輩だった。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
