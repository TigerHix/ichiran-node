import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たて.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // 立てる (tateru - transitive "to stand up/establish")
  // Different word, not the たて suffix
  '彼は店を立てた。',
  '新しい会社を立てる予定だ。',
  '証拠を立てる。',
  // 立て (tate - as in "batch/pair")
  '二つで一立てだ。',
  // 縦 (tate - vertical)
  '縦に書く。',
  '縦と横',
];

// Skip positives: GiNZA tokenization limitations for compound forms
// GiNZA tokenizes "verb stem + たて" as single compound tokens (e.g., 焼きたて, 揚げたて)
// These compound tokens have text that ends with "て" not "たて" as a separate substring
// For example:
// - 焼きたて has text="焼きたて" but GiNZA doesn't tokenize "たて" separately
// - 揚げたて has text="揚げたて" with same issue
// - The lemma might be "立てる" or related, but text-based matching fails
// - Our DSL requires matching actual token text, not substrings
//
// This is the same limitation as the かけ (JLPT3) rule, which also skips compound forms
const skipPositives = [
  'ここにあるのは全部揚げたてですよ。',        // "揚げたて" = compound token
  '私は出来たてのパンを食べるのが大好きです。',    // "出来たて" = compound token
  '生まれたての赤ちゃんっておさるさんみたいじゃない？', // "生まれたて" = compound token
  'できたてのお菓子を食べたのは初めてだ。',       // "できたて" = compound token
  'このお店ではとりたての野菜が買えます。',       // "とりたて" = compound token
  'もぎたての果物はみずみずしい。',            // "もぎたて" = compound token
  'このベンチはペンキぬりたてのため、気を付けてください。', // "ぬりたて" = compound token
  '彼女は女優になりたてです。',               // "なりたて" = compound token
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
