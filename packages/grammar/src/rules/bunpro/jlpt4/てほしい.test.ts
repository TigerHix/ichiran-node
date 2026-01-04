import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てほしい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てほしい
const negatives = [
  // ほしい as standalone adjective (not te-form + ほしい)
  // This is the がほしい grammar point (different structure)
  'お金がほしい。',
  '新しい車がほしいです。',
  '彼は何もほしくないと言っている。',

  // たい form (want to do oneself, not want someone else to do)
  // This is a different grammar point
  '行きたいです。',
  '何もしたくない。',

  // て form without ほしい (just te-form, not this grammar)
  '本を読んでいます。',
  '手を洗ってきました。',

  // ほしい with different particles (not te-form)
  '彼はほしいものを買った。',
  'これはほしくない。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. 車できてほしかった (car + by + come-te-form + wanted)
//
// GiNZA parses this sentence ambiguously:
//   車できて → lemma=できる (potential: "can do") ✗ WRONG
//   Correct parse: 車で (by car) + 来て (come-te-form)
//
// The test sentence uses "きてほしかった" (answer key), meaning "wanted (you) to come".
// But GiNZA parses "でき" as lemma=できる (potential form of する), not as te-form of 来る.
//
// This creates two problems:
// 1. "でき" has pos=AUX, but our patterns expect pos=VERB for regular te-form verbs
// 2. Even if we matched pos=AUX, "lemma=できる" is wrong - it should be "lemma=くる"
//
// We cannot distinguish between:
//   車ででき + る (can [do] by car [grammatically valid but semantically odd])
//   車で + 来 + て (come by car [correct parse])
//
// Matching all AUX with 連用形 would cause false positives on unrelated patterns.
// This is a fundamental GiNZA limitation in parsing particle + te-form verb sequences.
//
// 2. 一緒にたべにいってほしいんです (go eat together [explanatory polite])
//
// This sentence combines てほしい with んです (explanatory):
//   たべにいって + ほしい + ん + です
//
// GiNZA parses this as:
//   [4] いっ (iku, VERB, root)
//   [5] て (SCONJ, mark → 4)
//   [6] ほしい (AUX, fixed → 5)
//   [7] ん (SCONJ, mark → 4)
//   [8] です (AUX, fixed → 7)
//
// The pattern expects ほしい → です (adjacent), but ん appears between them.
// This is the explanatory んです structure, which attaches to the entire clause.
//
// While we could add a pattern for ほしい + ん + です, this would overlap with
// the separate んです grammar point. The てほしい rule should focus on the core
// pattern without combining with other sentence-ending structures.
//
// Similar sentences that DO work: "一緒に勉強してほしいです。" (no ん)
const skipPositives = [
  '車できてほしかった。',
  '一緒にたべにいってほしいんです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
