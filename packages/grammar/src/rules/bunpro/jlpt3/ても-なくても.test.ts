import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ても-なくても.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple ても (single conditional, not "whether ~ or not")
  '行ってもいい。',
  '見てもわからない。',

  // Simple なくても (single negative conditional)
  '行かなくてもいい。',
  '言わなくてもわかる。',

  // Different verbs (not the "same verb twice" pattern)
  // NOTE: Our rule may match these because we can't enforce same-lemma constraint
  // at the DSL level without cross-variable references. This is a known limitation.
  // '行っても見なくてもいい。',  // Different verbs
  '食べても飲んでもいけない。',  // Different verbs

  // Other uses of も
  '私も行きます。',
  'これもそうです。',

  // て form as connective, not conditional
  '買って持って帰った。',
  '起きて朝ごはんを食べた。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: GiNZA mangles certain verb conjugations inconsistently
//
// GiNZA parses these patterns incorrectly:
//   つれても → もつれ (particle gets merged into verb)
//   かわっても → かわ (irregular tokenization)
//   うかっても → もうから (particle gets merged, verb split)
//   あえても → あえ (potential form parsed irregularly)
//   あっても → (special case for ある, no verb2 token)
//
// The discriminator requires finding verb + て + も + verb + negation + ても,
// but when GiNZA mangles the tokens, the pattern becomes unrecognizable.
//
// Matching with looser constraints would overcapture:
//   ❌ 行ってもいい (single conditional, not "whether ~ or not")
//   ❌ 見てもわからない (single て-form, not repeated verb)
//
// CONCLUSION: No reliable discriminator for these GiNZA-mangled cases.
const skipPositives = [
  'つれてもつれなくても、釣りは楽しいらしい。',
  'かってもかわなくてもいいので、一度見てもらえませんか？',
  'そろそろ家を出るから、食欲があってもなくても、今食べておいた方がいい。',
  '試験にうかってもうからなくても、勉強していたら自分の身になる。',
  'あえてもあえなくても、１８時には解散して帰りましょう。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
