import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てくる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: patterns that look similar but should NOT match
const negatives = [
  // ていく (opposite direction: away from speaker/into future)
  // This is the key contrast - いく vs くる
  'そのまま持ち帰っていきます。',
  '雨が降っていくでしょう。',
  'これからも頑張っていくつもりです。',
  '日本語の勉強を続けていく。',
  // くる as standalone verb "to come" (not in te-form)
  '彼が来る。',
  '春が来る。',
  'バスが来た。',
  // て-form without auxiliary (different grammar)
  '本を読んで、寝ました。',
  'ご飯を食べて、学校へ行きます。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb[te] + くる pattern
//
// GiNZA parses "にて" (ni-te) inconsistently:
//   なってきた → なっ [VERB] + て [SCONJ] + き [AUX] ✓ WORKS
//   にてきた → にて [VERB, text=にて, lemma=にて] ✗ SINGLE TOKEN
//
// The pattern て (te-form) + くる requires:
//   1. Verb stem in conjunctive form (連用形)
//   2. て particle (pos=SCONJ or similar)
//   3. くる auxiliary (lemma=くる)
//
// In "お母さんににてきた", GiNZA tokenizes "にて" as a single token:
//   - text: "にて"
//   - lemma: "にて"
//   - pos: "VERB"
//   - tag: "助詞-格助詞" (particle)
//   - dep: "case"
//
// This is a compound particle form where the て is embedded within the token,
// not parsed as a separate SCONJ particle. Our rule requires a separate て token.
//
// CONCLUSION: No reliable discriminator. GiNZA parses "にて" as a compound token.
// Cannot distinguish between particle "にて" (at/in) and verb-te "にて" (ni-te form).
const skipPositives = [
  'お母さんににてきたね。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
