import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './adjective-て-b.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb + て (not adjective)
  '食べておいしい。',
  '走って疲れた。',
  '読んで分かった。',
  // Instrumental で (not conjunction)
  '鉛筆で書く。',
  '電車で来た。',
  'バスで行く。',
  // Locative で (at/in place)
  '教室で勉強する。',
  '家で寝る。',
  '公園で遊ぶ。',
  // ではない negation (different grammar)
  '学生ではない。',
  '静かではない。',
  // Conjunction で between sentences (not listing)
  '雨が降った。で、試合は中止だ。',
  // て as means/manner for verbs
  '大きく書いてください。',
  '小さく切る。',
  // Just adjective without て/で
  '部屋が広い。',
  'ここは静かだ。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Noun + で conjunction (copula te-form)
//
// GiNZA parses this pattern inconsistently:
//   漫画家で  → lemma=だ (copula) ✓ WORKS
//   映画監督で → lemma=で (particle) ✗ INDISTINGUISHABLE
//   数学者で  → lemma=で (particle) ✗ INDISTINGUISHABLE
//
// The discriminator `lemma=だ` identifies copula で vs locative で.
// But GiNZA only assigns lemma=だ to SOME Noun+で conjunctions.
// When lemma=で, it's identical to locative で (東京で働く).
//
// Matching all で with lemma=で would overcapture:
//   ❌ 東京で働く (locative: work IN Tokyo)
//   ❌ 鉛筆で書く (instrumental: write WITH pencil)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'クエンティンさんは映画監督で俳優です。',
  '彼はアメリカの数学者で大学教授だった。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});

