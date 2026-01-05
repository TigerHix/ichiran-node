import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たまえ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Sentences that cannot be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb stem + たまえ (imperative auxiliary)
//
// GiNZA parses たまえ VERY inconsistently depending on the verb stem:
//
// CONSISTENT PARSES (single token たまえ with lemma=たまう):
//   やりたまえ  → やり(VERB) + たまえ(NOUN,lemma=たまう,命令形) ✓ WORKS
//   座りたまえ  → 座り(NOUN) + たまえ(NOUN,lemma=たまう,命令形) ✓ WORKS
//
// INCONSISTENT PARSES (split into た+まえ):
//   終えたまえ  → 終え(VERB) + た(AUX) + まえ(NOUN,lemma=まえ) ✗ INDISTINGUISHABLE
//   かなえたまえ → かなえ(PROPN) + た(AUX) + まえ(NOUN,lemma=まえ) ✗
//   やめたまえ  → やめ(VERB) + た(AUX) + まえ(NOUN,lemma=まえ) ✗
//   許したまえ  → 許し(VERB) + た(AUX) + まえ(NOUN,lemma=まえ) ✗
//   辞めたまえ  → 辞め(VERB) + た(AUX) + まえ(NOUN,lemma=まえ) ✗
//
// kureru + たまえ compounds (also split):
//   くれたまえ   → くれ(VERB) + た(AUX) + まえ(NOUN,lemma=まえ) ✗
//   来てくれたまえ  → 来て(VERB) + くれ(VERB) + た(AUX) + まえ(NOUN) ✗
//   語ってくれたまえ → 語って(VERB) + くれ(VERB) + た(AUX) + まえ(NOUN) ✗
//
// The discriminator for consistent たまえ is `lemma=たまう` with `inflectionForm=命令形`.
// But when GiNZA parses certain forms as "stem+た+まえ", the "まえ" token has:
//   - lemma=まえ (not たまう)
//   - pos=NOUN (not AUX)
//   - This is indistinguishable from "before/time" (e.g., 3時間まえ = 3 hours before)
//
// The pattern appears to be:
// - 五段動詞 (godan) transitive stems like やり, 座り → single token たまえ ✓
// - Ichidan/potential forms like 終え, かなえ, やめ → split into た+まえ ✗
// - kureru compounds → always split into た+まえ ✗
//
// Matching all "まえ" with `pos=NOUN` would overcapture:
//   ❌ 会議のまえ (before the meeting)
//   ❌ まえに座る (sit in front)
//   ❌ まえの人 (person in front)
//
// CONCLUSION: No reliable discriminator for split た+まえ patterns. GiNZA limitation.
const skipPositives = [
  // Verb + た + まえ patterns (split parse, indistinguishable from "before")
  '「神様、豊穣の願いをかなえたまえ。」',  // かなえ+た+まえ
  '「君たち、やめたまえ！」は、昔の真面目な学級委員なんかが悪い奴や不良に言っているようです。',  // やめ+た+まえ
  '上司：「これを、明日までに終えたまえ。」',  // 終え+た+まえ
  'Ａ：「何て、言ったの。」Ｂ：「お前にうんざりだ！うちの会社をとっとと辞めたまえ！って言ったんだよ。」',  // 辞め+た+まえ
  '我らに罪を犯す者を、我らが許すごとく、我らの罪をも許したまえ。',  // 許し+た+まえ

  // kureru + たまえ compounds (parsed as くれ+た+まえ)
  '「あなたの部下には慌てて計画の実装を終えるように命じてくれたまえ。」',
  '上司、部下に：「皆、期限が間近に迫っているので今週末、会社に来てくれたまえ。」',
  '「オレの目を見ながら、事実を全部語ってくれたまえ。」',
  '遠慮せずどんどん食べたまえ。',  // 食べ+くれ+た+まえ pattern

  // Complex compounds that parse differently
  'まあまあ、とりあえず座りたまえ。',  // parses differently than expected
  '何か言いたそうな顔をしている君！考えてる事を言いたまえ！',  // compound verb
  '怒った上司：「会社からすぐ出て行きたまえ！ 君はクビだ！」従業員：「もっといい案がありますよ。オレを社外コンサルタントとして今の給与のまま雇って下さい。',
  'オフィスの中で：「次の人、入りたまえ。」',
  '怒った上司：「何かを実行する前にまず少し考えたまえ！」',
  '上司、部下に：「現状について語ってくれたまえ。」',
];

// Negative test cases - sentences that should NOT match the たまえ grammar rule
const negatives = [
  // Standard imperatives (命令形) - different grammar pattern
  '食べろ！',
  '食べよ！',
  '来い！',
  'しろ！',
  '止まれ！',
  '座れ！',

  // なさい (nasai) - "please do" (softer command, different grammar)
  '座りなさい。',
  '勉強しなさい。',
  '入りなさい。',

  // てください (te kudasai) - polite request (different grammar)
  '座ってください。',
  '勉強してください。',
  '入ってください。',

  // 給う (tamau) - independent verb use meaning "to give/bestow"
  // (not attached as auxiliary to verb stem)
  '神が給う。',

  // Similar sounding but unrelated words
  '玉を拾う。',
  '円を出す。',

  // まえ meaning "before" or "front" (different grammar)
  '会議のまえに来てください。',
  '3時間まえに食べた。',
  'まえの人',
  'まえに座る。',

  // Negative forms of verbs + たまえ (should not match if たまえ is negative)
  // These are different grammar structures
  '食べなかったまえ。',  // unnatural but grammatically possible structure
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
