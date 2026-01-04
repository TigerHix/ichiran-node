import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: そう - Looks like / Seems (appearance-based conjecture)
 *
 * Matches verb/adjective stem + そう to express "looks like" based on visual appearance.
 *
 * Structures:
 * - Verb［stem］+ そう (looks like it will...)
 * - ［い］Adjective［-い］+ そう (looks...)
 * - ［な］Adjective + そう (seems...)
 *
 * Examples:
 * - 雪が降りそうです (It looks like it's going to snow)
 * - この教科書はとてもむずかしそうです (This textbook looks very difficult)
 * - おいしそうです (It looks delicious)
 * - かんたんそうだ (It looks easy)
 * - たべやすそう (Looks easy to eat)
 * - 丁寧そうだ (Seems polite)
 *
 * Key discriminators:
 * - そう is an AUX (sometimes ADV/VERB) with lemma=そう and dep=aux
 * - It attaches with aux dependency to the stem
 * - For negative forms: なさそう (not なそう)
 *
 * GiNZA parse structure:
 * - 降りそうです: 降り(VERB, 連用形-一般) + そう(AUX, dep=aux, head=降り) + です(AUX)
 * - むずかしそう: むずかし(ADJ, 語幹-一般) + そう(AUX, dep=aux, head=むずかし)
 * - 丁寧そう: 丁寧(ADJ) + そう(AUX, dep=aux, head=丁寧)
 * - かんたんそうだ: かんたん(ADV) + そう(ADV, dep=aux, head=かんたん)
 *
 * Note: This is the "looks like" そう, NOT そうだ (reported speech/hearsay).
 * The reported speech version attaches to plain forms (降るそうだ),
 * while this version attaches to stems (降りそうだ).
 */
export default linguisticRule('そう', (r) => {
  r.either(
    // Branch 1: Verb stem (連用形-一般) + そう
    // Example: 降りそう, なりそう, できそう, ありそう, 正しそう
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    },

    // Branch 2: Verb stem (other inflection forms) + そう
    // Example: 走っていそう (iru in 連用形), なさそう (negative stem)
    (b) => {
      const stem = b.verb({
        inflectionFormOneOf: [
          '連用形-促音便',
          '未然形-一般',
        ],
      }, 'stem');
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    },

    // Branch 3: I-adjective stem (語幹-一般) + そう
    // Example: むずかしそう, おいしそう
    // Stem can be VERB or ADJ with tag=形容詞-一般
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        tag: '形容詞-一般',
        inflectionForm: '語幹-一般',
      }, 'stem');
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    },

    // Branch 4: Negative i-adjective stem (語幹-サ) + そう
    // Example: たのしくなさそう (parsed as separate tokens)
    (b) => {
      const naiStem = b.tok({
        lemma: 'ない',
        inflectionForm: '語幹-サ',
      }, 'naiStem');
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(naiStem, sou);
      b.captureSpan('そう', naiStem, sou);
    },

    // Branch 5: Na-adjective (ADJ) + そう
    // Example: 丁寧そう
    (b) => {
      const stem = b.adj({
        tag: '形状詞-一般',
      }, 'stem');
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    },

    // Branch 6: Na-adjective (ADV) + そう
    // Example: かんたんそうだ (かんたん is ADV, tag=形状詞-一般)
    // そう is ADV with tag=形状詞-助動詞語幹 (not hearsay which is 名詞-助動詞語幹)
    (b) => {
      const stem = b.adv({
        tag: '形状詞-一般',
      }, 'stem');
      const sou = b.aux({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    },

    // Branch 7: Negative verb/adj + な + そう
    // Example: はいらなそうです (はいら + な + そう)
    // The stem before な is in 未然形
    // GiNZA sometimes parses the stem as ADJ (for verbs like はいる)
    (b) => {
      const stem = b.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '未然形-一般',
      }, 'stem');
      const na = b.aux({
        text: 'な',
        lemma: 'ない',
      }, 'na');
      b.auxOf(stem, na);
      const sou = b.aux({
        lemma: 'そう',
      }, 'sou');
      b.auxOf(stem, sou);
      b.captureSpan('そう', stem, sou);
    }
  );
});
