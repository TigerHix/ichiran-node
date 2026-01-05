import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('-よう--う-まい-が-と', (r) => {
  // Pattern: volitional (よう/う) + が/と + negative volitional (まい) + が/と
  // e.g., 行こうが行くまいが, 読もうと読むまいと

  r.either(
    // Pattern 1: Simple verb volitional + が/と + same verb + まい + が/と
    // e.g., 行こうが行くまいが, 雨が降ろうが降るまいが
    (b) => {
      const vol1 = b.verb({ inflectionForm: '意志推量形' }, 'vol1');
      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(vol1, particle1, 1);

      const verb2 = b.verb({}, 'verb2');
      b.inOrder(particle1, verb2);

      const mai = b.aux({ text: 'まい' }, 'mai');
      b.auxOf(verb2, mai);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(mai, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', vol1, particle2);
    },

    // Pattern 2a: Suru-verb volitional (しよう = し + よう) + が/と + same suru-verb + するまい + が/と
    // e.g., 出席しようが出席するまいが
    (b) => {
      // Match: verb stem + し (optional) + よう + が/と + same verb + するまい + が/と
      // We use inOrder without distance constraints to allow for different GiNZA parsings
      const verbStem1 = b.verb({}, 'verbStem1');
      const you = b.tok({ text: 'よう' }, 'you');
      b.inOrder(verbStem1, you);

      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(you, particle1, 1);

      const verbStem2 = b.verb({}, 'verbStem2');
      b.inOrder(particle1, verbStem2);

      const suru = b.tok({ text: 'する' }, 'suru');
      b.inOrder(verbStem2, suru, 1);

      const mai = b.tok({ text: 'まい' }, 'mai');
      b.inOrder(suru, mai, 1);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(mai, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', verbStem1, particle2);
    },

    // Pattern 2b: Suru-verb volitional (しよう = し + よう) + が/と + same suru-verb + しまい + が/と
    // e.g., 離婚しようが離婚しまいが
    (b) => {
      const verbStem1 = b.verb({}, 'verbStem1');
      const you = b.tok({ text: 'よう' }, 'you');
      b.inOrder(verbStem1, you);

      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(you, particle1, 1);

      const verbStem2 = b.verb({}, 'verbStem2');
      b.inOrder(particle1, verbStem2);

      const shi = b.tok({ text: 'し' }, 'shi');
      b.inOrder(verbStem2, shi, 1);

      const mai = b.tok({ text: 'まい' }, 'mai');
      b.inOrder(shi, mai, 1);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(mai, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', verbStem1, particle2);
    },

    // Pattern 3: Te + iru volitional + が/と + te + iru + まい + が/と
    // e.g., 生きていようが生きていまいが
    (b) => {
      const verb1 = b.verb({ inflectionForm: '連用形-一般' }, 'verb1');
      const te1 = b.aux({ lemma: 'て' }, 'te1');
      b.auxOf(verb1, te1);

      const iru1 = b.aux({ lemma: 'いる', inflectionForm: '意志推量形' }, 'iru1');
      b.auxOf(te1, iru1);

      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(iru1, particle1, 1);

      const verb2 = b.verb({ inflectionForm: '連用形-一般' }, 'verb2');
      const te2 = b.aux({ lemma: 'て' }, 'te2');
      b.auxOf(verb2, te2);

      b.inOrder(particle1, verb2);

      const iru2 = b.aux({ lemma: 'いる' }, 'iru2');
      b.auxOf(te2, iru2);

      const mai = b.aux({ text: 'まい' }, 'mai');
      b.auxOf(iru2, mai);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(mai, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', verb1, particle2);
    },

    // Pattern 4: Te + iru volitional + が/と + iru + まい + が/と (shortened form)
    // e.g., 晴れていようがいまいが (repeats "晴れて" only once in speech)
    (b) => {
      const verbTe = b.verb({ inflectionForm: '連用形-一般' }, 'verbTe');
      const te = b.aux({ lemma: 'て' }, 'te');
      b.auxOf(verbTe, te);

      const iru1 = b.aux({ lemma: 'いる', inflectionForm: '意志推量形' }, 'iru1');
      b.auxOf(te, iru1);

      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(iru1, particle1, 1);

      const iru2 = b.aux({ lemma: 'いる' }, 'iru2');
      b.inOrder(particle1, iru2);

      const mai = b.aux({ text: 'まい' }, 'mai');
      b.auxOf(iru2, mai);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(mai, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', verbTe, particle2);
    },

    // Pattern 5: Noun/Adj/Verb in volitional form + が/と + same in volitional + が/と
    // For nouns and adjectives, まい is not used, so we match volitional + volitional
    // e.g., 犬であろうが猫であろうが, 楽しかろうが楽しくなかろうが, 安全だろうが安全でなかろうが
    (b) => {
      const word1 = b.tok({ inflectionForm: '意志推量形' }, 'word1');
      const particle1 = b.tok({ textOneOf: ['が', 'と'] }, 'particle1');
      b.inOrder(word1, particle1, 1);

      const word2 = b.tok({ inflectionForm: '意志推量形' }, 'word2');
      b.inOrder(particle1, word2);

      const particle2 = b.tok({ textOneOf: ['が', 'と'] }, 'particle2');
      b.inOrder(word2, particle2, 1);

      b.captureSpan('-よう--う-まい-が-と', word1, particle2);
    }
  );
});
