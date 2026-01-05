import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ところを見ると (tokoro o miru to) - "judging from, seeing that"
 *
 * A phrase used when the speaker judges (B) after looking at the situation
 * or place of (A). The speaker observes (A) and makes a subjective judgment
 * about it.
 *
 * Structure: [Verb in any form] + ところ + を + 見る + と
 *
 * Examples:
 * - 彼がニコニコしているところを見ると、テストに合格したに違いない。
 *   (Seeing that he is smiling, I have no doubt that he passed the test.)
 * - 漢字を綺麗に書けるところを見ると、彼は長い間漢字を勉強しているのかもしれない。
 *   (Judging from the fact that he can write kanji very nicely, he probably has been studying kanji for a long time.)
 * - 香水をつけているところを見ると、お出掛けですね？
 *   (Seeing that you are wearing perfume, are you going out?)
 * - 焦がしたところを見ると、彼女は料理が苦手なのでしょう。
 *   (Seeing that she burned it, she is not that great at cooking, is she?)
 *
 * Key discriminators:
 * - Follows a verb phrase (any form: dictionary, te-form, past, etc.)
 * - ところ is a noun (NOUN) meaning "place, situation, aspect"
 * - を is the object marker particle (ADP)
 * - 見る is the verb "to see/look" in dictionary form (lemma=見る)
 * - と is the conditional particle (ADP/SCONJ) meaning "if/when"
 * - Expresses subjective judgment based on observation
 * - Usually followed by conjectural expressions (だろう, かも, でしょう, etc.)
 *
 * GiNZA parse structure:
 * - [Verb/Phrase] (any form) + ところ(NOUN) + を(ADP) + 見る(VERB) + と(ADP/SCONJ)
 * - Various dependency relations depending on the verb form
 *
 * Different from:
 * - からして (karashite) - follows nouns, not verbs
 * - からすると (karasuruto) - more objective judgment, follows nouns
 * - にしては (nishite) - "considering, for" (different nuance)
 * - ところを (tokoro o) - "just as/when" (different meaning)
 * - Simple を見て (o mite) - "looking at" without conditional
 */
export default linguisticRule('ところを見ると', (r) => {
  r.either(
    // Branch 1: Verb phrase + ところ(lemma) + を + 見る(lemma) + と
    // Most common pattern with lemma for both ところ and 見る
    (b1) => {
      const verbPhrase = b1.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'verbPhrase');
      const tokoro = b1.noun({ lemma: '所' }, 'tokoro');
      const wo = b1.particle('を', 'wo');
      const miru = b1.verb({ lemma: '見る' }, 'miru');
      const to = b1.particle('と', 'to');

      b1.inOrder(verbPhrase, tokoro, 5);
      b1.inOrder(tokoro, wo, 1);
      b1.inOrder(wo, miru, 1);
      b1.inOrder(miru, to, 1);

      b1.captureSpan('ところを見ると', verbPhrase, to);
    },

    // Branch 2: Verb phrase + ところ(text=所) + を + 見る(lemma) + と
    // Alternative with text constraint for ところ
    (b2) => {
      const verbPhrase = b2.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'verbPhrase');
      const tokoro = b2.noun({ text: '所' }, 'tokoro');
      const wo = b2.particle('を', 'wo');
      const miru = b2.verb({ lemma: '見る' }, 'miru');
      const to = b2.particle('と', 'to');

      b2.inOrder(verbPhrase, tokoro, 5);
      b2.inOrder(tokoro, wo, 1);
      b2.inOrder(wo, miru, 1);
      b2.inOrder(miru, to, 1);

      b2.captureSpan('ところを見ると', verbPhrase, to);
    },

    // Branch 3: Verb phrase + ところ(text=ところ) + を + 見る(lemma) + と
    // Hiragana form for ところ
    (b3) => {
      const verbPhrase = b3.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'verbPhrase');
      const tokoro = b3.noun({ text: 'ところ' }, 'tokoro');
      const wo = b3.particle('を', 'wo');
      const miru = b3.verb({ lemma: '見る' }, 'miru');
      const to = b3.particle('と', 'to');

      b3.inOrder(verbPhrase, tokoro, 5);
      b3.inOrder(tokoro, wo, 1);
      b3.inOrder(wo, miru, 1);
      b3.inOrder(miru, to, 1);

      b3.captureSpan('ところを見ると', verbPhrase, to);
    },

    // Branch 4: Noun + ところ + を + 見る + と
    // For cases where the preceding element is a noun
    // Example: 心当たりがないところを見ると...
    (b4) => {
      const nounPhrase = b4.noun({}, 'nounPhrase');
      const tokoro = b4.noun({ lemmaOneOf: ['所', 'ところ'] }, 'tokoro');
      const wo = b4.particle('を', 'wo');
      const miru = b4.verb({ lemma: '見る' }, 'miru');
      const to = b4.particle('と', 'to');

      b4.inOrder(nounPhrase, tokoro, 5);
      b4.inOrder(tokoro, wo, 1);
      b4.inOrder(wo, miru, 1);
      b4.inOrder(miru, to, 1);

      b4.captureSpan('ところを見ると', nounPhrase, to);
    },

    // Branch 5: Loose pattern - catch-all for unexpected parsings
    (b5) => {
      const phrase = b5.tok({ posOneOf: ['VERB', 'AUX', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'phrase');
      const tokoro = b5.tok({ textOneOf: ['所', 'ところ'] }, 'tokoro');
      const wo = b5.particle('を', 'wo');
      const miru = b5.verb({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const to = b5.particle('と', 'to');

      b5.inOrder(phrase, tokoro, 10);
      b5.inOrder(tokoro, wo, 1);
      b5.inOrder(wo, miru, 1);
      b5.inOrder(miru, to, 1);

      b5.captureSpan('ところを見ると', phrase, to);
    }
  );
});
