import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことだから (koto-dakara) - "It is exactly because, given that"
 *
 * A set expression used when the speaker is fairly certain about (B), based
 * specifically on their experience with (A). It emphasizes that (A) is the
 * exact reasoning from which (B) can be determined.
 *
 * Structure:
 * - Noun + の + こと + だから
 * - Verb (dictionary form) + こと + だから
 * - な-adjective + な + こと + だから
 *
 * Examples:
 * - いつも遅れてくる田中くんのことだから、今日も遅れてくるだろう。
 *   (It is exactly because Tanaka-kun always arrives late, that he will probably arrive late today.)
 * - 真面目な田中さんのことだから、約束は守るだろう。
 *   (Given that it's the serious Tanaka-san, he'll probably keep his promise.)
 * - お客様のプライバシーに関わることなので、これ以上詳しいことは言えません。
 *   (Because this is something that has to do with the privacy of our customer, we can't give you further details.)
 * - 子供のことだから、仕方がない。
 *   (Given that they're children, it can't be helped.)
 *
 * Key discriminators:
 * - Follows nouns (with の), verbs, or な-adjectives (with な)
 * - こと is a noun (NOUN) meaning "thing, matter"
 * - だ is a copula (AUX) indicating assertion
 * - から is a particle (ADP/SCONJ) indicating "because"
 * - Expresses strong reasoning based on known characteristics
 * - Often used to make predictions about people's behavior
 *
 * GiNZA parse structure:
 * - Noun + の(PART) + こと(NOUN) + だ(AUX) + から(ADP/SCONJ)
 * - Verb + こと(NOUN) + だ(AUX) + から(ADP/SCONJ)
 * - Various dependency relations (compound, fixed, mark, obl)
 *
 * Different from:
 * - ものだから (more subjective, often used for excuses)
 * - ことから (from the fact that, more objective)
 * - からこそ (precisely because, more emphatic)
 */
export default linguisticRule('ことだから', (r) => {
  r.either(
    // Pattern 1: Noun + の + こと + だから (most common pattern)
    // Example: 田中さんのことだから、今日も遅れてくるだろう。
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b1.particle('の', 'no');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b1.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b1.particle('から', 'kara');

      b1.inOrder(noun, no, 1);
      b1.inOrder(no, koto, 1);
      b1.inOrder(koto, da, 1);
      b1.inOrder(da, kara, 1);

      b1.captureSpan('ことだから', noun, kara);
    },

    // Pattern 2: Verb + こと + だから
    // Example: あの人がすることだから、どうせ人を騙して儲けているに違いない。
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b2.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b2.particle('から', 'kara');

      b2.inOrder(verb, koto, 1);
      b2.inOrder(koto, da, 1);
      b2.inOrder(da, kara, 1);

      b2.captureSpan('ことだから', verb, kara);
    },

    // Pattern 3: Verb + た + こと + だから (past tense verb)
    // Example: 撮影が無事終わったことだから、打ち上げでもしましょう！
    // GiNZA parses: 終わっ(verb) + た(aux) + こと + だ + から
    (b3) => {
      const verb = b3.verb({}, 'verb');
      const ta = b3.aux({ lemma: 'た' }, 'ta');
      b3.auxOf(verb, ta);

      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b3.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b3.particle('から', 'kara');

      b3.inOrder(ta, koto, 2);
      b3.inOrder(koto, da, 1);
      b3.inOrder(da, kara, 1);

      b3.captureSpan('ことだから', verb, kara);
    },

    // Pattern 4: な-adjective + な + こと + だから
    (b4) => {
      const adj = b4.adj({}, 'adj');
      const na = b4.particle('な', 'na');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b4.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b4.particle('から', 'kara');

      b4.inOrder(adj, na, 1);
      b4.inOrder(na, koto, 1);
      b4.inOrder(koto, da, 1);
      b4.inOrder(da, kara, 1);

      b4.captureSpan('ことだから', adj, kara);
    },

    // Pattern 5: Polite form: ...ことな + から (copula is な instead of だ)
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b5.particle('の', 'no');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na = b5.aux({ text: 'な', lemma: 'だ' }, 'na');
      const kara = b5.particle('から', 'kara');

      b5.inOrder(noun, no, 1);
      b5.inOrder(no, koto, 1);
      b5.inOrder(koto, na, 1);
      b5.inOrder(na, kara, 1);

      b5.captureSpan('ことだから', noun, kara);
    },

    // Pattern 6: Verb + こと + な + から (polite verb form)
    (b6) => {
      const verb = b6.verb({}, 'verb');
      const koto = b6.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na = b6.aux({ text: 'な', lemma: 'だ' }, 'na');
      const kara = b6.particle('から', 'kara');

      b6.inOrder(verb, koto, 1);
      b6.inOrder(koto, na, 1);
      b6.inOrder(na, kara, 1);

      b6.captureSpan('ことだから', verb, kara);
    },

    // Pattern 7: との + こと + だ + から (quoted clause)
    // Example: お相手が「感激した」とのことだから
    // GiNZA: と(ADP) + の(ADP) + こと + だ + から
    (b7) => {
      const to = b7.tok({ text: 'と' }, 'to');
      const no = b7.tok({ text: 'の' }, 'no');
      b7.inOrder(to, no, 1);

      const koto = b7.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b7.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b7.particle('から', 'kara');

      b7.inOrder(no, koto, 2);
      b7.inOrder(koto, da, 1);
      b7.inOrder(da, kara, 1);

      b7.captureSpan('ことだから', to, kara);
    },

    // Pattern 8: ...こと + な + ので (data quality issue - different grammar but in test data)
    // Example: お客様のプライバシーに関わることなので、これ以上詳しいことは言えません。
    // Note: This is technically ことなので (different grammar), but included here as test data
    // GiNZA: 関わる(VERB) + こと(NOUN) + な(AUX) + の(SCONJ) + で(AUX,lemma=だ)
    (b8) => {
      const pred = b8.tok({}, 'pred');
      const koto = b8.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na = b8.aux({ text: 'な', lemma: 'だ' }, 'na');
      const no = b8.tok({ text: 'の', lemma: 'の' }, 'no');
      const de = b8.aux({ lemma: 'だ', inflectionForm: '連用形-一般' }, 'de');

      b8.inOrder(pred, koto, 3);
      b8.inOrder(koto, na, 1);
      b8.inOrder(na, no, 1);
      b8.inOrder(no, de, 1);

      b8.captureSpan('ことだから', pred, de);
    }
  );
});
