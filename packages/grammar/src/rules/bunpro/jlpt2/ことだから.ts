import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('ことだから', (r) => {
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

      // Allow various dependency relationships
      b1.headChild(noun, no, 'compound');
      b1.headChild(noun, koto, 'compound');
      b1.headChild(koto, da, 'compound');
      b1.headChild(koto, kara, 'obl');

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

      // Verb is typically nominalized by こと
      b2.headChild(koto, verb, 'compound');
      b2.headChild(koto, da, 'compound');
      b2.headChild(koto, kara, 'obl');

      b2.captureSpan('ことだから', verb, kara);
    },

    // Pattern 3: な-adjective + な + こと + だから
    // Example: それは学術的なことだから、僕はうまく説明できない。
    (b3) => {
      const adj = b3.adj({}, 'adj');
      const na = b3.particle('な', 'na');
      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b3.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b3.particle('から', 'kara');

      b3.inOrder(adj, na, 1);
      b3.inOrder(na, koto, 1);
      b3.inOrder(koto, da, 1);
      b3.inOrder(da, kara, 1);

      b3.headChild(adj, na, 'compound');
      b3.headChild(adj, koto, 'compound');
      b3.headChild(koto, da, 'compound');
      b3.headChild(koto, kara, 'obl');

      b3.captureSpan('ことだから', adj, kara);
    },

    // Pattern 4: Looser dependency structure (catch-all)
    // For unexpected GiNZA parsings
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b4.particle('の', 'no');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b4.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b4.particle('から', 'kara');

      b4.inOrder(noun, no, 1);
      b4.inOrder(no, koto, 1);
      b4.inOrder(koto, da, 1);
      b4.inOrder(da, kara, 1);

      b4.captureSpan('ことだから', noun, kara);
    },

    // Pattern 5: Verb + こと + だから (looser dependencies)
    (b5) => {
      const verb = b5.verb({}, 'verb');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b5.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b5.particle('から', 'kara');

      b5.inOrder(verb, koto, 1);
      b5.inOrder(koto, da, 1);
      b5.inOrder(da, kara, 1);

      b5.captureSpan('ことだから', verb, kara);
    },

    // Pattern 6: な-adjective + な + こと + だから (looser dependencies)
    (b6) => {
      const adj = b6.adj({}, 'adj');
      const na = b6.particle('な', 'na');
      const koto = b6.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b6.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b6.particle('から', 'kara');

      b6.inOrder(adj, na, 1);
      b6.inOrder(na, koto, 1);
      b6.inOrder(koto, da, 1);
      b6.inOrder(da, kara, 1);

      b6.captureSpan('ことだから', adj, kara);
    },

    // Pattern 7: Polite form: ...ことな + から (copula is な instead of だ)
    // Example: お客様のプライバシーに関わることなので、これ以上詳しいことは言えません。
    (b7) => {
      const noun = b7.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b7.particle('の', 'no');
      const koto = b7.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na = b7.aux({ text: 'な', lemma: 'だ' }, 'na');
      const kara = b7.particle('から', 'kara');

      b7.inOrder(noun, no, 1);
      b7.inOrder(no, koto, 1);
      b7.inOrder(koto, na, 1);
      b7.inOrder(na, kara, 1);

      b7.captureSpan('ことだから', noun, kara);
    },

    // Pattern 8: Verb + こと + な + から (polite verb form)
    (b8) => {
      const verb = b8.verb({}, 'verb');
      const koto = b8.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na = b8.aux({ text: 'な', lemma: 'だ' }, 'na');
      const kara = b8.particle('から', 'kara');

      b8.inOrder(verb, koto, 1);
      b8.inOrder(koto, na, 1);
      b8.inOrder(na, kara, 1);

      b8.captureSpan('ことだから', verb, kara);
    },

    // Pattern 9: ...とのこと + だから (quoted clause + こと)
    // Example: お相手が「感激した」とのことだから、結果的にはあれで良かったと思います。
    (b9) => {
      const to = b9.particle('と', 'to');
      const no = b9.particle('の', 'no');
      const koto = b9.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b9.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b9.particle('から', 'kara');

      b9.inOrder(to, no, 1);
      b9.inOrder(no, koto, 1);
      b9.inOrder(koto, da, 1);
      b9.inOrder(da, kara, 1);

      // Capture from との (or just noun + の)
      b9.captureSpan('ことだから', to, kara);
    },

    // Pattern 10: Any token + の + こと + だから (very loose, for quoted patterns)
    (b10) => {
      const prev = b10.tok({}, 'prev');
      const no = b10.particle('の', 'no');
      const koto = b10.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const da = b10.aux({ text: 'だ', lemma: 'だ' }, 'da');
      const kara = b10.particle('から', 'kara');

      b10.inOrder(prev, no, 1);
      b10.inOrder(no, koto, 1);
      b10.inOrder(koto, da, 1);
      b10.inOrder(da, kara, 1);

      b10.captureSpan('ことだから', prev, kara);
    },

    // Pattern 11: な-adjective + な + こと + な + から (polite adjective form)
    (b11) => {
      const adj = b11.adj({}, 'adj');
      const na1 = b11.particle('な', 'na1');
      const koto = b11.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const na2 = b11.aux({ text: 'な', lemma: 'だ' }, 'na2');
      const kara = b11.particle('から', 'kara');

      b11.inOrder(adj, na1, 1);
      b11.inOrder(na1, koto, 1);
      b11.inOrder(koto, na2, 1);
      b11.inOrder(na2, kara, 1);

      b11.captureSpan('ことだから', adj, kara);
    }
  );
});
