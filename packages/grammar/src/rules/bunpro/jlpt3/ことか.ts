import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことか (koto ka) - "How...!", "What...!", "God knows..."
 *
 * Matches exclamatory/rhetorical question pattern expressing strong emotion
 * about the extent or degree of something.
 *
 * Structure:
 * - (Question word: どれだけ/どんなに/何回/何と/など) + predicate + ことか
 * - Predicate can be: verb, i-adjective, na-adjective+な, noun+である
 *
 * Examples:
 * - あの人のコンサートには何回いったことか (God knows how many times...)
 * - どんなに可愛いことか (How cute [she is]!)
 * - どれだけ心配であることか (How worried [I am]!)
 * - 何たることか (What a...!)
 *
 * GiNZA parse structure:
 * - GiNZA may tokenize ことか as a single NOUN token
 * - Or as separate tokens: こと(NOUN) + か(PARTICLE/ADP)
 * - The key is that this appears at the end of an exclamatory clause
 */
export default bunproLinguisticRule('ことか', (r) => {
  r.either(
    // Branch 1: ことか as single token (GiNZA sometimes fuses these)
    (b) => {
      const kotoka = b.noun({ text: 'ことか' }, 'kotoka');
      b.capture(kotoka);
    },
    // Branch 2: 事か as single token (kanji form)
    (b) => {
      const kotoka = b.noun({ text: '事か' }, 'kotoka');
      b.capture(kotoka);
    },
    // Branch 3: こと + か as separate tokens
    (b) => {
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const ka = b.tok({ text: 'か' }, 'ka');

      // か must immediately follow こと (adjacent tokens, max distance 1)
      b.inOrder(koto, ka, 1);

      // Exclude: こと + は + ... + か (topic marker pattern)
      b.not((bb) => {
        const wa = bb.particle('は');
        bb.inOrder(koto, wa, 1);
        bb.inOrder(wa, ka);
      });

      // Exclude: こと + で + す + か (copula desu pattern)
      b.not((bb) => {
        const desu = bb.tok({ lemma: 'だ' });
        bb.inOrder(koto, desu);
        bb.inOrder(desu, ka);
      });

      // Exclude: こと + が + ... + か (subject marker pattern)
      b.not((bb) => {
        const ga = bb.particle('が');
        bb.inOrder(koto, ga, 1);
        bb.inOrder(ga, ka);
      });

      // Exclude: こと + を + ... + か (object marker pattern)
      b.not((bb) => {
        const wo = bb.particle('を');
        bb.inOrder(koto, wo, 1);
        bb.inOrder(wo, ka);
      });

      b.captureSpan('ことか', koto, ka);
    }
  );
});
