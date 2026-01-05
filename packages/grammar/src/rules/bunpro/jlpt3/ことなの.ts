import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことなの (koto nano) - "the thing is/it is that"
 *
 * Matches explanatory construction using こと + なの to clarify meaning or definition.
 *
 * Structure:
 * - Verb/い-adj (attributive) + ことなの / ことなん
 * - Na-adj + な + ことなの / ことなん
 * - Noun + の + ことなの / ことなん
 * - (Optional) という + ことなの / ことなん
 *
 * Examples:
 * - 漫画家というのは漫画を描く人のことなのだ (Manga artists are people who draw manga)
 * - 雨が上がるというのは雨が止むことなのです (Rain rising means rain stops)
 * - 頭が切れるというのは頭がいい事なのだ (To be sharp means to be smart)
 * - 幽霊船というのは生きた乗組員のいない船舶の事なのである (A ghost ship is a vessel with no living crew)
 * - そんなに怒ることなのですか (Is this something to get so angry about?)
 *
 * GiNZA parse structure:
 * - こと: NOUN
 * - な: AUX or copula form
 * - の: AUX or PART with explanatory function
 * - The pattern ends with だ, です, etc. (copula)
 *
 * The key is matching こと followed by なの or なん (contraction),
 * which creates the explanatory "it means that / the thing is" construction.
 */
export default bunproLinguisticRule('ことなの', (r) => {
  r.either(
    // Branch 1: ... + こと + なの (explanatory)
    (b1) => {
      const koto = b1.noun({ lemma: 'こと' }, 'koto');

      // なの can be:
      // - な (AUX/copula) + の (AUX explanatory)
      // - or contracted to なん (single token)
      const na = b1.tok({ text: 'な' }, 'na');
      const no = b1.tok({ text: 'の' }, 'no');
      b1.inOrder(koto, na, 1);
      b1.inOrder(na, no, 1);

      // Optional: copula (だ, です, etc.) follows
      b1.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(no, copulaTok, 2);
      });

      b1.captureSpan('ことなの', koto, no);
    },
    // Branch 2: ... + こと + なん (contracted form)
    (b2) => {
      const koto = b2.noun({ lemma: 'こと' }, 'koto');
      const nan = b2.tok({ text: 'なん' }, 'nan');

      b2.inOrder(koto, nan, 1);

      // Optional: copula follows
      b2.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(nan, copulaTok, 2);
      });

      b2.captureSpan('ことなの', koto, nan);
    },
    // Branch 3: ... + という + こと + なの
    (b3) => {
      const toiu = b3.tok({ lemma: 'という' }, 'toiu');
      const koto = b3.noun({ lemma: 'こと' }, 'koto');
      b3.inOrder(toiu, koto, 1);

      const na = b3.tok({ text: 'な' }, 'na');
      const no = b3.tok({ text: 'の' }, 'no');
      b3.inOrder(koto, na, 1);
      b3.inOrder(na, no, 1);

      // Optional: copula follows
      b3.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(no, copulaTok, 2);
      });

      b3.captureSpan('ことなの', koto, no);
    },
    // Branch 4: ... + という + こと + なん
    (b4) => {
      const toiu = b4.tok({ lemma: 'という' }, 'toiu');
      const koto = b4.noun({ lemma: 'こと' }, 'koto');
      b4.inOrder(toiu, koto, 1);

      const nan = b4.tok({ text: 'なん' }, 'nan');
      b4.inOrder(koto, nan, 1);

      // Optional: copula follows
      b4.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(nan, copulaTok, 2);
      });

      b4.captureSpan('ことなの', koto, nan);
    },
    // Branch 5: ... + の + こと + なの (after nouns/na-adj)
    (b5) => {
      const noParticle = b5.particle('の', 'noParticle');
      const koto = b5.noun({ lemma: 'こと' }, 'koto');
      b5.inOrder(noParticle, koto, 1);

      const na = b5.tok({ text: 'な' }, 'na');
      const no = b5.tok({ text: 'の' }, 'no');
      b5.inOrder(koto, na, 1);
      b5.inOrder(na, no, 1);

      // Optional: copula follows
      b5.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(no, copulaTok, 2);
      });

      b5.captureSpan('ことなの', koto, no);
    },
    // Branch 6: ... + の + こと + なん (after nouns/na-adj)
    (b6) => {
      const noParticle = b6.particle('の', 'noParticle');
      const koto = b6.noun({ lemma: 'こと' }, 'koto');
      b6.inOrder(noParticle, koto, 1);

      const nan = b6.tok({ text: 'なん' }, 'nan');
      b6.inOrder(koto, nan, 1);

      // Optional: copula follows
      b6.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(nan, copulaTok, 2);
      });

      b6.captureSpan('ことなの', koto, nan);
    },
    // Branch 7: ... + な + こと + なの (after na-adj)
    (b7) => {
      const naParticle = b7.particle('な', 'naParticle');
      const koto = b7.noun({ lemma: 'こと' }, 'koto');
      b7.inOrder(naParticle, koto, 1);

      const na = b7.tok({ text: 'な' }, 'na');
      const no = b7.tok({ text: 'の' }, 'no');
      b7.inOrder(koto, na, 1);
      b7.inOrder(na, no, 1);

      // Optional: copula follows
      b7.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(no, copulaTok, 2);
      });

      b7.captureSpan('ことなの', koto, no);
    },
    // Branch 8: ... + な + こと + なん (after na-adj)
    (b8) => {
      const naParticle = b8.particle('な', 'naParticle');
      const koto = b8.noun({ lemma: 'こと' }, 'koto');
      b8.inOrder(naParticle, koto, 1);

      const nan = b8.tok({ text: 'なん' }, 'nan');
      b8.inOrder(koto, nan, 1);

      // Optional: copula follows
      b8.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(nan, copulaTok, 2);
      });

      b8.captureSpan('ことなの', koto, nan);
    },
    // Branch 9: ... + の + こと + な + ん (when なん is split)
    (b9) => {
      const noParticle = b9.particle('の', 'noParticle');
      const koto = b9.noun({ lemma: 'こと' }, 'koto');
      b9.inOrder(noParticle, koto, 1);

      const na = b9.tok({ text: 'な' }, 'na');
      const n = b9.tok({ text: 'ん' }, 'n');
      b9.inOrder(koto, na, 1);
      b9.inOrder(na, n, 1);

      // Optional: copula follows
      b9.optional((cop) => {
        const copulaTok = cop.tok({ lemmaOneOf: ['だ', 'です', 'である'] }, 'copula');
        cop.inOrder(n, copulaTok, 2);
      });

      b9.captureSpan('ことなの', koto, n);
    }
  );
});
