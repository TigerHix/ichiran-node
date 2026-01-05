import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('い-adj-く-もなんともない', (r) => {
  // Match i-adjective (く-form) or auxiliary たい + も + なん + と + も + ない pattern
  // e.g., 美味しくもなんともない (not delicious at all)
  // e.g., しりたくもなんともない (don't want to know at all)

  // Fixed pattern: も + なん + と + も + ない
  const mo1 = r.particle('も', 'mo1');
  const nan = r.tok({ text: 'なん', pos: 'PRON' }, 'nan');
  const to = r.particle('と', 'to');
  const mo2 = r.particle('も', 'mo2');
  const nai = r.adj({ lemma: 'ない' }, 'nai');

  // Structural constraints for fixed pattern
  r.inOrder(mo1, nan, 1);
  r.inOrder(nan, to, 1);
  r.inOrder(to, mo2, 1);
  r.inOrder(mo2, nai, 1);

  // The i-adjective or auxiliary たい before the pattern
  // Handle multiple variants due to GiNZA parsing inconsistencies:
  r.either(
    (b) => {
      // Variant 1: i-adjective in adverbial form (連用形-一般)
      // e.g., 美味しく, 難しく, 珍しく
      const iadj = b.adj({ inflectionForm: '連用形-一般' }, 'iadj');
      b.inOrder(iadj, mo1, 1);
      b.captureSpan('い-adj-く-もなんともない', iadj, nai);
    },
    (b) => {
      // Variant 2: auxiliary たい in adverbial form (連用形-一般)
      // e.g., たく (from たい in したく, 行きたく)
      const tai = b.aux({ inflectionForm: '連用形-一般' }, 'iadj');
      b.inOrder(tai, mo1, 1);
      b.captureSpan('い-adj-く-もなんともない', tai, nai);
    },
    (b) => {
      // Variant 3: GiNZA parses some adverbial forms as NOUN
      // These end with く but have no inflectionForm
      // Match specific known forms to avoid overmatching
      const nounAdv = b.tok({
        pos: 'NOUN',
        textOneOf: ['いたく', 'かなしく', 'みたく', 'めずらしく']
      }, 'iadj');
      b.inOrder(nounAdv, mo1, 1);
      b.captureSpan('い-adj-く-もなんともない', nounAdv, nai);
    }
  );
});
