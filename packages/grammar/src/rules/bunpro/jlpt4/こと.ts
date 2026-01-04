import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('こと', (r) => {
  // こと as nominalizer (converts verb phrase to noun)
  // Structure: [VERB/AUX phrase] こと
  //
  // Most common pattern: verb has dep=acl (adnominal clause) pointing to こと
  //   - 日本語を話すことは難しい (話す→こと via acl)
  //   - スポーツをすることが好き (する→こと via acl)
  //
  // Alternative pattern: verb + こと + は + ある (there are times when...)
  //   - 職場でワープロをつかうことはある？
  //   Here つかう is root, and こと is nsubj pointing to ある

  r.either(
    // Standard pattern: verb has dep=acl (adnominal clause) pointing to こと
    (b1) => {
      const koto = b1.noun({ lemma: 'こと' }, 'koto');
      const verb = b1.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');

      b1.inOrder(verb, koto, 2);
      b1.headChild(koto, verb, 'acl');
      b1.capture(koto);
    },
    // Flexible pattern: verb precedes こと without specific dependency
    // For constructions like こと + は + ある
    (b2) => {
      const koto = b2.noun({ lemma: 'こと' }, 'koto');
      const verb = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');

      b2.inOrder(verb, koto, 2);
      b2.capture(koto);
    }
  );
});
