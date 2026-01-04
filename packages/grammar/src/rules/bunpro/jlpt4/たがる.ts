import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たがる', (r) => {
  // Verb stem + たがる (third-person desire)
  // Examples: 行きたがる, 食べたがる, したがる
  //
  // GiNZA parsing:
  // - Verb stem: pos=VERB/AUX, may or may not have inflection form
  // - たがる: pos=AUX, lemma=たがる, dep=aux, attaches to verb stem
  //
  // Conjugations of たがる:
  // - Present: たがる (終止形-一般, 連体形-一般)
  // - Past: たがった (たがっ + た)
  // - Te-form: たがって (たがっ + て)
  // - Progressive: たがっている (たがっ + いる)
  //
  // Note: Suru-verbs (勉強する) parse as: 勉強 (VERB, no inflection) + し (AUX, 連用形-一般)

  r.either(
    // Branch 1: Present form (たがる)
    (b) => {
      const tagaru = b.aux({
        lemma: 'たがる',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'tagaru');

      // Require that たがる attaches to a VERB/AUX
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, tagaru);
      b.inOrder(verb, tagaru, 5);  // Allow some distance for auxiliaries
      b.captureSpan('たがる', verb, tagaru);
    },

    // Branch 2: Past form (たがった)
    // Both たがっ and た attach to verb with aux dep
    (b) => {
      const tagaru = b.aux({
        lemma: 'たがる',
        inflectionForm: '連用形-促音便',
      }, 'tagaru');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      // Both attach to the same verb
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, tagaru);
      b.auxOf(verb, ta);
      b.inOrder(verb, tagaru, 5);
      b.inOrder(tagaru, ta, 1);
      b.captureSpan('たがる', verb, ta);
    },

    // Branch 3: Te-form (たがって)
    // て has dep=mark, not aux
    (b) => {
      const tagaru = b.aux({
        lemma: 'たがる',
        inflectionForm: '連用形-促音便',
      }, 'tagaru');
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, tagaru);
      b.headChild(verb, te, 'mark');
      b.inOrder(verb, tagaru, 5);
      b.inOrder(tagaru, te, 1);
      b.captureSpan('たがる', verb, te);
    },

    // Branch 4: Progressive (たがっている)
    (b) => {
      const tagaru = b.aux({
        lemma: 'たがる',
        inflectionForm: '連用形-促音便',
      }, 'tagaru');
      const iru = b.aux({ lemmaOneOf: ['いる', 'る'] }, 'iru');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      b.auxOf(verb, tagaru);
      b.auxOf(verb, iru);
      b.inOrder(verb, tagaru, 5);
      b.inOrder(tagaru, iru, 1);
      b.captureSpan('たがる', verb, iru);
    }
  );
});
