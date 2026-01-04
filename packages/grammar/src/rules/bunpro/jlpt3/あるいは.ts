import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('あるいは', (r) => {
  // あるいは as a conjunction meaning "or/alternatively"
  // GiNZA tokenizes this as a single CCONJ token with lemma=あるいは
  // This distinguishes it from verb ある + particle は (separate tokens)
  const aruiwa = r.tok({
    text: 'あるいは',
    pos: 'CCONJ',
    lemma: 'あるいは'
  }, 'aruiwa');

  r.capture(aruiwa);
});
