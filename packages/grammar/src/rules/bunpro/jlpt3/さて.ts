import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('さて', (r) => {
  // さて is a discourse marker/conjunction used to change topic or transition
  // GiNZA parses it as: text="さて" lemma="さて" pos="CCONJ" dep="cc"
  // It's typically sentence-initial

  const sate = r.tok({
    text: 'さて',
    lemma: 'さて',
    pos: 'CCONJ',
    dep: 'cc'
  }, 'sate');

  r.capture(sate);
});
