import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('そこで', (r) => {
  // そこで (therefore / accordingly / so / to that end)
  // Conjunction showing consequence or action taken in response to a situation
  // GiNZA tokenizes as either:
  // - Single token (text=そこで)
  // - Two tokens: そこ + で
  //
  // Note: This matches both conjunction use (therefore) and locative use (at that place)
  // since GiNZA doesn't consistently distinguish them with dep labels,
  // and the distinction is context-dependent.
  r.either(
    // Pattern 1: Single token そこで
    (b) => {
      const sokode = b.tok({ text: 'そこで' }, 'sokode');
      b.capture(sokode);
    },
    // Pattern 2: Two tokens - そこ + で
    (b) => {
      const soko = b.tok({ text: 'そこ' }, 'soko');
      const de = b.tok({ text: 'で' }, 'de');
      b.inOrder(soko, de, 1);
      b.captureSpan('そこで', soko, de);
    }
  );
});
