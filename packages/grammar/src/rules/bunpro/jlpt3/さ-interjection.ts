import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('さ-interjection', (r) => {
  // さ as an interjection particle (attention-getter, filler, or encouragement)
  // Used at the beginning of sentences or utterances
  // Variations: さ, さあ, さぁ, さー
  //
  // GiNZA POS tagging is inconsistent:
  // - Sometimes pos=INTJ (interjection)
  // - Sometimes pos=PART (particle) with dep=dep or dep=advcl
  // - Sometimes pos=VERB with dep=dep
  //
  // Key discriminator from さ-casualよ (sentence-final particle):
  // - Sentence-final: pos=PART, dep=mark
  // - Interjection: pos=INTJ, or pos=PART with dep=dep/advcl, or pos=VERB with dep=dep

  r.either(
    // Pattern 1: さ as INTJ (straightforward case)
    (b) => {
      const sa = b.tok({ text: 'さ', pos: 'INTJ' }, 'sa');
      b.capture(sa);
    },
    // Pattern 2: さ as PART with dep=dep (discourse marker)
    (b) => {
      const sa = b.tok({ text: 'さ', pos: 'PART', dep: 'dep' }, 'sa');
      b.capture(sa);
    },
    // Pattern 3: さあ as INTJ
    (b) => {
      const saa = b.tok({ text: 'さあ', pos: 'INTJ' }, 'saa');
      b.capture(saa);
    },
    // Pattern 4: さあ as PART with dep=dep (discourse marker)
    (b) => {
      const saa = b.tok({ text: 'さあ', pos: 'PART', dep: 'dep' }, 'saa');
      b.capture(saa);
    },
    // Pattern 5: さぁ as INTJ
    (b) => {
      const saaa = b.tok({ text: 'さぁ', pos: 'INTJ' }, 'saaa');
      b.capture(saaa);
    },
    // Pattern 6: さぁ as VERB with dep=dep (GiNZA sometimes tags it this way)
    (b) => {
      const saaa = b.tok({ text: 'さぁ', pos: 'VERB', dep: 'dep' }, 'saaa');
      b.capture(saaa);
    },
    // Pattern 7: さー as INTJ
    (b) => {
      const saaDash = b.tok({ text: 'さー', pos: 'INTJ' }, 'saaDash');
      b.capture(saaDash);
    },
    // Pattern 8: さー as PART with dep=advcl (GiNZA tags it this way sometimes)
    (b) => {
      const saaDash = b.tok({ text: 'さー', pos: 'PART', dep: 'advcl' }, 'saaDash');
      b.capture(saaDash);
    },
    // Pattern 9: さー as PART with dep=dep
    (b) => {
      const saaDash = b.tok({ text: 'さー', pos: 'PART', dep: 'dep' }, 'saaDash');
      b.capture(saaDash);
    }
  );
});
