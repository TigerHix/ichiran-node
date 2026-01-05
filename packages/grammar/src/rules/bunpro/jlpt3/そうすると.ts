import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('そうすると', (r) => {
  // そうすると (conjunction: "and then", "so", "with that")
  // A sentence starter meaning "once (A) is done, (B)" or "upon (A), (B)"
  // Similar to すると but with そう (like that/as such) added
  //
  // GiNZA may tokenize this as:
  // 1. Single token: そうすると
  // 2. Multiple tokens: そう (ADV) + する (VERB) + と (PART)

  r.either(
    // Branch 1: Single token (if GiNZA tokenizes it that way)
    (b) => {
      const sousuruto = b.tok({ text: 'そうすると' }, 'sousuruto');
      b.capture(sousuruto);
    },
    // Branch 2: Multi-token: そう + する + と
    (b) => {
      const sou = b.tok({ text: 'そう' }, 'sou');
      const suru = b.tok({ text: 'する' }, 'suru');
      const to = b.tok({ text: 'と' }, 'to');
      b.inOrder(sou, suru, 1);
      b.inOrder(suru, to, 1);
      b.captureSpan('そうすると', sou, to);
    }
  );
});
