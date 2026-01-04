import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('てくれてありがとう', (r) => {
  // Verb[te-form] + くれて + ありがとう (thank you for doing)
  // Structure: Any verb in te-form + くれて + ありがとう (or polite variants)
  // Examples:
  //   教えてくれてありがとう (Thank you for teaching me)
  //   作ってくれてありがとうございます (Thank you for making)
  //   買ってくれてありがとう (Thank you for buying)
  //
  // GiNZA parsing:
  //   - Verb in te-form: VERB + て (SCONJ, dep=mark)
  //   - くれて: くれ (VERB, lemma=くれる, dep=fixed/advcl) + て (SCONJ, dep=mark)
  //   - ありがとう: INTJ with lemma=ありがとう
  //   - Polite: ございます as AUX attached to ありがとう
  //
  // The pattern expresses gratitude for someone doing something for you.
  // Casual: ありがとう, Polite: ございます (non-past) / ございました (past)

  r.either(
    // Variant 1: Casual form (ありがとう)
    // GiNZA: verb-te + くれ(fixed) + て + ありがとう(root)
    // Note: te2.head points to verbTe, not to kure
    // Handles both simple verb-te + くれて and complex chains like verb-te-いる-te-くれて
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const kure = b.verb({ lemma: 'くれる' }, 'kure');
      const te2 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te2');
      const arigatou = b.tok({ lemma: 'ありがとう', pos: 'INTJ' }, 'arigatou');

      // Structural constraints
      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 1);
      b.inOrder(te1, kure, 5);  // Allow more distance for aux chains like ている
      b.inOrder(kure, te2, 1);
      b.headChild(verbTe, te2, 'mark');  // te2's head is verbTe, not kure!
      b.inOrder(te2, arigatou, 2);

      // Capture the full pattern
      b.captureSpan('てくれてありがとう', verbTe, arigatou);
    },

    // Variant 2: Polite non-past (ありがとうございます)
    // GiNZA: verb-te + くれ(fixed) + て + ありがとう(discourse) + ござい(root) + ます(aux)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const kure = b.verb({ lemma: 'くれる' }, 'kure');
      const te2 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te2');
      const arigatou = b.tok({ lemma: 'ありがとう', posOneOf: ['INTJ', 'NOUN'], dep: 'discourse' }, 'arigatou');
      const gozai = b.verb({ lemma: 'ござる', dep: 'root' }, 'gozai');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 1);
      b.inOrder(te1, kure, 5);  // Allow more distance
      b.inOrder(kure, te2, 1);
      b.headChild(verbTe, te2, 'mark');
      b.inOrder(te2, arigatou, 2);
      b.inOrder(arigatou, gozai, 2);
      b.auxOf(gozai, masu);

      b.captureSpan('てくれてありがとうございます', verbTe, masu);
    },

    // Variant 3: Polite past (ありがとうございました)
    (b) => {
      const verbTe = b.tok({ posOneOf: ['VERB', 'AUX'] }, 'verbTe');
      const te1 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te1');
      const kure = b.verb({ lemma: 'くれる' }, 'kure');
      const te2 = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te2');
      const arigatou = b.tok({ lemma: 'ありがとう', posOneOf: ['INTJ', 'NOUN'], dep: 'discourse' }, 'arigatou');
      const gozai = b.verb({ lemma: 'ござる', dep: 'root' }, 'gozai');
      const mashita = b.aux({ lemma: 'ます' }, 'mashita');

      b.headChild(verbTe, te1, 'mark');
      b.inOrder(verbTe, te1, 1);
      b.inOrder(te1, kure, 5);  // Allow more distance
      b.inOrder(kure, te2, 1);
      b.headChild(verbTe, te2, 'mark');
      b.inOrder(te2, arigatou, 2);
      b.inOrder(arigatou, gozai, 2);
      b.auxOf(gozai, mashita);

      b.captureSpan('てくれてありがとうございました', verbTe, mashita);
    }
  );
});
