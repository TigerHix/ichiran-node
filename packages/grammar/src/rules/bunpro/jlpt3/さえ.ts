import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('さえ', (r) => {
  // さえ (sae) - "even" particle
  // Emphasizes that something is surprising or extreme enough to be included
  //
  // Patterns from test data:
  // 1. Particle + さえ: 日本にさえ, 彼からさえ, あのことについてさえ, 僕の声さえ
  // 2. Noun + さえ: 戦いさえ, 家族さえ
  // 3. Noun + で + さえ: 夏でさえ, 子供でさえ, プロでさえ
  // 4. Verb-te + さえ: 放っておいてさえ, いてさえ (conjunctive form + さえ)
  // 5. Optional も: 家族さえも (for extra emphasis)
  //
  // The さえ particle typically has dep='case' when marking nouns

  r.either(
    // Pattern 1: Particle/Verb-te + さえ (most common pattern)
    // 日本にさえ, 彼からさえ, あのことについてさえ, 僕の声さえ
    // 放っておいてさえ, いてさえ (verb conjunctive form)
    // さえ acts as a case marker (dep=case) following particles or verb-te forms
    (b) => {
      const sae = b.particle('さえ', 'sae', { dep: 'case' });
      const particle = b.tok({
        posOneOf: ['ADP', 'SCONJ'],  // ADP for particles, SCONJ for て-form
      }, 'particle');

      // Optional: Allow も after さえ for extra emphasis (e.g., 日本にさえも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sae, mo, 1);
      });

      b.inOrder(particle, sae, 1);
      b.captureSpan('さえ', particle, sae);
    },

    // Pattern 2: Noun + さえ (direct attachment)
    // 戦いさえ, 家族さえ (when no other particle)
    // さえ acts as case marker attached to noun
    (b) => {
      const sae = b.particle('さえ', 'sae', { dep: 'case' });
      const noun = b.noun({}, 'noun');

      // Optional: Allow も after さえ for extra emphasis (e.g., 家族さえも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sae, mo, 1);
      });

      b.inOrder(noun, sae, 1);
      b.captureSpan('さえ', noun, sae);
    },

    // Pattern 3: Noun + で + さえ
    // 夏でさえ, 子供でさえ, プロでさえ
    // で is a copula/case marker, さえ follows it
    (b) => {
      const sae = b.particle('さえ', 'sae', { dep: 'case' });
      const de = b.particle('で', 'de');
      const noun = b.noun({}, 'noun');

      // Optional: Allow も after さえ for extra emphasis (e.g., 子供でさえも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sae, mo, 1);
      });

      b.inOrder(noun, de, 1);
      b.inOrder(de, sae, 1);
      b.captureSpan('でさえ', noun, sae);
    }
  );
});
