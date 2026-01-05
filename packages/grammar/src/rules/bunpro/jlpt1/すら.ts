import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('すら', (r) => {
  // すら (sura) - "even" particle (formal, more emphatic than さえ)
  // Emphasizes extreme examples; often used with negative
  //
  // Patterns from test data:
  // 1. Noun + すら: 鉛筆すら, 平仮名すら, 本名すら, 日常会話すら
  // 2. Noun + particle + すら: 家族にすら, 子供でさえ, 夏ですら
  // 3. Noun + で + すら: 小学生ですら, ネイティブですら, 子供ですら
  // 4. Verb-te/noun + の + すら: 立ち上がることすら, 夏ですら
  // 5. Noun + すら + も: ガラケーすらも (extra emphasis)
  //
  // The すら particle typically has dep='case' when marking nouns

  r.either(
    // Pattern 1: Noun + すら (direct attachment - most common)
    // 鉛筆すら, 平仮名すら, 本名すら, 日常会話すら
    // すら acts as case marker attached directly to noun
    (b) => {
      const sura = b.particle('すら', 'sura', { dep: 'case' });
      const noun = b.noun({}, 'noun');

      // Optional: Allow も after すら for extra emphasis (e.g., 鉛筆すらも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sura, mo, 1);
      });

      b.inOrder(noun, sura, 1);
      b.captureSpan('すら', noun, sura);
    },

    // Pattern 2: Particle + すら
    // 家族にすら, 夏ですら, ネイティブですら, 子供でさえ
    // すら acts as a case marker (dep=case) following particles
    (b) => {
      const sura = b.particle('すら', 'sura', { dep: 'case' });
      const particle = b.tok({
        posOneOf: ['ADP', 'PART'],  // particles like に, で, か, の
      }, 'particle');

      // Optional: Allow も after すら for extra emphasis (e.g., すらも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sura, mo, 1);
      });

      b.inOrder(particle, sura, 1);
      b.captureSpan('すら', particle, sura);
    },

    // Pattern 3: Noun + particle + すら
    // 家族にすら, 子供でさえ (when particle follows noun)
    // We need to capture from the noun
    (b) => {
      const sura = b.particle('すら', 'sura', { dep: 'case' });
      const particle = b.tok({
        posOneOf: ['ADP', 'PART'],
      }, 'particle');
      const noun = b.noun({}, 'noun');

      // Optional: Allow も after すら for extra emphasis
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sura, mo, 1);
      });

      b.inOrder(noun, particle, 1);
      b.inOrder(particle, sura, 1);
      b.captureSpan('すら', noun, sura);
    },

    // Pattern 4: Noun + の + すら
    // 立ち上がることすら (verb nominalized with の + すら)
    (b) => {
      const sura = b.particle('すら', 'sura', { dep: 'case' });
      const no = b.particle('の', 'no');
      const noun = b.noun({}, 'noun');

      // Optional: Allow も after すら for extra emphasis (e.g., ことすらも)
      b.optional((opt) => {
        const mo = opt.particle('も', 'mo');
        opt.inOrder(sura, mo, 1);
      });

      b.inOrder(noun, no, 1);
      b.inOrder(no, sura, 1);
      b.captureSpan('すら', noun, sura);
    }
  );
});
