import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('しかない', (r) => {
  // しかない (nothing but/only/have no choice but to)
  // Pattern: Verb + しか + ない (negative auxiliary)
  // The particle しか emphasizes "nothing but" and requires negative
  //
  // GiNZA parsing variations:
  //
  // 1. Simple verb + しかない: "呼ぶしかない"
  //    呼ぶ (VERB, ROOT) → 0
  //    しか (PART, mark) → 0
  //    ない (AUX, fixed) → 1
  //
  // 2. Verb-te-iru + しかない: "待っているしかない"
  //    待っ (VERB, ROOT) → 2
  //    て (SCONJ, mark) → 2
  //    いる (VERB, fixed) → 3
  //    しか (PART, mark) → 2 (marks main verb)
  //    ない (AUX, fixed) → 4
  //
  // 3. Polite: "謝るしかないです"
  //    謝る (VERB, advcl) → 4
  //    しか (PART, mark) → 2
  //    ない (ADJ, ROOT) → 4
  //    です (AUX, aux) → 4
  //
  // 4. Past tense: "守るしかなかった"
  //    守る (VERB, ROOT) → 0
  //    しか (PART, mark) → 0
  //    なかっ (AUX, fixed) → 1
  //    た (AUX, aux) → 0

  r.either(
    // Pattern 1: Verb (dictionary form) + しか + ない (non-past)
    // ない is AUX with dep=fixed (most common)
    // e.g., 呼ぶしかない, 行くしかない, 待つしかない, するしかない, 待っているしかない
    (b) => {
      const verb = b.verb({}, 'verb');
      const shika = b.particle('しか', 'shika', { dep: 'mark' });
      const nai = b.aux({
        lemma: 'ない',
        dep: 'fixed'
      }, 'nai');

      b.headChild(verb, shika);
      b.headChild(shika, nai);
      b.inOrder(verb, shika, 5);  // Allow up to 5 tokens for verb-te-iru constructions
      b.inOrder(shika, nai, 2);

      b.captureSpan('しかない', verb, nai);
    },
    // Pattern 2: Verb + しか + ない (polite form with です)
    // ない is ADJ (i-adjective) when followed by polite です
    // e.g., 謝るしかないです
    // Parsing: 謝る(VERB) → しか(PART,mark) → ない(ADJ,ROOT) ← です(AUX,aux)
    (b) => {
      const verb = b.verb({}, 'verb');
      const shika = b.particle('しか', 'shika', { dep: 'mark' });
      const nai = b.adj({
        lemma: 'ない'
      }, 'nai');
      const desu = b.aux({
        lemma: 'です'
      }, 'desu');

      b.headChild(verb, shika);
      b.headChild(nai, desu);
      b.inOrder(verb, shika, 2);
      b.inOrder(shika, nai, 2);
      b.inOrder(nai, desu, 1);

      b.captureSpan('しかない', verb, nai);
    },
    // Pattern 3: Verb (dictionary form) + しか + なかった (past tense)
    // e.g., 守るしかなかった
    (b) => {
      const verb = b.verb({}, 'verb');
      const shika = b.particle('しか', 'shika', { dep: 'mark' });
      const nakkatsu = b.aux({
        lemma: 'ない',
        dep: 'fixed'
      }, 'nakkatsu');
      const ta = b.aux({
        lemma: 'た',
        dep: 'aux'
      }, 'ta');

      b.headChild(verb, shika);
      b.headChild(shika, nakkatsu);
      b.auxOf(verb, ta);
      b.inOrder(verb, shika, 2);
      b.inOrder(shika, nakkatsu, 2);
      b.inOrder(nakkatsu, ta, 1);

      b.captureSpan('しかない', verb, ta);
    }
  );
});
