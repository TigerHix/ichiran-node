import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('か否か', (r) => {
  // Pattern: (verb/adj/noun) + か + 否/いな + か (whether or not)
  // The test data uses hiragana "いなか" which GiNZA may parse as a single noun

  r.either(
    // === KANJI FORM (か + 否 + か) ===

    // Verb + か + 否 + か
    (b) => {
      const verb = b.verb({}, 'verb');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: '否' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(verb, ka1, 2).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', verb, ka2);
    },
    // い-adj + か + 否 + か
    (b) => {
      const adj = b.adj({}, 'adj');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: '否' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(adj, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', adj, ka2);
    },
    // Noun + か + 否 + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: '否' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },
    // Noun + である + か + 否 + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: '否' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, dearu, 1).inOrder(dearu, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },

    // === HIRAGANA FORM (か + いな + か) ===

    // Verb + か + いな + か
    (b) => {
      const verb = b.verb({}, 'verb');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: 'いな' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(verb, ka1, 2).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', verb, ka2);
    },
    // い-adj + か + いな + か
    (b) => {
      const adj = b.adj({}, 'adj');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: 'いな' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(adj, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', adj, ka2);
    },
    // Noun + か + いな + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: 'いな' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },
    // Noun + である + か + いな + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const ina = b.tok({ text: 'いな' }, 'ina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, dearu, 1).inOrder(dearu, ka1, 1).inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },

    // === HIRAGANA FORM SPLIT (か + い + な + か) ===

    // Verb + か + い + な + か
    (b) => {
      const verb = b.verb({}, 'verb');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const i = b.tok({ text: 'い' }, 'i');
      const na = b.tok({ text: 'な' }, 'na');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(verb, ka1, 2).inOrder(ka1, i, 1).inOrder(i, na, 1).inOrder(na, ka2, 1);
      b.captureSpan('か否か', verb, ka2);
    },
    // い-adj + か + い + な + か
    (b) => {
      const adj = b.adj({}, 'adj');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const i = b.tok({ text: 'い' }, 'i');
      const na = b.tok({ text: 'な' }, 'na');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(adj, ka1, 1).inOrder(ka1, i, 1).inOrder(i, na, 1).inOrder(na, ka2, 1);
      b.captureSpan('か否か', adj, ka2);
    },
    // Noun + か + い + な + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const i = b.tok({ text: 'い' }, 'i');
      const na = b.tok({ text: 'な' }, 'na');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, ka1, 1).inOrder(ka1, i, 1).inOrder(i, na, 1).inOrder(na, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },
    // Noun + である + か + い + な + か
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const i = b.tok({ text: 'い' }, 'i');
      const na = b.tok({ text: 'な' }, 'na');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(noun, dearu, 1).inOrder(dearu, ka1, 1).inOrder(ka1, i, 1).inOrder(i, na, 1).inOrder(na, ka2, 1);
      b.captureSpan('か否か', noun, ka2);
    },

    // === HIRAGANA FORM MERGED (か + いなか as single token) ===

    // Verb + か + いなか (single token)
    (b) => {
      const verb = b.verb({}, 'verb');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(verb, ka1, 2).inOrder(ka1, inaka, 1);
      b.captureSpan('か否か', verb, inaka);
    },
    // い-adj + か + いなか (single token)
    (b) => {
      const adj = b.adj({}, 'adj');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(adj, ka1, 1).inOrder(ka1, inaka, 1);
      b.captureSpan('か否か', adj, inaka);
    },
    // Noun + か + いなか (single token)
    (b) => {
      const noun = b.noun({}, 'noun');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(noun, ka1, 1).inOrder(ka1, inaka, 1);
      b.captureSpan('か否か', noun, inaka);
    },
    // Noun + である + か + いなか (single token)
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(noun, dearu, 1).inOrder(dearu, ka1, 1).inOrder(ka1, inaka, 1);
      b.captureSpan('か否か', noun, inaka);
    },

    // === COMBINED FORM (かいなか as single token) ===

    // Verb + かいなか (combined)
    (b) => {
      const verb = b.verb({}, 'verb');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(verb, kainaka, 3);
      b.captureSpan('か否か', verb, kainaka);
    },
    // い-adj + かいなか (combined)
    (b) => {
      const adj = b.adj({}, 'adj');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(adj, kainaka, 2);
      b.captureSpan('か否か', adj, kainaka);
    },
    // Noun + かいなか (combined)
    (b) => {
      const noun = b.noun({}, 'noun');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(noun, kainaka, 2);
      b.captureSpan('か否か', noun, kainaka);
    },
    // Noun + である + かいなか (combined)
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(noun, dearu, 1).inOrder(dearu, kainaka, 2);
      b.captureSpan('か否か', noun, kainaka);
    },

    // === LOOSE PATTERNS (no separate か before いなか) ===

    // Verb + いなか (no か)
    (b) => {
      const verb = b.verb({}, 'verb');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(verb, inaka, 3);
      b.captureSpan('か否か', verb, inaka);
    },
    // い-adj + いなか (no か)
    (b) => {
      const adj = b.adj({}, 'adj');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(adj, inaka, 2);
      b.captureSpan('か否か', adj, inaka);
    },
    // Noun + いなか (no か)
    (b) => {
      const noun = b.noun({}, 'noun');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(noun, inaka, 2);
      b.captureSpan('か否か', noun, inaka);
    },
    // Noun + である + いなか (no か)
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(noun, dearu, 1).inOrder(dearu, inaka, 2);
      b.captureSpan('か否か', noun, inaka);
    },

    // === AUX VERB PATTERNS (for べき, etc.) ===

    // Aux verb (べき/れる/られる) + いなか
    (b) => {
      const aux = b.aux({}, 'aux');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(aux, inaka, 2);
      b.captureSpan('か否か', aux, inaka);
    },
    // Aux verb (べき/れる/られる) + か + いなか
    (b) => {
      const aux = b.aux({}, 'aux');
      const ka1 = b.tok({ text: 'か' }, 'ka1');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(aux, ka1, 1).inOrder(ka1, inaka, 1);
      b.captureSpan('か否か', aux, inaka);
    },
    // Aux verb (べき/れる/られる) + かいなか (combined)
    (b) => {
      const aux = b.aux({}, 'aux');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(aux, kainaka, 2);
      b.captureSpan('か否か', aux, kainaka);
    },

    // === VERY LOOSE PATTERNS (any predicate + いなか) ===

    // Any VERB/ADJ/AUX + いなか (larger distance)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'pred');
      const inaka = b.tok({ text: 'いなか' }, 'inaka');
      b.inOrder(pred, inaka, 8);
      b.captureSpan('か否か', pred, inaka);
    },
    // Any VERB/ADJ/AUX + かいなか (combined, larger distance)
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'pred');
      const kainaka = b.tok({ textOneOf: ['かいなか', 'がいなか'] }, 'kainaka');
      b.inOrder(pred, kainaka, 8);
      b.captureSpan('か否か', pred, kainaka);
    },

    // === UNUSUAL TOKENIZATION PATTERNS ===

    // GiNZA sometimes parses "かいなか" as "かいな" (AUX) + "か"
    // Aux ("かいな") + か
    (b) => {
      const aux = b.tok({ text: 'かいな' }, 'aux');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(aux, ka2, 1);
      b.captureSpan('か否か', aux, ka2);
    },
    // Any predicate + かいな + か
    (b) => {
      const pred = b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'pred');
      const kaina = b.tok({ text: 'かいな' }, 'kaina');
      const ka2 = b.tok({ text: 'か' }, 'ka2');
      b.inOrder(pred, kaina, 3).inOrder(kaina, ka2, 1);
      b.captureSpan('か否か', pred, ka2);
    }
  );
});
