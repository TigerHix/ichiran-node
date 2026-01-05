import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('か否か', (r) => {
  // Core pattern: か + 否(いな) + か
  // 否(いな) is typically read as the interjection "否" (no/not)
  const ka1 = r.tok({ text: 'か' }, 'ka1');
  const ina = r.tok({ text: '否' }, 'ina');
  const ka2 = r.tok({ text: 'か' }, 'ka2');
  r.inOrder(ka1, ina, 1).inOrder(ina, ka2, 1);

  // The pattern attaches to various POS types (verb, adj, noun)
  // We use r.either() to handle the different attachment patterns
  r.either(
    // Verb + か否か (e.g., 受け入れるか否か, 転職するか否か)
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, ka1, 1);
      b.captureSpan('か否か', verb, ka2);
    },
    // い-adjective + か否か (e.g., 怖いか否か)
    (b) => {
      const adj = b.adj({ pos: 'ADJ' }, 'adj');
      b.inOrder(adj, ka1, 1);
      b.captureSpan('か否か', adj, ka2);
    },
    // Noun + か否か (e.g., スポーツ選手か否か)
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(noun, ka1, 1);
      b.captureSpan('か否か', noun, ka2);
    },
    // Noun + である + か否か (e.g., 殺人犯であるか否か, 貧乏であるか否か)
    (b) => {
      const noun = b.noun({}, 'noun');
      const dearu = b.aux({ lemma: 'である' }, 'dearu');
      b.inOrder(noun, dearu, 1).inOrder(dearu, ka1, 1);
      b.captureSpan('か否か', noun, ka2);
    }
  );
});
