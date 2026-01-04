import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('どうしても', (r) => {
  // どうしても (doushitemo) - adverb meaning "by all means, no matter what, at any cost"
  // Expresses strong volition, necessity, or inevitability
  //
  // Patterns:
  // 1. どうしても + sentence-initial: どうしても食べたい, どうしても行きたい
  // 2. Verb + と + どうしても: 食べるとどうしても眠たくなる
  //
  // GiNZA parses どうしても as 4 tokens:
  // - どう (ADV)
  // - し (AUX, lemma=する)
  // - て (SCONJ, lemma=て)
  // - も (ADP, lemma=も)

  const dou = r.adv({ text: 'どう' }, 'dou');
  const shi = r.aux({ lemma: 'する', text: 'し' }, 'shi');
  const te = r.tok({ pos: 'SCONJ', lemma: 'て', text: 'て' }, 'te');
  const mo = r.tok({ pos: 'ADP', lemma: 'も', text: 'も' }, 'mo');

  r.either(
    // Pattern 1: どうしても + predicate (sentence-initial or mid-sentence)
    // どうしても食べたい, どうしても行きたい
    // どうしてもアイスクリームが食べたい
    // どうしても諦められない夢がある
    (b) => {
      b.inOrder(dou, shi, 1);
      b.inOrder(shi, te, 1);
      b.inOrder(te, mo, 1);

      // Match any predicate (verb, adjective, or auxiliary)
      const predicate = b.tok({
        posOneOf: ['VERB', 'ADJ', 'AUX'],
      }, 'predicate');
      b.inOrder(mo, predicate, 5);
      b.captureSpan('どうしても', dou, mo);
    },

    // Pattern 2: Verb + と + どうしても + predicate
    // 食べるとどうしても眠たくなる
    // 牛乳飲むとどうしてもお腹が痛くなるんです
    // 寝る前にお腹いっぱいに食べるとどうしても寝れなくなっちゃうんだよね
    (b) => {
      const verb1 = b.verb({}, 'verb1');
      const to = b.particle('と', 'to');
      b.inOrder(verb1, to, 1);
      b.inOrder(to, dou, 1);
      b.inOrder(dou, shi, 1);
      b.inOrder(shi, te, 1);
      b.inOrder(te, mo, 1);

      const predicate = b.tok({
        posOneOf: ['VERB', 'ADJ', 'AUX'],
      }, 'predicate');
      b.inOrder(mo, predicate, 10);
      b.captureSpan('どうしても', dou, mo);
    }
  );
});
