import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verb-て-b', (r) => {
  // Match verb て-form used for connecting sequential actions
  // Pattern: Verb[連用形] + て + (next action)
  // Meaning: "and", "and then", "after that" - sequence of events
  // e.g., "パンを買って、食べた" (bought bread AND THEN ate it)

  // The key discriminator from verb-て (request) is:
  // - verb-て-b: verb has dep=advcl (subordinate clause modifying main verb)
  // - verb-て (request): verb has dep=root (main predicate)

  const verb = r.verb(
    {
      // Verb must be in te-form (連用形)
      // This is a subordinate clause connecting to the next action
      dep: 'advcl',
    },
    'verb'
  );

  const te = r.tok(
    {
      text: 'て',
      lemma: 'て',
      pos: 'SCONJ',
      dep: 'mark',
    },
    'te'
  );

  // て attaches to the verb
  r.headChild(verb, te, 'mark');
  r.inOrder(verb, te, 1);

  // Capture from verb to て (e.g., "買って", "食べて")
  r.captureSpan('verb-te', verb, te);
});
