import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のに-despite', (r) => {
  // のに (despite, although, even though)
  // Contrastive conjunction particle connecting two clauses
  // Pattern: Verb/Adj + のに = "despite X, Y"
  // 
  // Grammar structures:
  // 1. Verb + のに (direct connection)
  // 2. い-adj + のに (direct connection)
  // 3. な-adj + な + のに (standard form: auxiliary な)
  // 4. Noun + な + のに (standard form: auxiliary な)
  // 5. Noun + だ + のに (colloquial/dialectal form: copula だ)
  //
  // GiNZA parses のに as:
  // - の: pos=SCONJ, dep=mark (marks the preceding clause)
  // - に: pos=ADP, dep=case (case particle)
  //
  // For な:
  // - With な-adjectives: dep=aux, lemma=だ, text=な
  // - With nouns: dep=cop, lemma=だ, text=な (GiNZA inconsistency)
  // For だ (colloquial):
  // - dep=cop, lemma=だ, text=だ
  //
  // The pattern connects a preceding clause (verb/adj/noun+な/だ) to のに,
  // expressing contrast or unexpected outcome.
  // Similar to が/けど but specifically for "despite/although" nuance.

  r.either(
    // Pattern 1: Verb + のに (direct connection)
    (b) => {
      const verb = b.verb({}, 'verb');
      const no = b.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const ni = b.tok({ text: 'に', pos: 'ADP', dep: 'case' }, 'ni');
      
      b.inOrder(verb, no).inOrder(no, ni, 1);
      b.captureSpan('のに', verb, ni);
    },
    // Pattern 2: い-adj + のに (direct connection)
    (b) => {
      const adj = b.adj({}, 'adj');
      const no = b.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const ni = b.tok({ text: 'に', pos: 'ADP', dep: 'case' }, 'ni');
      
      b.inOrder(adj, no).inOrder(no, ni, 1);
      b.captureSpan('のに', adj, ni);
    },
    // Pattern 3: な-adj/Noun + な + のに (standard form)
    // Note: GiNZA assigns dep=aux for な-adjectives but dep=cop for nouns
    (b) => {
      const noun = b.noun({}, 'noun');
      const na = b.aux({ lemma: 'だ', text: 'な', depOneOf: ['aux', 'cop'] }, 'na');
      const no = b.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const ni = b.tok({ text: 'に', pos: 'ADP', dep: 'case' }, 'ni');
      
      b.inOrder(noun, na, 1).inOrder(na, no, 1).inOrder(no, ni, 1);
      b.captureSpan('なのに', noun, ni);
    },
    // Pattern 4: Noun + だ + のに (colloquial form)
    (b) => {
      const noun = b.noun({}, 'noun');
      const da = b.aux({ lemma: 'だ', text: 'だ', dep: 'cop' }, 'da');
      const no = b.tok({ text: 'の', pos: 'SCONJ', dep: 'mark' }, 'no');
      const ni = b.tok({ text: 'に', pos: 'ADP', dep: 'case' }, 'ni');
      
      b.inOrder(noun, da, 1).inOrder(da, no, 1).inOrder(no, ni, 1);
      b.captureSpan('だのに', noun, ni);
    }
  );
});
