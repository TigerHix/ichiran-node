// Quick debug script to see how GiNZA parses a sample sentence
import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/src/program.ts';

async function debug() {
  const engine = new GrammarEngine({
    rulesets: [],
  });
  
  const sent = 'サッカーをするのがへたです。';
  console.log('Parsing:', sent);
  console.log('');
  
  const doc = await engine.analyze(sent);
  
  for (let i = 0; i < doc.tokens.length; i++) {
    const tok = doc.tokens[i];
    const inf = tok.inflectionForm || 'none';
    console.log(`[${i}] ${tok.text}`);
    console.log(`    POS: ${tok.pos}, Lemma: ${tok.lemma}`);
    console.log(`    Dep: ${tok.dep}, Head: ${tok.head}`);
    console.log(`    Inflection: ${inf}`);
    console.log('');
  }
}

debug().catch(console.error);
