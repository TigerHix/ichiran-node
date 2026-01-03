import { linguisticRule } from './packages/grammar/src/engine/lang.js';
import { GiNZA } from './packages/grammar/src/ginza/client.js';
import { Compiler } from './packages/grammar/src/engine/compiler.js';
import { matchInDocument } from './packages/grammar/src/engine/runtime.js';

async function main() {
  const ginza = new GiNZA();

  // Create a simple test rule
  const testRule = linguisticRule('test-ga', (r) => {
    const ga = r.tok({ text: 'が' }, 'ga');
    r.capture(ga);
  });

  const compiled = new Compiler().compile(testRule);
  const sentence = '毎日走るが、運動はきらいです。';
  const doc = await ginza.parse(sentence);

  console.log('\n=== ' + sentence + ' ===');
  console.log('All tokens:');
  for (let i = 0; i < doc.tokens.length; i++) {
    const token = doc.tokens[i];
    console.log(`  [${i}] ${token.text}: pos=${token.pos}, dep=${token.dep}, lemma=${token.lemma}`);
  }

  console.log('\n=== Matching with just text=が ===');
  const matches = matchInDocument(compiled.variants[0], doc);
  console.log('Matches:', matches.length);
  if (matches.length > 0) {
    console.log('First match:', JSON.stringify(matches[0], null, 2));
  }

  // Now test with pos constraint
  const testRule2 = linguisticRule('test-ga-pos', (r) => {
    const ga = r.tok({ text: 'が', pos: 'ADP' }, 'ga');
    r.capture(ga);
  });

  const compiled2 = new Compiler().compile(testRule2);
  console.log('\n=== Matching with text=が, pos=ADP ===');
  const matches2 = matchInDocument(compiled2.variants[0], doc);
  console.log('Matches:', matches2.length);

  // Test with pos=PART
  const testRule3 = linguisticRule('test-ga-part', (r) => {
    const ga = r.tok({ text: 'が', pos: 'PART' }, 'ga');
    r.capture(ga);
  });

  const compiled3 = new Compiler().compile(testRule3);
  console.log('\n=== Matching with text=が, pos=PART ===');
  const matches3 = matchInDocument(compiled3.variants[0], doc);
  console.log('Matches:', matches3.length);

  // Test with pos=SCONJ
  const testRule4 = linguisticRule('test-ga-sconj', (r) => {
    const ga = r.tok({ text: 'が', pos: 'SCONJ' }, 'ga');
    r.capture(ga);
  });

  const compiled4 = new Compiler().compile(testRule4);
  console.log('\n=== Matching with text=が, pos=SCONJ ===');
  const matches4 = matchInDocument(compiled4.variants[0], doc);
  console.log('Matches:', matches4.length);

  // Test with different deps
  for (const dep of ['case', 'dep', 'cc', 'mark', 'nsubj', 'obj']) {
    const testRuleDep = linguisticRule(`test-ga-dep-${dep}`, (r) => {
      const ga = r.tok({ text: 'が', dep }, 'ga');
      r.capture(ga);
    });

    const compiledDep = new Compiler().compile(testRuleDep);
    const matchesDep = matchInDocument(compiledDep.variants[0], doc);
    console.log(`\n=== Matching with text=が, dep=${dep} ===`);
    console.log('Matches:', matchesDep.length);
  }
}

main().catch(console.error);
