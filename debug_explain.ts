import { linguisticRule, explainMatch } from './packages/grammar/src/engine/index.js';
import { GiNZA } from './packages/grammar/src/ginza/client.js';

async function main() {
  const ginza = new GiNZA();

  const sentence = '毎日走るが、運動はきらいです。';

  console.log('\n=== ' + sentence + ' ===');
  const doc = await ginza.parse(sentence);

  console.log('All tokens:');
  for (let i = 0; i < doc.tokens.length; i++) {
    const token = doc.tokens[i];
    console.log(`  [${i}] ${token.text}: pos=${token.pos}, dep=${token.dep}, lemma=${token.lemma}`);
  }

  // Test with just text
  const testRule1 = linguisticRule('test-ga-text', (r) => {
    const ga = r.tok({ text: 'が' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 1: text=が ===');
  const result1 = await explainMatch(ginza, testRule1, sentence);
  console.log(JSON.stringify(result1, null, 2));

  // Test with pos=ADP
  const testRule2 = linguisticRule('test-ga-adp', (r) => {
    const ga = r.tok({ text: 'が', pos: 'ADP' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 2: text=が, pos=ADP ===');
  const result2 = await explainMatch(ginza, testRule2, sentence);
  console.log(JSON.stringify(result2, null, 2));

  // Test with pos=PART
  const testRule3 = linguisticRule('test-ga-part', (r) => {
    const ga = r.tok({ text: 'が', pos: 'PART' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 3: text=が, pos=PART ===');
  const result3 = await explainMatch(ginza, testRule3, sentence);
  console.log(JSON.stringify(result3, null, 2));

  // Test with pos=SCONJ
  const testRule4 = linguisticRule('test-ga-sconj', (r) => {
    const ga = r.tok({ text: 'が', pos: 'SCONJ' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 4: text=が, pos=SCONJ ===');
  const result4 = await explainMatch(ginza, testRule4, sentence);
  console.log(JSON.stringify(result4, null, 2));

  // Test with dep=cc
  const testRule5 = linguisticRule('test-ga-cc', (r) => {
    const ga = r.tok({ text: 'が', dep: 'cc' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 5: text=が, dep=cc ===');
  const result5 = await explainMatch(ginza, testRule5, sentence);
  console.log(JSON.stringify(result5, null, 2));

  // Test with dep=dep
  const testRule6 = linguisticRule('test-ga-dep', (r) => {
    const ga = r.tok({ text: 'が', dep: 'dep' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 6: text=が, dep=dep ===');
  const result6 = await explainMatch(ginza, testRule6, sentence);
  console.log(JSON.stringify(result6, null, 2));

  // Test with dep=mark
  const testRule7 = linguisticRule('test-ga-mark', (r) => {
    const ga = r.tok({ text: 'が', dep: 'mark' }, 'ga');
    r.capture(ga);
  });

  console.log('\n=== Test 7: text=が, dep=mark ===');
  const result7 = await explainMatch(ginza, testRule7, sentence);
  console.log(JSON.stringify(result7, null, 2));
}

main().catch(console.error);
