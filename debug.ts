import { segmentSentenceWithAlternatives } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';
import { matchSentence } from './src/grammarMatcher/runtime.js';
import { grammarCatalog } from './src/grammarMatcher/catalog.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const testSentence = 'この道はあの道よりずっと広い。';

console.log('Testing sentence:', testSentence);
console.log('='.repeat(60));

const alternatives = await segmentSentenceWithAlternatives(testSentence);

console.log('\nTokenization (first alternative):');
console.log('='.repeat(60));
alternatives[0].tokens.forEach((token, i) => {
  console.log(`\nToken ${i}: "${token.text}"`);
  console.log('  POS:', token.grammarInfo?.partOfSpeech);
  console.log('  Kana:', token.wordInfo?.kana);
  if (token.grammarInfo?.conjugations) {
    console.log('  Base:', token.grammarInfo.conjugations[0]?.word);
  }
});

console.log('\n\nPattern matching:');
console.log('='.repeat(60));
const matches = await matchSentence(testSentence, grammarCatalog);
console.log('Matches found:', matches.length);
matches.forEach(match => {
  console.log(`  - ${match.grammarId}`);
});

const targetMatch = matches.find(m => m.grammarId === 'n5.a-wa-b-yori');
if (targetMatch) {
  console.log('\nTarget match found! Captures:');
  Object.entries(targetMatch.captures).forEach(([name, capture]) => {
    console.log(`  ${name}:`, capture.tokens.map(t => t.text).join(''));
  });
} else {
  console.log('\n❌ n5.a-wa-b-yori did NOT match');
}

