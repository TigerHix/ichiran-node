import { segmentSentenceWithAlternatives, matchSentence } from './src/grammarMatcher/runtime.js';
import { resolvePredicate } from './src/grammarMatcher/predicates.js';
import { grammarCatalog } from './src/grammarMatcher/catalog.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const sentence = 'なんでそんなに慌てているんだ？';
console.log('Testing:', sentence);
console.log('');

const alternatives = await segmentSentenceWithAlternatives(sentence);
const tokens = alternatives[0].tokens;

console.log('Tokens:');
const isNominalHead = resolvePredicate('isNominalHead');
for (let i = 0; i < tokens.length; i++) {
  const token = tokens[i];
  const matches = await isNominalHead(token, { tokens, index: i });
  console.log(`[${i}] "${token.text}" - POS: ${token.grammarInfo?.partOfSpeech} - isNominalHead: ${matches}`);
}

console.log('');
const matches = await matchSentence(sentence, grammarCatalog);
const noDesuMatches = matches.filter(m => m.grammarId === 'n4.no-desu');
console.log(`Found ${noDesuMatches.length} n4.no-desu match(es)`);

