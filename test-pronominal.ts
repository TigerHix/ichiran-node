import { matchSentence } from './src/grammarMatcher/runtime.js';
import { grammarCatalog } from './src/grammarMatcher/catalog.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

console.log('Testing pronominal の support:\n');

const testCases = [
  { sentence: '会社のか学校のかを決めます。', pattern: 'n5.noun-ka-noun-ka', desc: 'Pronominal の in noun-ka-noun-ka' },
  { sentence: '赤いのか青いのかを選びます。', pattern: 'n5.noun-ka-noun-ka', desc: 'Adjective + pronominal の' },
  { sentence: '私の自転車は弟のよりも軽い。', pattern: 'n5.a-wa-b-yori', desc: 'Pronominal の in a-wa-b-yori (already working)' },
];

for (const test of testCases) {
  const matches = await matchSentence(test.sentence, grammarCatalog);
  const patternMatches = matches.filter(m => m.grammarId === test.pattern);
  const result = patternMatches.length > 0 ? '✅' : '❌';
  
  console.log(`${result} ${test.desc}`);
  console.log(`   Sentence: ${test.sentence}`);
  console.log(`   Pattern: ${test.pattern}`);
  
  if (patternMatches.length > 0 && patternMatches[0].captures.length > 0) {
    console.log(`   Captures:`, patternMatches[0].captures.map((c: any) => c.tokens.map((t: any) => t.text).join('')));
  }
  console.log('');
}
