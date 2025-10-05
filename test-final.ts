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

const testCases = [
  { sentence: '会社か学校かを決めます。', shouldMatch: true, desc: 'Simple nouns' },
  { sentence: 'いい会社か悪い学校かを決めます。', shouldMatch: true, desc: 'Adjectives + nouns' },
  { sentence: '会社の代表番号か担当者の携帯かを把握しています。', shouldMatch: true, desc: 'Compound nouns with の' },
  { sentence: 'いい会社の代表番号か悪い担当者の携帯かを把握しています。', shouldMatch: true, desc: 'Adj + compound nouns' },
  { sentence: '今日か明日かを決めます。', shouldMatch: true, desc: 'Temporal expressions' },
];

console.log('Testing n5.noun-ka-noun-ka pattern:\n');

for (const test of testCases) {
  const matches = await matchSentence(test.sentence, grammarCatalog);
  const nounKaMatches = matches.filter(m => m.grammarId === 'n5.noun-ka-noun-ka');
  const matched = nounKaMatches.length > 0;
  const result = matched === test.shouldMatch ? '✅' : '❌';
  
  console.log(`${result} ${test.desc}`);
  console.log(`   Sentence: ${test.sentence}`);
  console.log(`   Expected: ${test.shouldMatch ? 'match' : 'no match'}, Got: ${matched ? 'match' : 'no match'}`);
  
  if (matched && nounKaMatches[0]) {
    const match = nounKaMatches[0];
    console.log(`   option1: ${match.captures[0]?.tokens.map(t => t.text).join('')}`);
    console.log(`   option2: ${match.captures[1]?.tokens.map(t => t.text).join('')}`);
  }
  console.log('');
}
