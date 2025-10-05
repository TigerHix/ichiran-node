import { segmentSentenceWithAlternatives, matchSentence } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';
import { readFileSync } from 'fs';
import { join } from 'path';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const sentence = '私は「彼より強い」と冗談で言った。';
console.log('Testing:', sentence);
console.log('---\n');

const alternatives = await segmentSentenceWithAlternatives(sentence, 2);

for (let i = 0; i < alternatives.length; i++) {
  const alt = alternatives[i];
  console.log(`Alternative ${i + 1}:`);
  console.log('  Tokens:', alt.tokens.map(t => t.text).join(' | '));
}

const grammarPath = join(process.cwd(), 'src/grammarMatcher/grammars/n5/n5.a-wa-b-yori.json');
const grammarDef = JSON.parse(readFileSync(grammarPath, 'utf-8'));

const matches = await matchSentence(sentence, [grammarDef]);
console.log('\nMatches:', matches.length);

if (matches.length > 0) {
  for (const match of matches) {
    console.log('  ❌ MATCHED (should not match):');
    for (const capture of match.captures) {
      console.log(`    ${capture.label}: "${capture.tokens.map(t => t.text).join('')}"`);
    }
  }
}

