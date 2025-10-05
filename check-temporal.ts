import { segmentSentenceWithAlternatives } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const temporals = ['今日', '明日', '昨日', '今年', '去年'];

for (const word of temporals) {
  const alternatives = await segmentSentenceWithAlternatives(word);
  const token = alternatives[0].tokens[0];
  console.log(`${word}: ${token.grammarInfo?.partOfSpeech}`);
}
