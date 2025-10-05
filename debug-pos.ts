import { segmentSentenceWithAlternatives } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const sentence = 'なんでそんなに慌てているんだ？';
const alternatives = await segmentSentenceWithAlternatives(sentence);
const token = alternatives[0].tokens[2]; // 慌てている

console.log('Token:', token.text);
console.log('POS:', token.grammarInfo?.partOfSpeech);
console.log('Word info:', token.wordInfo);
console.log('');
console.log('Full grammar info:', JSON.stringify(token.grammarInfo, null, 2));
