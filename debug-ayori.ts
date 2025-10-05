import { segmentSentenceWithAlternatives } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';
import { resolvePredicate } from './src/grammarMatcher/predicates.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const sentence = '日本語より英語のほうが単語は覚えやすいですか。';
const alts = await segmentSentenceWithAlternatives(sentence);
const tokens = alts[0].tokens;
console.log('Tokens:');
tokens.forEach((t, i) => console.log());

const isNominalHead = resolvePredicate('isNominalHead');
const isNounPhrase = resolvePredicate('isNounPhrase');
const isParticle = resolvePredicate('isParticle');

for (let i = 0; i < tokens.length; i++) {
  if (tokens[i].text === '単語') {
    const nh = await isNominalHead(tokens[i], { tokens, index: i });
    const np = await isNounPhrase(tokens[i], { tokens, index: i });
    const next = tokens[i + 1];
    const p = next ? await isParticle(next, { tokens, index: i + 1 }) : false;
    console.log();
    console.log();
  }
}
