import { getSharedEngine } from './packages/grammar/dist/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT5 } from './packages/grammar/dist/rules/bunpro/jlpt5/index.js';

const engine = await getSharedEngine([BUNPRO_JLPT5]);

// Test sentences from の-noun-ommission
const sentences = [
  'この本はたけしさんのです。',
  'あの車、あなたが乗っているのですか。',
  'そのペンは誰の？あなたの？',
  '木綿のスカーフは私のです。',
  'これは僕のじゃないです。田中さんのです。'
];

for (const sent of sentences) {
  console.log('\n===', sent, '===');
  const doc = await engine.analyze(sent);
  const tokens = doc.tokens.map(t => ({
    text: t.text,
    pos: t.pos,
    lemma: t.lemma,
    dep: t.dep,
    head: t.head,
    inflectionForm: t.inflectionForm
  }));
  console.log(JSON.stringify(tokens, null, 2));
}

process.exit(0);
