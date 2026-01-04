import { GrammarEngine } from './packages/grammar/dist/program.js';
import { GinzaClient } from './packages/grammar/dist/ginza/client.js';
import { spawn } from 'child_process';

// Start Ginza server
const ginza = spawn('python3', ['-m', 'ginza.server'], {
  cwd: './packages/grammar/python',
  stdio: ['ignore', 'pipe', 'inherit']
});

await new Promise(resolve => {
  ginza.stdout.on('data', (data) => {
    if (data.toString().includes('Ginza server')) {
      resolve();
    }
  });
});

const client = new GinzaClient('http://127.0.0.1:8080');
const engine = GrammarEngine.create([], { client });

// Test sentences from の-noun-ommission
const sentences = [
  'この本はたけしさんのです。',
  'たけしさんの車は新しいです。',
  '日本の寿司は美味しい。'
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
    inflectionForm: t.inflectionForm,
    features: t.features
  }));
  console.log(JSON.stringify(tokens, null, 2));
}

ginza.kill();
process.exit(0);
