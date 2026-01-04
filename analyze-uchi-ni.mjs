#!/usr/bin/env bun
import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/dist/program.js';
import { GinzaClient } from '/home/tiger/ichiran-node/packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentences = [
  '近いうちに連絡します。',
  '熱いうちに食べて！',
  '赤ちゃんは食べているうちに寝てしまった。',
  '明るいうちに家に帰ろう。',
  '日本にいるうちに東京に行ってみたい。',
  'お爺ちゃんがまだ元気なうちに家族皆で旅行に行こう！',
  // Negatives - different usages of うち
  '私のうちには猫がいる。', // "my house" - locative noun
  '暗いうちは怖い。', // "dark inside" - spatial noun
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, head=${tok.head}`);
    }
  }
}

await client.stop();
