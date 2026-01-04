#!/usr/bin/env bun
import { GrammarEngine } from './packages/grammar/dist/program.js';
import { GinzaClient } from './packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentences = [
  '明日って晴れるんだっけ？',
  'もうすぐ梅雨も終わりだっけ？',
  'この資料渡したっけ？',
  'あの人誰だっけ。',
  '土曜日って暇だっけ。',
  'クミコって納豆食べれるんだっけ。',
  '日本の文化について発表をしたのは誰だったっけ？',
  '今日の朝は朝ご飯を食べたっけ。',
  'トムって日本語話せるっけ。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: lemma=${tok.lemma}, pos=${tok.pos}, dep=${tok.dep}, head=${tok.head}, tag=${tok.tag}, inf=${tok.inflectionForm}`);
    }
  }
}

await client.stop();
