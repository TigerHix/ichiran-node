#!/usr/bin/env bun
import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/dist/program.js';
import { GinzaClient } from '/home/tiger/ichiran-node/packages/grammar/dist/ginza/client.js';

const client = new GinzaClient({ python: 'python3' });
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentences = [
  'この建物をもっと丈夫にする方法がありますか？',  // na-adj + ni + suru
  '風呂を熱くするよ。いい？',  // i-adj + ku + suru
  '部屋を大きくしたいです。',  // i-adj + ku + suru
  '法律を新しくしてほしい。',  // i-adj + ku + shite
  '庭を元気にするアイデアはない？',  // na-adj + ni + suru
  'コーヒーを甘くするために、砂糖をください。',  // i-adj + ku + suru
  '汚い部屋を綺麗にするのは大変だ。',  // na-adj + ni + suru
  '歯医者さんで歯を白くしてもらった。',  // i-adj + ku + shite
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (doc && doc.sentences.length > 0) {
    for (const tok of doc.sentences[0].tokens) {
      console.log(`  ${tok.text}: lemma=${tok.lemma}, pos=${tok.pos}, tag=${tok.tag}, inflectionForm=${tok.inflectionForm}, dep=${tok.dep}, head=${tok.head}`);
    }
  }
}

await client.stop();
