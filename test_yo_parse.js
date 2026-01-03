import { describeRule } from './packages/grammar/src/rules/bunpro/_test/helpers.js';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  const e = engine.get();
  
  const sentences = [
    '今日は水曜日だよ。',
    'トムは足が早いよ。',
    'これはお風呂ですよ。',
    'それは熱いよ。',
    'あれは先生だよね。',
  ];
  
  for (const sentence of sentences) {
    console.log('\n=== ' + sentence + ' ===');
    const doc = await e.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main().catch(console.error);
