import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5]);
  
  const tests = [
    { sentence: '今日は水曜日だよ。', shouldMatch: true },
    { sentence: 'トムは足が早いよ。', shouldMatch: true },
    { sentence: 'これはお風呂ですよ。', shouldMatch: true },
    { sentence: 'あれは先生だよね。', shouldMatch: true },
    { sentence: '四月よふみは誰ですか。', shouldMatch: false },
    { sentence: '良い天気です。', shouldMatch: false },
  ];
  
  for (const test of tests) {
    const hits = await engine.match(test.sentence);
    const yoHit = hits.find(h => h.ruleId === 'よ');
    const matched = !!yoHit;
    const status = matched === test.shouldMatch ? '✓' : '✗';
    console.log(`${status} "${test.sentence}" - Matched: ${matched}, Expected: ${test.shouldMatch}`);
  }
  
  await engine.close();
}

main().catch(console.error);
