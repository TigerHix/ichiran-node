import { describe, test, expect } from 'bun:test';
import { getSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

async function main() {
  const engine = await getSharedEngine([]);

  const sentences = [
    'お茶かなにかありませんか？',
    'ビールかなにかがほしいな。',
    'コーヒーかなにか飲みませんか？',
    '学校かなにかに通ってるみたいだけど。',
    '交通渋滞かなにかのような気がする。',
    'バールかなにかを使って、窓を開けたと思います。'
  ];

  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const result = await engine.analyze(sent);

    for (const token of result.tokens) {
      console.log(`  ${token.id}: ${token.text} | ${token.pos} | ${token.lemma || '-'} | head=${token.head} | dep=${token.dep || '-'}`);
    }
  }
}

main().catch(console.error);
