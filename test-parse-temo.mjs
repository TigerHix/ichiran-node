import { describe } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]);

async function main() {
  const sentences = [
    '言っても言わなくても、彼はちゃんと仕事ができるよ。',
    '勉強してもしなくても、試験の結果は変わらない。',
    'あってもなくても、今食べておいた方がいい。',
  ];

  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===\n');
    const doc = await engine.get().analyze(sent);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main();
