#!/usr/bin/env node
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.ts';

async function main() {
  const { get: getEngine } = useSharedEngine([]);
  const engine = getEngine();

  const sentences = [
    '風と共に去りぬ。',
    '心身と共に健康だ。',
    '厳しいと共に優しい。',
    '静かであると共に、空気が綺麗だ。',
    '風が強くなると共に雨が降ってきます。',
  ];

  for (const s of sentences) {
    console.log('\n=== ' + s + ' ===');
    const result = await engine.analyze(s);
    result.tokens.forEach((t, i) => {
      console.log(`${i}: ${t.text} (pos=${t.pos}, lemma=${t.lemma}, tag=${t.tag || 'N/A'})`);
    });
  }
}

main().catch(console.error);
