import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  
  const tests = ['しよう', '行こう', '帰ろう', '話そう', '待とう'];
  for (const test of tests) {
    const result = await engine.analyze(test);
    console.log(`Parse for "${test}":`);
    result.tokens.forEach(t => {
      console.log(`  text="${t.text}" lemma="${t.lemma}" pos=${t.pos} inflectionForm=${t.inflectionForm}`);
    });
  }
}

main().catch(console.error);
