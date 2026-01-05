// Simple direct test
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT2 } from './src/rules/bunpro/jlpt2/index.js';

async function main() {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  const e = await engine.get();
  
  const sentence = '彼は長男です。したがって、次期社長はおそらく彼でしょう。';
  
  const hits = await e.match(sentence);
  console.log('Total hits:', hits.length);
  for (const hit of hits) {
    console.log('- Rule:', hit.ruleId);
  }
}

main().catch(console.error);
