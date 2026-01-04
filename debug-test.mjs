import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

async function debug() {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  const e = await engine.get();
  
  const sentences = [
    'この猫はまん丸でかわいい。',
    'まん中にある本をとってください。',
    '体がまっ赤になるくらい お風呂が熱かった。',
    '道が雪でまっ白になった。',
    '真っ直ぐに行ってください。',
  ];
  
  for (const sent of sentences) {
    console.log('\n===', sent, '===');
    const result = await e.analyze(sent);
    result.tokens.forEach(t => {
      console.log(`  text="${t.text}" pos=${t.pos}`);
    });
  }
}

debug().catch(console.error);
