import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './packages/grammar/src/rules/bunpro/jlpt3/index.js';

async function debug() {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  const sentences = [
    'この時間、医師は一人だけしかいません。',
    '彼は野菜だけしか食べられません。',
    '現金は今１０００円だけしかありません。',
  ];

  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    console.log(JSON.stringify(doc, null, 2));
  }
}

debug();
