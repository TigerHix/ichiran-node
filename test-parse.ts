import { getSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await getSharedEngine([BUNPRO_JLPT5]);

  const sentences = [
    '行かないでください。',
    '窓を閉めないでください。',
    'ここでサッカーをしないでください。',
  ];

  for (const sent of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log('ANALYZING: ' + sent);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sent);
    console.log(JSON.stringify(doc, null, 2));
  }

  await engine.close();
}

main().catch(console.error);
