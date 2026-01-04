import { useSharedEngine } from './src/rules/bunpro/_test/engine.ts';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

async function main() {
  const { get } = useSharedEngine([BUNPRO_JLPT3]);
  const engine = get();
  
  const tests = [
    'ポケモンというゲームを知っている？',
    '佐藤浩一という人を知っていますか？',
    '先生という職業は、けっこう大変な仕事だ。',
  ];
  
  for (const sent of tests) {
    console.log('===', sent, '===');
    const result = await engine.analyze(sent);
    for (let i = 0; i < result.tokens.length; i++) {
      const t = result.tokens[i];
      console.log(`[${i}] ${t.text} (lemma=${t.lemma}, pos=${t.pos}, dep=${t.dep}, head=${t.head})`);
    }
    console.log('');
  }
}

main().catch(console.error);
