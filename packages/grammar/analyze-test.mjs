import { GrammarEngine } from './dist/program.js';
import { rulesets } from './dist/rules/bunpro/index.js';

async function main() {
  const engine = GrammarEngine.create(rulesets);
  
  const tests = [
    'ポケモンというゲームを知っている？',
    '佐藤浩一という人を知っていますか？',
    '今は梅雨という時期です。',
    '先生という職業は、けっこう大変な仕事だ。',
  ];
  
  for (const sent of tests) {
    console.log('===', sent, '===');
    const result = await engine.analyze(sent);
    const tokens = result.tokens.map(t => ({
      text: t.text,
      lemma: t.lemma,
      pos: t.pos,
      dep: t.dep,
      head: t.head
    }));
    console.log(JSON.stringify(tokens, null, 2));
    console.log('');
  }
}

main().catch(console.error);
