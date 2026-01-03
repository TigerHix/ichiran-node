import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';
import { explainMatch } from './packages/grammar/src/engine/compiler.js';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = '朝ごはんを作ったのは、お父さんじゃありませんでした。';
  const doc = await e.analyze(test);
  const sent = doc.sentences[0];

  const rs = e.program.rulesets[0];
  const rule56 = rs.rules[56];

  console.log('Testing explainMatch on rule 56...');
  const result = explainMatch(rule56, sent, test);
  console.log('Result:', JSON.stringify(result, null, 2));

  await e.close();
}

main();
