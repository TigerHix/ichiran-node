import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';
import { buildSentenceIndex } from './packages/grammar/src/engine/index.js';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = '朝ごはんを作ったのは、お父さんじゃありませんでした。';
  const doc = await e.analyze(test);
  const sent = doc.sentences[0];

  const rs = e.program.rulesets[0];
  const rule56 = rs.rules[56];

  console.log('Rule 56:', rule56.id);

  // Call the match function
  console.log('\nCalling rule.match() with idx=null...');
  const matches1 = rule56.match(sent, test, null);
  console.log('Matches:', matches1.length);

  // Now try with an explicit index
  const idx = buildSentenceIndex(sent);
  console.log('\nCalling rule.match() with explicit index...');
  const matches2 = rule56.match(sent, test, idx);
  console.log('Matches:', matches2.length);

  if (matches2.length > 0) {
    console.log('First match captures:', JSON.stringify(matches2[0], null, 2));
  }

  // Also test rule 57, 58, 59
  for (const rIdx of [57, 58, 59]) {
    const rule = rs.rules[rIdx];
    const matches = rule.match(sent, test, idx);
    console.log(`\nRule ${rIdx} (${rule.id}): ${matches.length} matches`);
  }

  await e.close();
}

main();
