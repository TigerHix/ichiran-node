import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.ts';
import { buildSentenceIndex } from './packages/grammar/src/engine/index.js';

async function main() {
  const e = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' },
  });

  const test = '朝ごはんを作ったのは、お父さんじゃありませんでした。';
  const doc = await e.analyze(test);

  if (!doc || !doc.sentences[0]) {
    console.log('No doc parsed');
    await e.close();
    return;
  }

  const sent = doc.sentences[0];
  const sourceText = test;
  const idx = buildSentenceIndex(sent);

  // Check which rules are candidates
  const candidates = new Set();
  for (const tok of sent.tokens) {
    const keys = [`lemma:${tok.lemma}`, `text:${tok.text}`];
    for (const k of keys) {
      const pairs = e.program.dispatch.get(k);
      if (pairs) {
        for (const [rsIdx, rIdx] of pairs) {
          candidates.add(`${rsIdx}:${rIdx}`);
        }
      }
    }
  }

  console.log('Number of candidates:', candidates.size);

  // Try matching each candidate
  const rs = e.program.rulesets[0];

  // Print all candidates
  console.log('All candidates:');
  for (const key of Array.from(candidates).sort()) {
    const [rsIdxStr, rIdxStr] = key.split(':');
    const rsIdx = Number(rsIdxStr);
    const rIdx = Number(rIdxStr);
    const rule = rs.rules[rIdx];
    console.log(`  [${rsIdx}:${rIdx}] -> ${rule ? rule.id : 'MISSING'}`);
  }

  for (const key of Array.from(candidates)) {
    const [rsIdxStr, rIdxStr] = key.split(':');
    const rsIdx = Number(rsIdxStr);
    const rIdx = Number(rIdxStr);
    const rule = rs.rules[rIdx];

    if (!rule) continue;

    const capturesList = rule.match(sent, sourceText, idx);
    if (capturesList.length > 0) {
      console.log(`Candidate [${rsIdx}:${rIdx}] -> Rule ${rule.id}: matched with ${capturesList.length} captures`);
    }
  }

  await e.close();
}

main();
