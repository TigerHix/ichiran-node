import { GrammarEngine } from '../src/program.js';
import rule from '../src/rules/bunpro/jlpt5/か-or.js';
import { BUNPRO_JLPT5 } from '../src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], {
    ginza: { python: 'python3' }
  });

  const sent = '彼が来るかどうか分かりません。';
  console.log('Sentence:', sent);

  const doc = await engine.analyze(sent);
  if (doc && doc.sentences[0]) {
    const tokens = doc.sentences[0].tokens;
    console.log('\nTokens:');
    tokens.forEach((t, i) => {
      if (t.text === 'か') {
        console.log('  ' + i + ': "' + t.text + '" (pos=' + t.pos + ', dep=' + t.dep + ')');
      }
    });
  }

  const hits = await engine.match(sent);
  const hit = hits.find((h) => h.ruleId === rule.id);
  if (hit) {
    console.log('\nMatch found!');
    console.log('  Start:', hit.captures.match.start);
    console.log('  End:', hit.captures.match.end);
    console.log('  Text:', hit.captures.match.text);
  }

  await engine.close();
}

main().catch(console.error);
