import { GrammarEngine } from '../src/program.js';
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
      console.log('  ' + i + ': "' + t.text + '" (pos=' + t.pos + ')');
    });

    const kaIndex = tokens.findIndex(t => t.text === 'か' && t.pos === 'PART');
    const douIndex = tokens.findIndex(t => t.text === 'どう');
    console.log('\nFirst か (PART) at index: ' + kaIndex);
    console.log('どう at index: ' + douIndex);
    console.log('Distance: ' + (douIndex - kaIndex));
    console.log('Is distance <= 1? ' + (douIndex - kaIndex <= 1));
  }

  await engine.close();
}

main().catch(console.error);
