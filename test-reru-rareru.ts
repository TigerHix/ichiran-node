import { GrammarEngine, BUNPRO_RULESETS } from './packages/grammar/src/index.js';

async function main() {
  const grammarEngine = await GrammarEngine.create(BUNPRO_RULESETS);

  const text = '昨日、友達と映画を見に行きました';
  console.log('Testing:', text);
  console.log();

  const matches = await grammarEngine.match(text);
  console.log(`Total matches: ${matches.length}`);
  console.log();

  for (const match of matches) {
    const summary = grammarEngine.getRuleSummary(match.ruleId);
    if (match.ruleId.includes('Verb') || match.ruleId.includes('passive') || match.ruleId.includes('potential')) {
      console.log('=== Match ===');
      console.log('Rule ID:', match.ruleId);
      console.log('Name:', summary?.name);
      console.log('Description:', summary?.description);
      console.log('Captures:', JSON.stringify(match.captures, null, 2));
      console.log();
    }
  }

  await grammarEngine.close();
}

main().catch(console.error);
