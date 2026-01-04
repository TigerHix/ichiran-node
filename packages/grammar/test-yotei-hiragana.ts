import { GinzaClient } from './src/ginza/client.js';
import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const client = new GinzaClient();
  await client.start();
  const engine = await GrammarEngine.create([BUNPRO_JLPT4], { client });

  const sentences = [
    'いつ日本語を勉強し始めるよていですか。',
    'なるべく早く日本語を喋れるようになるよていだ。',
    '雨が止んだから、三時から試合が始まるよていです。',
    '会議は月曜日のよていです。',
    '休みは来週からのよていだ。',
  ];

  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===\n');
    const doc = await engine.analyze(sent);
    if (doc) {
      const yoteiTokens = doc.sentences[0].tokens.filter(t => t.text === 'よてい' || t.lemma === '予定' || t.lemma === 'よてい');
      console.log('よてい tokens:', JSON.stringify(yoteiTokens, null, 2));
    }
  }

  await client.close();
}

main().catch(console.error);
