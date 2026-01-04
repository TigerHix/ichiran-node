import { GrammarEngine } from './src/engine/compiler.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const engine = new GrammarEngine([BUNPRO_JLPT4]);

  const sentences = [
    'いつ日本語を勉強し始める予定ですか。',
    'なるべく早く日本語を喋れるようになる予定だ。',
    '雨が止んだから、三時から試合が始まる予定です。',
    '休みは来週からの予定だ。',
    '会議は月曜日の予定です。',
    '学校で研究をする予定です。',
    '卒業してから、仕事を探す予定です。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main().catch(console.error);
