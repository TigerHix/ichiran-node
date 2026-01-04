import { GinzaClient } from './src/ginza/client.js';
import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const client = new GinzaClient();
  await client.start();
  const engine = await GrammarEngine.create([BUNPRO_JLPT4], { client });

  const sentences = [
    'いつ日本語を勉強し始める予定ですか。',
    'なるべく早く日本語を喋れるようになる予定だ。',
    '雨が止んだから、三時から試合が始まる予定です。',
    '休みは来週からの予定だ。',
    '会議は月曜日の予定です。',
    '学校で研究をする予定です。',
    '卒業してから、仕事を探す予定です。',
    '私は晩ご飯を作る予定だったが、彼がご飯を食べて帰って来た。',
    '今夜は先輩たちと飲みに行く予定だ。',
    '明日は友達とステーキを食べる予定だ。',
    '締め切りまでに家賃を全部払う予定だ。',
    'セミナーが来週の金曜日に開かれる予定だ。',
    '電気代が高くなるけど、エアコンを点けておく予定だ。',
    'あいつらが銀行のお金を盗む予定だったみたいです。',
  ];

  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===\n');
    const doc = await engine.analyze(sent);
    if (doc) {
      console.log(JSON.stringify(doc, null, 2));
    }
  }

  await client.close();
}

main().catch(console.error);
