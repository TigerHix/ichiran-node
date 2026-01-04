// Quick debug script to see GiNZA parses
import { spawn } from 'child_process';

const sentences = [
  'クライアントに電話を掛けようとしたが、夜遅かったので朝まで待つことにした。',
  '池で泳ごうとしたら、警察に止められた。',
  '逃げようとしたけど、捕まったら大変なことになるから逃げなかった。',
  '説得しようとしたが、失敗した。',
  'お弁当をたべようとしたが、箸が入っていなかった。',
  'でかけようとしたけれど、あまりにも天気が悪いのでやめました。',
];

async function testSentence(sentence) {
  console.log('\n=== ' + sentence + ' ===\n');
  
  const proc = spawn('bun', ['run', '-s', 'src/engine/ginza-cli.ts', sentence], {
    cwd: '/tmp/jlpt3-3/packages/grammar'
  });
  
  for await (const line of proc.stdout) {
    console.log(line.toString());
  }
  
  await new Promise(resolve => proc.on('close', resolve));
}

for (const s of sentences) {
  await testSentence(s);
}
