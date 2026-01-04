<<<<<<< HEAD
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './packages/grammar/src/rules/bunpro/jlpt3/index.js';

async function test() {
  const { get: engine } = useSharedEngine([BUNPRO_JLPT3]);
  const doc = await engine().analyze('面倒くさくても朝ご飯を食べることだ。');
  console.log('Tokens:');
  doc.tokens.forEach((t, i) => {
    console.log(`${i}: ${t.text} (lemma=${t.lemma}, pos=${t.pos}, inflection=${t.inflectionForm}, head=${t.head})`);
  });
}

test().catch(console.error);
=======
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
>>>>>>> jlpt3-verb-volitional-としたが
