import { spawn } from 'node:child_process';
import { createInterface } from 'node:readline';
import { once } from 'node:events';

async function parseWithGinza(sentence) {
  const ginza = spawn('python3', ['-m', 'ginza']);
  const rl = createInterface({
    input: ginza.stdout,
    crlfDelay: Infinity,
  });

  ginza.stdin.write(sentence + '\n');

  const tokens = [];
  for await (const line of rl) {
    if (!line.trim()) break;
    // GiNZA output format: text\tlemma\tpos\ttag\tdep\thead\tstart\tend
    const parts = line.split('\t');
    if (parts.length >= 8) {
      tokens.push({
        text: parts[0],
        lemma: parts[1],
        pos: parts[2],
        tag: parts[3],
        dep: parts[4],
        head: parseInt(parts[5]),
        start: parseInt(parts[6]),
        end: parseInt(parts[7]),
      });
    }
  }

  ginza.kill();
  return tokens;
}

async function analyze(sentence) {
  try {
    const tokens = await parseWithGinza(sentence);
    console.log(`\n=== ${sentence} ===`);
    tokens.forEach(t => {
      console.log(`${t.text} [${t.pos}] lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
    });
  } catch (e) {
    console.error(`Error analyzing: ${sentence}`, e.message);
  }
}

const sentences = [
  '彼はここに来ないようだ。',
  'ほとんど無くなっているようだ。',
  '彼の話し方は歌っているようだ。',
  '今日の天気は台風のようだ。',
  'あそこのプールは深いようだ。',
  'ヨーダは宇宙人のようだ。',
  'このコーラは苦い。まるでコーヒーのようだ。',
  '彼はいつも変な事言っているけど、本当は真面目なようだ。',
  '私は誕生日にグーグルピクセルが欲しいけど、もらえないようだ。',
  '彼女は話しにくいようだが、話しやすい。',
  'あの人はお金を持っていないようだが、本当はお金持ち。',
  'すみません！私の荷物を勘違いして、持って行ったようだ。',
  'あの選手はこの選手より速く走れるようだ。',
  '子供は先週もらったゲームにもう飽きたようだ。',
];

(async () => {
  for (const sent of sentences) {
    await analyze(sent);
  }
})();
