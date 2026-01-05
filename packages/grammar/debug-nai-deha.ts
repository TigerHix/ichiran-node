import { analyze } from './src/ginza/client.js';

async function main() {
  const sentences = [
    'セール品を見ると、買わないではいられなくなる。',
    '私は辛い物を食べると、牛乳を飲まないではいられないの。',
    'ペットのワンちゃんが自分の尻尾を追いかけているのを見るとおかしくて、笑わないではいられない。',
    '大親友が事故でなくなったときは、泣かないではいられなかった。',
    'おばあさんが困っているのを見ると助けないではいられない。',
    '高級なお寿司が半額だ。食べないではいられない。',
    '引退試合に負けて、泣かないではいられません。',
    '困ったことがあって、神様に頼らないではいられない。',
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log('Sentence:', sentence);
    console.log('='.repeat(80));
    const doc = await analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

main().catch(console.error);
