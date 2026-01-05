import { analyze } from './src/ginza/client.js';

async function main() {
  const sentences = [
    '困ったことがあって、神様にたよらないではいられない。',
    '競馬の馬の毛並みがあまりに美しい。さわらないではいられない。',
    'あまりにもひどい勘違いだから、口をださないではいられない。',
    '引退試合に負けて、なかないではいられません。',
    'セール品を見ると、買わないではいられなくなる。',
    '大親友が事故でなくなったときは、泣かないではいられなかった。',
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
