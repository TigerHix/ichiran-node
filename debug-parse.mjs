import { GinzaClient } from './packages/grammar/src/ginza/client.js';

async function main() {
  const client = new GinzaClient();

  const sentences = [
    '待ち合わせの時間に遅れてすみません。',
    'ご飯を残してすみません。',
    'してすみませんでした。',
    'しなくてすみません。',
  ];

  for (const sent of sentences) {
    console.log('\n========================================');
    console.log('Sentence:', sent);
    console.log('========================================');
    const doc = await client.parse(sent);

    for (let i = 0; i < doc.tokens.length; i++) {
      const t = doc.tokens[i];
      const pos = t.pos || '-';
      const lemma = t.lemma || '-';
      const inf = t.inflectionForm || '-';
      console.log('[' + i + '] ' + String(t.text).padEnd(15) + ' POS=' + pos.padEnd(8) + ' lemma=' + lemma.padEnd(10) + ' inflectionForm=' + inf);
    }
  }

  await client.close();
}

main().catch(console.error);
