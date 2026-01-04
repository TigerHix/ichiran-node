import { Ginza } from './packages/grammar/src/ginza/index.js';

async function main() {
  const ginza = new Ginza();
  const sentences = [
    '文化祭の準備をしている生徒：「だいたいでいいから、午前中までにはおわらせておいて。',
    'お客さんと話しているテクニカルサポートスタッフ：「だいたいは、スマートフォンをリセットすればＯＫです。',
    '水曜日はだいたい５時ごろ帰ります。',
  ];

  for (const sentence of sentences) {
    console.log('\n=== ' + sentence.substring(0, 50));
    const doc = await ginza.parse(sentence);
    
    for (const token of doc.tokens) {
      if (token.text.includes('だいたい') || token.lemma?.includes('だいたい')) {
        console.log(`Token: text="${token.text}" lemma="${token.lemma}" pos="${token.pos}" tag="${token.tag}"`);
      }
    }
  }
}

main().catch(console.error);
