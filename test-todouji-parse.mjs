import { analyze } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '代引きというのは、商品の受け渡しと同時に支払うシステムのことです。',
  'サイレンが鳴ったと同時に、犯人は逃げた。',
  'この機械は便利であると同時に危険である為、気をつけて使用してください。',
  'その人は博士であると同時に宇宙飛行士でもある。',
  '私の家は自宅と同時にオフィスでもある。',
  '野菜と同時に肉を鍋に入れてください。',
  'カギと同時に携帯をどこかに忘れてきた。',
  '目覚めると同時に着替えて出かけた。',
  '演奏が始まると同時に、彼は眠ってしまった。',
  '雨が降り出すと同時に、雷が鳴りだした。',
];

for (const sent of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sent);
  console.log('='.repeat(80));
  const doc = await analyze(sent);
  
  // Find tokens related to と同時に
  const relevant = doc.tokens.filter(t => 
    t.text.includes('と') || t.text.includes('同時') || t.lemma === 'どうじ' || t.lemma === 'と'
  );
  
  console.log('Relevant tokens:');
  for (const t of relevant) {
    const props = [
      `id=${t.id}`,
      `text="${t.text}"`,
      `lemma="${t.lemma}"`,
      `pos=${t.pos}`,
      `tag=${t.tag}`,
      `dep=${t.dep}`,
      `head=${t.head}`
    ].join(' ');
    console.log(`  ${props}`);
  }
}
