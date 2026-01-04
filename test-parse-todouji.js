const { program } = require('./dist/grammar/program.js');

async function test() {
  const engine = await program.createEngine();
  
  const sentences = [
    '代引きというのは、商品の受け渡しとどうじに支払うシステムのことです。',
    '野菜とどうじに肉を鍋に入れてください。',
    'その人は博士であるとどうじに宇宙飛行士でもある。',
  ];

  for (const sent of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log('Sentence:', sent);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sent);
    
    // Find tokens related to とどうじに
    for (const t of doc.tokens) {
      if (t.text.includes('と') || t.text.includes('どうじ')) {
        console.log(`id=${t.id} text="${t.text}" lemma="${t.lemma}" pos=${t.pos} tag=${t.tag} dep=${t.dep} head=${t.head}`);
      }
    }
  }
  
  await engine.stop();
}

test().catch(console.error);
