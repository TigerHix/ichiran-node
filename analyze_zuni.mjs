import { GiNZAEngine } from './packages/grammar/src/engine/index.js';

async function main() {
  const engine = new GiNZAEngine();
  
  const sentences = [
    '何も知らずにあんなこと言ってごめんなさい。',
    '朝ごはんを食べずに仕事に行きました。',
    '水を飲まずに運動をしていたから、頭が痛い。',
    '値段を見ずに買ったら大変なことになった。',
    '忘れずに届けてくれてありがとうございます。',
    '悩まずに生きるなんて不可能なのかもしれない。',
    '休まず、一日中ゲームをやり続けた。',
    '試合に一回も負けず、優勝した。',
    '肩の力を抜いて、力まずにスウィングした方がいいですよ。',
    'ラーメン屋でラーメンを注文せずにチャーハンだけ食べました。',
    '諦めずに続ければ、日本語もっと上手になりますよ。',
    '分からずに操作すると故障の原因になるのでやめてください。',
    '気を緩めず、次の試合も頑張ってください。',
    '勉強せずにテストを受けたから、テストの点数が低かった。',
    'あんまり無理をせずに頑張ってくださいね。'
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log(`Sentence: ${sentence}`);
    console.log('='.repeat(80));
    const doc = await engine.analyze(sentence);
    
    // Find tokens related to ず
    const zTokens = doc.tokens.filter(t => 
      t.text.includes('ず') || t.lemma?.includes('ず')
    );
    
    if (zTokens.length > 0) {
      console.log('Tokens with ず:');
      zTokens.forEach(t => {
        const tagInfo = t.tag ? ` tag=${t.tag}` : '';
        const depInfo = t.dep ? ` dep=${t.dep}` : '';
        const infInfo = t.inflectionForm ? ` inflectionForm=${t.inflectionForm}` : '';
        const conjInfo = t.conjugationClass ? ` conjugationClass=${t.conjugationClass}` : '';
        const headInfo = t.head !== undefined ? ` head=${t.head} -> "${doc.tokens[t.head]?.text}"` : '';
        console.log(`  - text="${t.text}" lemma="${t.lemma}" pos=${t.pos}${tagInfo}${depInfo}${infInfo}${conjInfo}${headInfo}`);
      });
    }
  }
  
  await engine.cleanup();
}

main().catch(console.error);
