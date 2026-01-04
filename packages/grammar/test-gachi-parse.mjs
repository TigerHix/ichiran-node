// Simple test to check GiNZA parsing for がち patterns
const { spawn } = require('child_process');

async function testParse(sentence) {
  return new Promise((resolve) => {
    const python = spawn('python3', ['-c', `
import ginza
import spacy
nlp = spacy.load('ja_ginza')
doc = nlp('${sentence}')
for token in doc:
    print(f"{token.i}:{token.text}|{token.pos_}|{token.lemma_}|{token.tag_}|{token.dep_}")
`], { cwd: '/tmp/jlpt3-15/packages/grammar' });
    
    let output = '';
    python.stdout.on('data', (data) => {
      output += data.toString();
    });
    python.on('close', () => resolve(output));
  });
}

async function main() {
  const sentences = [
    '彼はいつも遠慮がちだ。',
    '私の息子は病気がちだ。',
    '私は昔から遅刻しがちだ。',
    '車ばかり乗っていると、運動不足になりがちだ。',
    'さぼりがちな生徒を家まで迎えに行って、学校へ連れてきた。',
    'それは初心者にありがちのミスですね。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const result = await testParse(sent);
    console.log(result);
  }
}

main().catch(console.error);
