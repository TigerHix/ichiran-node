// Simple debug script to analyze parses
import { execSync } from 'child_process';

const sentences = [
  'これはアメリカからのお土産です。',
  '別れた後、彼との関係はどうなるんだろう。',
  '海外への手紙はこちらのポストにお入れください。',
  '環境に悪いので車での通勤は控えてください。',
  '出発までの時間、何をします？',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  try {
    const result = execSync(`docker exec ichiran-main-1 python3 -c "import ginza; import spacy; nlp = spacy.load('ja_ginza'); doc = nlp('${sent}'); [print(f'[{i}] {t.text} pos={t.pos_} dep={t.dep_} head={t.head.i} lemma={t.lemma_}') for i, t in enumerate(doc)]"`, { encoding: 'utf-8' });
    console.log(result);
  } catch (e) {
    console.log('Error:', e.message);
  }
}
