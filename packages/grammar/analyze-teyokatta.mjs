// Quick script to analyze how GiNZA parses てよかった patterns
const { GiNZA } = await import('./dist/ginza/client.js');
const client = new GiNZA();

const sentences = [
  '今日運動してよかった。',
  'このレストランにまたきてよかった。',
  'あの携帯を買わなくてよかった。',
  'このバスにまにあってよかったです。',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await client.parse(sent);
  for (const tok of doc.tokens) {
    console.log(`  ${tok.text} [${tok.pos}] lemma=${tok.lemma} inflection=${tok.inflectionForm} dep=${tok.dep} head=${tok.head}`);
  }
}

await client.close();
