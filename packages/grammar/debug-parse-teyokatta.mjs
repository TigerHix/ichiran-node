const testSentences = [
  '今日運動してよかった。',
  'このバスにまにあってよかったです。',
];

for (const sent of testSentences) {
  console.log('\n' + '='.repeat(60));
  console.log('Sentence:', sent);
  console.log('='.repeat(60));
  
  const { useSharedEngine } = await import('./dist/rules/bunpro/_test/engine.js');
  const engine = useSharedEngine([]).get();
  
  await new Promise(r => setTimeout(r, 100));
  
  const doc = await engine.analyze(sent);
  for (const t of doc.tokens) {
    const headText = t.head < doc.tokens.length ? doc.tokens[t.head].text : 'ROOT';
    console.log('  [' + t.i + '] ' + t.text);
    console.log('      pos=' + t.pos + ' tag=' + t.tag + ' lemma=' + t.lemma + ' inflectionForm=' + t.inflectionForm);
    console.log('      dep=' + t.dep + ' head=' + t.head + ' (' + headText + ')');
  }
}
