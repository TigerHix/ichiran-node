import { getSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

async function main() {
  const engine = await getSharedEngine([]);
  const sentences = [
    '安いからといって買いすぎてしまい、買った食べ物を腐らせている。',
    '丈夫だからといって、雑に扱えば必ず壊れます。',
    '暑いからって、そんなに休憩ばかりしていたら仕事が進まないだろ。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    doc.tokens.forEach((t, i) => {
      console.log(`${i}: ${t.text} (${t.pos}) lemma=${t.lemma} inflectionForm=${t.inflectionForm}`);
    });
  }
}

main().catch(console.error);
