import { describeRule } from './src/rules/bunpro/_test/helpers.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

async function main() {
  const { default: engine } = await import('./dist/engine/compiler.js');
  await engine.initialize();
  
  const sentences = [
    'このバスはかくバス停に止まります。',
    'このエレベーターはかく階で止まります。',
    'この旅館には日本かく地からお客さまが来る。',
  ];
  
  for (const sent of sentences) {
    console.log('\n' + '='.repeat(80));
    console.log('Sentence:', sent);
    const doc = await engine.analyze(sent);
    if (!doc) {
      console.log('  NO DOC');
      continue;
    }
    console.log('  Tokens:');
    doc.tokens.forEach((t, i) => {
      console.log(`    ${i}: "${t.text}" lemma="${t.lemma}" pos="${t.pos}"`);
    });
    
    const hits = await engine.match(sent);
    const kakuHit = hits.find(h => h.ruleId === '各');
    console.log('  Rule "各" matched:', !!kakuHit);
  }
}

main().catch(console.error);
