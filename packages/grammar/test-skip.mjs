import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/jlpt3/index.js';

async function main() {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  
  // Test the skipped sentence
  const sentence = '彼は強いだけではなく、とても優しい人だ。';
  
  console.log('Testing:', sentence);
  const doc = await engine.analyze(sentence);
  console.log('\nGiNZA parse:');
  console.log(JSON.stringify(doc.tokens.map(t => ({
    id: t.idx,
    text: t.text,
    lemma: t.lemma,
    pos: t.pos,
    dep: t.dep,
    head: t.head
  })), null, 2));
  
  const result = await engine.explainMatch(sentence, 'だけではない');
  console.log('\nMatch result:', result.matched);
  if (!result.matched) {
    console.log('Reason:', result.reason);
    console.log('Partial:', result.partialBinding);
  }
}

main().catch(console.error);
