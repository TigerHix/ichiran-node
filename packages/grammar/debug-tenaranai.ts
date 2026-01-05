import { beforeAll } from 'bun:test';
import { getSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT2 } from './src/rules/bunpro/jlpt2/index.js';

let engine;
beforeAll(async () => {
  engine = await getSharedEngine([BUNPRO_JLPT2]);
});

const test = async () => {
  const sentence = '昨日期日は徹夜して勉強してたから、眠くてならない。';
  const doc = await engine.analyze(sentence);
  
  console.log('\n=== Sentence:', sentence, '===\n');
  console.log('Tokens:');
  doc.tokens.forEach((tok, i) => {
    console.log(`${i}: text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" inflectionForm="${tok.inflectionForm || 'N/A'}"`);
  });
  
  console.log('\n\n=== Testing: 残念でならない ===\n');
  const doc2 = await engine.analyze('残念でならない');
  console.log('Tokens:');
  doc2.tokens.forEach((tok, i) => {
    console.log(`${i}: text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" inflectionForm="${tok.inflectionForm || 'N/A'}"`);
  });

  console.log('\n\n=== Testing: 暑くてならない ===\n');
  const doc3 = await engine.analyze('暑くてならない');
  console.log('Tokens:');
  doc3.tokens.forEach((tok, i) => {
    console.log(`${i}: text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" inflectionForm="${tok.inflectionForm || 'N/A'}"`);
  });

  console.log('\n\n=== Testing: 思い出されてならない ===\n');
  const doc4 = await engine.analyze('思い出されてならない');
  console.log('Tokens:');
  doc4.tokens.forEach((tok, i) => {
    console.log(`${i}: text="${tok.text}" lemma="${tok.lemma}" pos="${tok.pos}" inflectionForm="${tok.inflectionForm || 'N/A'}"`);
  });
};

test();
