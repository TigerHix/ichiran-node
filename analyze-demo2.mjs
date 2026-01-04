import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sent = '彼女は仕事が大変でも諦めません。';
console.log('Sentence:', sent);

const doc = await engine.analyze(sent);
console.log('Type:', typeof doc);
console.log('Keys:', Object.keys(doc || {}));
console.log('Full:', JSON.stringify(doc, null, 2));
