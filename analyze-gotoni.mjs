#!/usr/bin/env node
import { GrammarEngine } from './packages/grammar/src/program.js';

const ruleset = {
  id: 'test',
  rules: [],
};

const engine = await GrammarEngine.create([ruleset], {
  ginza: { python: 'python3' },
});

const doc = await engine.analyze('あの人は会う人ごとに笑顔で握手します。');
console.log(JSON.stringify(doc, null, 2));

await engine.close();
