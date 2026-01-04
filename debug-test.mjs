import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import { compileRuleset } from './packages/grammar/src/engine/compiler.js';
import rule from './packages/grammar/src/rules/bunpro/jlpt4/てくれてありがとう.js';

const client = new GinzaClient();

const sent = 'まっていてくれてありがとう';
console.log('=== ' + sent + ' ===');

const result = await client.analyze([sent]);
const doc = result["0"];

const ruleset = {
  id: 'test',
  rules: [rule],
};

const { match } = await compileRuleset(ruleset);
const matches = await match(doc);

console.log('Matches:', JSON.stringify(matches, null, 2));
